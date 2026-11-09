# This script provides functions to remove follow-up studies from a dataset, using the "Follow-ups tracker"

#' Function to read the follow-up tracker
#'
#' This function returns the "Follow-ups tracker" with standardised column types.
#' By default the tracker is read from the "Follow-ups tracker" workbook on the S:
#' drive; a data frame can be supplied to use a different version of the tracker.
#'
#' @param tracker data frame of the follow-up tracker to be used; default reads the "Follow-ups tracker" workbook from the S: drive
#' @param trace_dir folder into which a timestamped, byte-for-byte copy of the tracker workbook is saved for traceability; required when the tracker is read from the S: drive (i.e. `tracker` is NULL), ignored when a `tracker` data frame is supplied
#' @return data frame of the follow-up tracker, with the columns `id_study`, `year`, `fu`,
#'   `cohort_group_id`, `refresher_linking`, `returning_age_min` and `returning_age_max`
read_followups_tracker <- function(tracker = NULL, trace_dir = NULL) {

  if (is.null(tracker)) {
    # Fatal error: a folder is needed to keep a traceable copy of the tracker used
    if (is.null(trace_dir)) {
      stop("Supply `trace_dir`: a folder into which a timestamped copy of the tracker workbook is saved for traceability.")
    }

    # Read the "Follow-ups tracker" from the mapped drive, handling Mac and Windows roots
    if (.Platform$OS.type == "windows") {
      base <- "S:/Projects/HeightProject"
    } else {
      base <- "/Volumes/HeightProject"
    }
    tracker_path <- file.path(base, "Original dataset", "Data", "Surveys",
                              "__Follow-up studies", "Follow-ups tracker.xlsx")

    # Fatal error: the tracker workbook is not where it should be (mapped drive not connected / path wrong)
    if (!file.exists(tracker_path)) {
      stop(paste0("The tracker workbook was not found at ", tracker_path, ". Check the mapped drive is connected and the path is correct."))
    }

    # Keep a byte-for-byte copy of the exact workbook used, under a timestamped name so runs are never overwritten
    dir_ok <- dir.create(trace_dir, recursive = TRUE, showWarnings = FALSE) || dir.exists(trace_dir)
    if (!dir_ok) {
      stop(paste0("Could not create the traceability folder: ", trace_dir))
    }
    stem <- tools::file_path_sans_ext(basename(tracker_path))
    dest <- file.path(trace_dir, paste0(stem, "_", format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".xlsx"))

    # copy.mode = FALSE avoids copying POSIX permissions, which fails on SMB/network drives (e.g. mapped S:)
    copy_warn <- NULL
    copied <- withCallingHandlers(
      file.copy(tracker_path, dest, overwrite = FALSE, copy.mode = FALSE, copy.date = TRUE),
      warning = function(w) { copy_warn <<- conditionMessage(w); invokeRestart("muffleWarning") }
    )
    if (!copied) {
      stop(paste0("Could not save a traceability copy of the tracker workbook from ", tracker_path,
                  " to ", dest,
                  if (!is.null(copy_warn)) paste0(" (", copy_warn, ")") else "",
                  ". Check the file is not open in Excel and that you can write to the folder."))
    }
    print_it(paste("Traceability copy of the tracker saved to:", dest), "yellow")

    # Read as text and coerce below: readxl guesses a column's type from the first 1000 rows, so a
    # column blank for longer than that comes back as all NA
    tr <- suppressWarnings(readxl::read_excel(tracker_path, col_types = "text"))

    # Map the workbook's real headers to the names the rest of the code expects.
    # Only headers that are present are renamed; any other columns are left untouched.
    header_map <- c("Study ID"          = "id_study",
                    "Year"              = "year",
                    "FU"                = "fu",
                    "Cohort_group_id"   = "cohort_group_id",
                    "Refresher linking" = "refresher_linking",
                    "Returning age min" = "returning_age_min",
                    "Returning age max" = "returning_age_max")
    to_rename <- names(header_map)[names(header_map) %in% names(tr)]
    names(tr)[match(to_rename, names(tr))] <- header_map[to_rename]
  } else if (is.data.frame(tracker)) {
    tr <- tracker
  } else {
    # Fatal error: unusable `tracker` argument
    stop("`tracker` must be NULL or a data frame.")
  }

  # Fatal error: tracker does not hold the columns needed to identify follow-ups and link their returners
  required_cols <- c("id_study", "year", "fu", "cohort_group_id",
                     "refresher_linking", "returning_age_min", "returning_age_max")
  missing_cols <- setdiff(required_cols, names(tr))
  if (length(missing_cols) > 0) {
    print_it("CHECK - the following columns are missing from the follow-up tracker:", "br_red")
    print_it(missing_cols, indent = 2)
    stop("Update the \"Follow-ups tracker\" workbook so it holds these columns, or pass an up-to-date tracker to `tracker`.")
  }

  # Standardise column types
  tr$id_study           <- trimws(as.character(tr$id_study))
  tr$year               <- suppressWarnings(as.numeric(tr$year))
  tr$fu                 <- trimws(as.character(tr$fu))
  tr$cohort_group_id    <- trimws(as.character(tr$cohort_group_id))
  tr$refresher_linking  <- tolower(trimws(as.character(tr$refresher_linking)))
  tr$returning_age_min  <- suppressWarnings(as.numeric(tr$returning_age_min))
  tr$returning_age_max  <- suppressWarnings(as.numeric(tr$returning_age_max))

  # Blanks and NA mean the same thing, so settle on NA
  tr$fu[tr$fu == "" | tr$fu == "NA"] <- NA
  tr$cohort_group_id[tr$cohort_group_id == "" | tr$cohort_group_id == "NA"] <- NA
  tr$refresher_linking[tr$refresher_linking == "" | tr$refresher_linking == "na"] <- NA

  # Fatal error: a linking method we do not implement, which would otherwise be silently ignored
  known <- c("id", "age", "unlinkable")
  unknown <- setdiff(unique(tr$refresher_linking[!is.na(tr$refresher_linking)]), known)
  if (length(unknown) > 0) {
    print_it("CHECK - the follow-up tracker holds these unknown values in \"Refresher linking\":", "br_red")
    print_it(unknown, indent = 2)
    stop(paste0("\"Refresher linking\" must be one of ", paste(known, collapse = ", "), ", or blank."))
  }

  return(tr)
}

#' Function to remove follow-up studies from a dataset
#'
#' This function removes from a dataset everyone who is already counted in an earlier wave of their
#' own cohort, using the "Follow-ups tracker".
#'
#' The tracker categorises each study in the column `FU` as `"FU"` (pure follow-up),
#' `"FU_w_refreshers"` (follow-up with refreshers), `"nonFU"` (non-follow-up or baseline),
#' or leaves it blank when the study has not been categorised. Studies of one cohort share a
#' `Cohort_group_id`. The **first study of a cohort group** - the earliest wave of it present in
#' `data`, whatever its category - is always kept in full, because nobody in it is counted twice.
#'
#' A pure follow-up re-examines the earlier wave's participants and nobody else, so the whole study
#' is removed. A follow-up with refreshers also recruits people who have never taken part, so only
#' its returning participants are removed, and the tracker says how to find them in the column
#' `Refresher linking`:
#' - `"id"` - a returning participant keeps the id they held before, so the returners are the rows
#'   whose `id` appears in an earlier wave of the same cohort group. Needs an `id` column in `data`.
#' - `"age"` - the ids do not carry across waves, but the returners occupy an age range that the
#'   refreshers do not, given by `Returning age min` and `Returning age max`. The range is applied to
#'   the whole wave: every row inside it is a returner and every row outside it is not. A blank
#'   `Returning age max` means no upper bound. Needs an `age` column in `data`.
#' - `"unlinkable"` - the returners cannot be identified either way, so the whole study is removed.
#'
#' `Refresher linking` is only ever consulted for a follow-up with refreshers that is not the first
#' study of its cohort group; it is ignored, and may be left blank, everywhere else. Studies the
#' tracker does not categorise, and refresher waves it does not say how to link, are all kept and
#' printed out.
#'
#' @param data data frame of studies to be filtered: can be a single study or multiple studies
#' @param tracker data frame of the follow-up tracker to be used; default reads the "Follow-ups tracker" workbook from the S: drive
#' @param trace_dir folder into which a timestamped, byte-for-byte copy of the tracker workbook is saved for traceability; required when the tracker is read from the S: drive (i.e. `tracker` is NULL), ignored when a `tracker` data frame is supplied
#' @return data frame of `data` with the follow-up studies and the returning participants removed.
#'   What was removed, and what needs attention, is recorded in the `"followups"` attribute of the
#'   result: a list with the character vectors `pure_follow_ups`, `unlinkable_waves`,
#'   `uncategorised`, `not_in_tracker` and `no_linking_given`, and the named integer vector
#'   `returners_by_wave`.
#' @export
remove_followups <- function(data, tracker = NULL, trace_dir = NULL) {

  # Fatal error: no id_study column
  if (!"id_study" %in% names(data)) stop("File missing id_study")

  # Studies present in the incoming data
  data_ids <- unique(as.character(data$id_study))
  n_before <- length(data_ids)

  # Open the follow-up tracker and keep only the studies that are in the data
  tr <- read_followups_tracker(tracker, trace_dir)
  tr <- tr[tr$id_study %in% data_ids, , drop = FALSE]

  # First (earliest) study of each cohort group present in the data, keeping ties
  grouped <- tr[!is.na(tr$cohort_group_id) & tr$cohort_group_id != "", , drop = FALSE]
  min_year <- suppressWarnings(tapply(grouped$year, grouped$cohort_group_id, min, na.rm = TRUE))
  first_of_group <- grouped$id_study[which(grouped$year == min_year[grouped$cohort_group_id])]

  # Pure follow-ups to remove: "FU" and not the first study of their cohort group
  pure_follow_ups <- tr$id_study[which(tr$fu == "FU" & !tr$id_study %in% first_of_group)]
  data_out <- data[!as.character(data$id_study) %in% pure_follow_ups, , drop = FALSE]
  out_ids <- unique(as.character(data_out$id_study))

  print_it(paste("Studies before removing follow-ups:", n_before), "yellow")
  print_it(paste("Studies after removing pure follow-ups:", length(out_ids)), "yellow")

  # Follow-ups with refreshers that count someone twice: an earlier wave of their cohort group is
  # still here, so their returning participants have to go. The others are left alone entirely.
  counting_twice <- unique(tr$id_study[which(tr$fu == "FU_w_refreshers" &
                                               !tr$id_study %in% first_of_group &
                                               tr$id_study %in% out_ids)])
  linking <- tr$refresher_linking[match(counting_twice, tr$id_study)]

  # Refresher waves the tracker does not say how to link: kept, and reported
  no_linking_given <- counting_twice[is.na(linking)]
  if (length(no_linking_given) > 0) {
    print_it(paste0("CAUTION - ", length(no_linking_given), " follow-ups with refreshers count participants already counted in an earlier wave, but the tracker does not say how to link them: they were all kept for now, and \"Refresher linking\" should be filled in for each before proceeding"), "br_violet")
    print_it(no_linking_given, indent = 2)
  }

  # Refresher waves whose returning participants cannot be identified: the whole wave goes
  unlinkable_waves <- counting_twice[which(linking == "unlinkable")]
  if (length(unlinkable_waves) > 0) {
    data_out <- data_out[!as.character(data_out$id_study) %in% unlinkable_waves, , drop = FALSE]
    print_it(paste0("Studies after removing the ", length(unlinkable_waves), " refresher waves whose returning participants cannot be identified: ",
                    length(unique(as.character(data_out$id_study)))), "yellow")
    print_it(unlinkable_waves, indent = 2)
  }

  # The returning participants of the rest. Every wave is flagged against the data as it stands
  # before any returner is dropped, so a participant returning to the third wave of a cohort is
  # still found in the second even where that row is itself about to be dropped.
  to_link <- setdiff(counting_twice, c(unlinkable_waves, no_linking_given))
  study_of_row <- as.character(data_out$id_study)
  returning <- rep(FALSE, nrow(data_out))

  for (wave in to_link) {
    row <- match(wave, tr$id_study)
    in_wave <- study_of_row == wave

    if (tr$refresher_linking[row] == "id") {
      # Fatal error: asked to link by id with no id to link on
      if (!"id" %in% names(data)) {
        stop(paste0("`data` has no `id` column, so the returning participants of ", wave,
                    " cannot be found by id. Supply `id`, or set \"Refresher linking\" for ", wave,
                    " to \"age\" or \"unlinkable\" in the follow-up tracker."))
      }
      earlier_waves <- tr$id_study[which(tr$cohort_group_id == tr$cohort_group_id[row] &
                                           tr$year < tr$year[row])]
      earlier_ids <- data_out$id[study_of_row %in% earlier_waves & !is.na(data_out$id)]
      returning[in_wave] <- !is.na(data_out$id[in_wave]) & data_out$id[in_wave] %in% earlier_ids

    } else {
      # Fatal error: asked to link by age with no age to link on
      if (!"age" %in% names(data)) {
        stop(paste0("`data` has no `age` column, so the returning participants of ", wave,
                    " cannot be found by age. Supply `age`, or set \"Refresher linking\" for ", wave,
                    " to \"id\" or \"unlinkable\" in the follow-up tracker."))
      }
      lower <- tr$returning_age_min[row]
      upper <- tr$returning_age_max[row]
      # Fatal error: an age range that says nothing, which would silently remove nobody or everybody
      if (is.na(lower)) {
        stop(paste0("\"Refresher linking\" is \"age\" for ", wave,
                    " but \"Returning age min\" is blank in the follow-up tracker. Give the age from which participants are returning."))
      }
      if (is.na(upper)) upper <- Inf
      age_of_row <- floor(data_out$age[in_wave])
      returning[in_wave] <- !is.na(age_of_row) & age_of_row >= lower & age_of_row <= upper
    }
  }

  returners_by_wave <- vapply(to_link, function(wave) sum(returning & study_of_row == wave), 0L)
  if (length(to_link) > 0) {
    data_out <- data_out[!returning, , drop = FALSE]
    print_it(paste0("Returning participants removed from ", length(to_link), " refresher waves: ", sum(returning)), "yellow")
    for (wave in to_link) {
      print_it(paste0(wave, " (", tr$refresher_linking[match(wave, tr$id_study)], "): ", returners_by_wave[[wave]]), indent = 2)
    }
  }

  # Studies the tracker does not categorise: all kept, reported in two groups
  # Case A: present in the follow-up tracker but with no follow-up category
  in_tracker_uncat <- unique(tr$id_study[is.na(tr$fu)])
  if (length(in_tracker_uncat) > 0) {
    print_it(paste0("CAUTION - ", length(in_tracker_uncat), " studies are in the follow-up tracker but not categorised: they were all kept for now, but these should all be assessed and marked in the tracker before proceeding"), "br_violet")
    print_it(in_tracker_uncat[1:min(20, length(in_tracker_uncat))], indent = 2)
    if (length(in_tracker_uncat) > 20) print_it(paste0("... and ", length(in_tracker_uncat) - 20, " more"), indent = 2)
  }

  # Case B: not present in the follow-up tracker at all
  not_in_tracker <- setdiff(data_ids, tr$id_study)
  if (length(not_in_tracker) > 0) {
    print_it(paste0("CAUTION - ", length(not_in_tracker), " studies in the data are not in the follow-up tracker at all: they were all kept for now, but these should all be assessed and marked in the tracker before proceeding"), "br_violet")
    print_it(not_in_tracker[1:min(20, length(not_in_tracker))], indent = 2)
    if (length(not_in_tracker) > 20) print_it(paste0("... and ", length(not_in_tracker) - 20, " more"), indent = 2)
  }

  print_it(paste("Studies kept:", length(unique(as.character(data_out$id_study)))), "yellow")
  print_it(paste("Participants kept:", nrow(data_out)), "yellow")
  print_it("DONE", "yellow")

  # What was removed, and what still needs a decision, so the caller can log it without redoing the work
  attr(data_out, "followups") <- list(pure_follow_ups   = pure_follow_ups,
                                      unlinkable_waves  = unlinkable_waves,
                                      returners_by_wave = returners_by_wave,
                                      no_linking_given  = no_linking_given,
                                      uncategorised     = in_tracker_uncat,
                                      not_in_tracker    = not_in_tracker)

  return(data_out)
}
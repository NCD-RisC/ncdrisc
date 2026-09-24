# This script provides functions to remove follow-up studies from a dataset, using the "Follow-ups tracker"

#' Function to read the follow-up tracker
#'
#' This function returns the "Follow-ups tracker" with standardised column types.
#' By default the tracker is read from the "Follow-ups tracker" workbook on the S:
#' drive; a data frame can be supplied to use a different version of the tracker.
#'
#' @param tracker data frame of the follow-up tracker to be used; default reads the "Follow-ups tracker" workbook from the S: drive
#' @param trace_dir folder into which a timestamped, byte-for-byte copy of the tracker workbook is saved for traceability; required when the tracker is read from the S: drive (i.e. `tracker` is NULL), ignored when a `tracker` data frame is supplied
#' @return data frame of the follow-up tracker, with the columns `id_study`, `year`, `fu` and `cohort_group_id`
read_followups_tracker <- function(tracker = NULL, trace_dir = NULL) {

  if (is.null(tracker)) {
    # Fatal error: a folder is needed to keep a traceable copy of the tracker used
    if (is.null(trace_dir)) {
      stop("Supply `trace_dir`: a folder into which a timestamped copy of the tracker workbook is saved for traceability.")
    }

    # Read the "Follow-ups tracker" from the mapped drive, handling Mac and Windows roots
    if (.Platform$OS.type == "windows") {
      base <- "S:/HeightProject"
    } else {
      base <- "/Volumes/HeightProject"
    }
    tracker_path <- file.path(base, "Original dataset", "Data", "Surveys",
                              "__Follow-up studies", "Follow-ups tracker.xlsx")

    # Keep a byte-for-byte copy of the exact workbook used, under a timestamped name so runs are never overwritten
    dir.create(trace_dir, recursive = TRUE, showWarnings = FALSE)
    stem <- tools::file_path_sans_ext(basename(tracker_path))
    dest <- file.path(trace_dir, paste0(stem, "_", format(Sys.time(), "%Y-%m-%d_%H%M%S"), ".xlsx"))
    copied <- file.copy(tracker_path, dest, overwrite = FALSE)
    if (!copied) {
      stop(paste0("Could not save a traceability copy of the tracker workbook from ", tracker_path, " to ", dest))
    }
    print_it(paste("Traceability copy of the tracker saved to:", dest), "yellow")

    tr <- suppressWarnings(readxl::read_excel(tracker_path))
  } else if (is.data.frame(tracker)) {
    tr <- tracker
  } else {
    # Fatal error: unusable `tracker` argument
    stop("`tracker` must be NULL or a data frame.")
  }

  # Fatal error: tracker does not hold the columns needed to identify follow-ups
  required_cols <- c("id_study", "year", "fu", "cohort_group_id")
  missing_cols <- setdiff(required_cols, names(tr))
  if (length(missing_cols) > 0) {
    print_it("CHECK - the following columns are missing from the follow-up tracker:", "br_red")
    print_it(missing_cols, indent = 2)
    stop("Update the \"Follow-ups tracker\" workbook so it holds these columns, or pass an up-to-date tracker to `tracker`.")
  }

  # Standardise column types
  tr$id_study        <- trimws(as.character(tr$id_study))
  tr$year            <- suppressWarnings(as.numeric(tr$year))
  tr$fu              <- trimws(as.character(tr$fu))
  tr$cohort_group_id <- trimws(as.character(tr$cohort_group_id))

  return(tr)
}

#' Function to remove follow-up studies from a dataset
#'
#' This function removes follow-up studies from a dataset, using the "Follow-ups tracker".
#' The tracker categorises each study in the column `FU` as `"FU"` (pure follow-up),
#' `"FU_w_refreshers"` (follow-up with refreshers), `"nonFU"` (non-follow-up or baseline),
#' or leaves it blank when the study has not been categorised. The function:
#' - removes the pure follow-ups, except those that are the first study (earliest wave held,
#'   including non-follow-ups) of their cohort group among the studies present in `data`;
#' - keeps the follow-ups with refreshers, but prints them out so they can be dealt with
#'   individually - again except those that are the first study of their cohort group;
#' - prints out the studies in `data` that the tracker does not categorise.
#'
#' @param data data frame of studies to be filtered: can be a single study or multiple studies
#' @param tracker data frame of the follow-up tracker to be used; default reads the "Follow-ups tracker" workbook from the S: drive
#' @param trace_dir folder into which a timestamped, byte-for-byte copy of the tracker workbook is saved for traceability; required when the tracker is read from the S: drive (i.e. `tracker` is NULL), ignored when a `tracker` data frame is supplied
#' @return data frame of `data` with the pure follow-up studies removed
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
  to_remove <- tr$id_study[which(tr$fu == "FU" & !tr$id_study %in% first_of_group)]
  data_out <- data[!as.character(data$id_study) %in% to_remove, , drop = FALSE]
  out_ids <- unique(as.character(data_out$id_study))

  print_it(paste("Studies before removing follow-ups:", n_before), "yellow")
  print_it(paste("Studies after removing pure follow-ups:", length(out_ids)), "yellow")

  # Follow-ups with refreshers: kept in the data, to be dealt with individually
  fu_with_ref <- unique(tr$id_study[which(tr$fu == "FU_w_refreshers" &
                                            !tr$id_study %in% first_of_group &
                                            tr$id_study %in% out_ids)])
  if (length(fu_with_ref) > 0) {
    print_it(paste0("CAUTION - ", length(fu_with_ref), " follow-ups with refreshers remain in the data and need to be dealt with individually:"), "br_violet")
    print_it(fu_with_ref, indent = 2)
  }

  # Studies the tracker does not categorise: all kept, reported in two groups
  # Case A: present in the follow-up tracker but with a blank/NA follow-up category
  in_tracker_uncat <- unique(tr$id_study[which(is.na(tr$fu) | tr$fu == "")])
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

  print_it("DONE", "yellow")

  return(data_out)
}
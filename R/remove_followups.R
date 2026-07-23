# This script provides functions to remove follow-up studies from a dataset, using the "Follow-ups tracker"

#' Function to read the follow-up tracker
#'
#' This function returns the "Follow-ups tracker" with standardised column types.
#' By default the version packaged with `ncdrisc` is used; a data frame can be supplied
#' to use a more recent version of the tracker.
#'
#' @param tracker data frame of the follow-up tracker to be used; default is the tracker packaged with `ncdrisc`
#' @return data frame of the follow-up tracker, with the columns `id_study`, `year`, `fu` and `cohort_group_id`
read_followups_tracker <- function(tracker = NULL) {

  if (is.null(tracker)) {
    tr <- suppressWarnings(ncdrisc::followups_tracker)
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
    stop("Update `data-raw/followups-tracker.csv` from the latest tracker and rerun `data-raw/format-data.R`, or pass an up-to-date tracker to `tracker`.")
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
#' @param tracker data frame of the follow-up tracker to be used; default is the tracker packaged with `ncdrisc`
#' @return data frame of `data` with the pure follow-up studies removed
#' @export
remove_followups <- function(data, tracker = NULL) {

  # Fatal error: no id_study column
  if (!"id_study" %in% names(data)) stop("File missing id_study")

  # Studies present in the incoming data
  data_ids <- unique(as.character(data$id_study))
  n_before <- length(data_ids)

  # Open the follow-up tracker and keep only the studies that are in the data
  tr <- read_followups_tracker(tracker)
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

  # Studies that the tracker does not categorise: all kept
  uncategorised <- setdiff(data_ids, tr$id_study[which(!is.na(tr$fu) & tr$fu != "")])
  if (length(uncategorised) > 0) {
    print_it(paste0("CAUTION - ", length(uncategorised), " studies in the data are not categorised in the follow-up tracker: they were all kept"), "br_violet")
    print_it(uncategorised[1:min(20, length(uncategorised))], indent = 2)
    if (length(uncategorised) > 20) print_it(paste0("... and ", length(uncategorised) - 20, " more"), indent = 2)
  }

  print_it("DONE", "yellow")

  return(data_out)
}
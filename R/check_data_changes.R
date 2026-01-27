# This script provides functions to compare new extraction data against previously extracted survey data to identify changes and differences

#' Function to read previously extracted dataframe
#'
#' This function reads extracted data from the specified file path.
#'
#' @param filename name of file to be read without suffix
#' @param target_dir location of study folder for reading extracted files; default is the current study folder
#' @return data frame of previously extracted data, or NULL if not found
read_extracted_df <- function(filename, target_dir = NULL) {

  # Specify study folder - only used when reading from study folder
  if (is.null(target_dir)) {
    target_dir <- paste0(getwd(), "/")
  }

  # Read CSV file
  csv_path <- paste0(target_dir, filename, ".csv")

  # Check if CSV file exists
  if (file.exists(csv_path)) {
    tryCatch({
      df_latin1 <- read.csv(csv_path, fileEncoding = "latin1")
      df_utf8 <- read.csv(csv_path, fileEncoding = "UTF-8")

      if (nrow(df_latin1) != nrow(df_utf8)) {
        print_it("(read as UTF-8)", "yellow")
        return(df_utf8)
      } else {
        print_it("(read as latin1)", "yellow")
        return(df_latin1)
      }
    }, error = function(e) {
      return(NULL)
    })
  }

  # If file doesn't exist or fails to load
  return(NULL)
}

#' Function to compare two dataframes for differences
#'
#' This function compares the data being extracted and previously extracted data.
#' It compares row counts, column names, matches rows, and performs detailed value-by-value comparison.
#'
#' @param new_data data frame of the data being extracted
#' @param comparison_data data frame of previously extracted data
compare_dataframes <- function(new_data, comparison_data) {

  any_change_found <- FALSE
  matching_options <- list(
    id = c("id"),
    `sample weight, age, sex, and height` = c("sex", "age", "psu", "samplewt_anthro", "height", "height1", "height2", "height3")
  )

  match_rows <- function(new_df, old_df, by_columns, type, new_total, old_total, fail_threshold) {

    merged <- merge(new_df, old_df, by = by_columns)

    if (nrow(merged) == 0) {
      print_it(paste("No matching possible based on", type), "br_red")
      return(NULL)
    }

    new_unmatched <- new_total - length(unique(merged$.new_idx))
    old_unmatched <- old_total - length(unique(merged$.old_idx))

    if (new_unmatched > 0) {
      print_it(paste("CAUTION -", new_unmatched, "rows in new extraction data could not be matched in previously extracted data based on", type), "br_violet")
    }
    if (old_unmatched > 0) {
      print_it(paste("CAUTION -", old_unmatched, "rows in previously extracted data could not be matched in new extraction data based on", type), "br_violet")
    }

    pct_new_lost <- new_unmatched / new_total
    pct_old_lost <- old_unmatched / old_total

    if (pct_new_lost >= fail_threshold && pct_old_lost >= fail_threshold) {
      print_it(paste("No matching possible based on", type), "br_red")
      return(NULL)
    }

    print_it(paste("Matched", nrow(merged), "rows based on", type), "yellow")

    return(list(
      new_idx = merged$.new_idx,
      old_idx = merged$.old_idx,
      matched_on = type,
      match_columns = by_columns,
      any_change_found = new_unmatched > 0 || old_unmatched > 0
    ))
  }

  # --- Inner function: compare_values ---
  # Performs value-by-value comparison on matched rows, excluding the columns used for matching.
  compare_values <- function(new_data_matched, old_data_matched, matched_on, match_columns, numeric_var_list) {

    values_changed <- FALSE

    print_it(paste("Comparing values of columns other than", matched_on, "after matching based on", matched_on), "yellow")

    common_columns <- setdiff(intersect(colnames(new_data_matched), colnames(old_data_matched)), match_columns)

    for (column_name in common_columns) {
      new_column_data <- new_data_matched[[column_name]]
      old_column_data <- old_data_matched[[column_name]]

      # Standardize data types for this column based on std_names_list
      if (column_name %in% numeric_var_list) {
        new_column_data <- as.numeric(new_column_data)
        old_column_data <- as.numeric(old_column_data)
      } else {
        if (!is.character(new_column_data)) {
          new_column_data <- as.character(new_column_data)
        }
        if (!is.character(old_column_data)) {
          old_column_data <- as.character(old_column_data)
        }
      }

      # Count changes from or to NA
      na_to_nonNA_indices <- which(!is.na(new_column_data) & is.na(old_column_data))
      if (length(na_to_nonNA_indices) > 0) {
        values_changed <- TRUE
        print_it(paste("CAUTION -", length(na_to_nonNA_indices), "NA values changed to non-NA in column", column_name), "br_violet")
        new_values_from_na <- new_column_data[na_to_nonNA_indices]
        unique_new_values <- unique(new_values_from_na)
        for (value in unique_new_values[1:min(5, length(unique_new_values))]) {
          value_count <- sum(new_values_from_na == value, na.rm = TRUE)
          print_it(paste("NA ->", value, paste0("(", value_count), "cases)"), indent = 2)
        }
      }
      nonNA_to_na_indices <- which(is.na(new_column_data) & !is.na(old_column_data))
      if (length(nonNA_to_na_indices) > 0) {
        values_changed <- TRUE
        print_it(paste("CAUTION -", length(nonNA_to_na_indices), "non-NA values changed to NA in column", column_name), "br_violet")
        old_values_to_na <- old_column_data[nonNA_to_na_indices]
        unique_old_values <- unique(old_values_to_na)
        for (value in unique_old_values[1:min(5, length(unique_old_values))]) {
          value_count <- sum(old_values_to_na == value, na.rm = TRUE)
          print_it(paste(value, "-> NA", paste0("(", value_count), "cases)"), indent = 2)
        }
      }

      both_not_na <- !is.na(new_column_data) & !is.na(old_column_data)
      if (sum(both_not_na) > 0) {

        # Check type changes
        new_is_numeric <- is.numeric(new_column_data)
        old_is_numeric <- is.numeric(old_column_data)

        if (new_is_numeric && !old_is_numeric) {
          values_changed <- TRUE
          stop(paste("ERROR - Column", column_name, "has incompatible data types: new extraction data is numeric but previously extracted data is not"))
        } else if (!new_is_numeric && old_is_numeric) {
          values_changed <- TRUE
          stop(paste("ERROR - Column", column_name, "has incompatible data types: new extraction data is not numeric but previously extracted data is numeric"))
        }

        # Count value differences (excluding changes from or to NA)
        value_diff_indices <- c()

        if (new_is_numeric && old_is_numeric) {
          # Both numeric: precision tolerance
          value_diff_indices <- which(both_not_na & abs(new_column_data - old_column_data) > 1e-6)
        } else if (!new_is_numeric && !old_is_numeric) {
          # Both non-numeric: exact comparison
          value_diff_indices <- which(both_not_na & new_column_data != old_column_data)
        }

        if (length(value_diff_indices) > 0) {
          values_changed <- TRUE
          print_it(paste("CAUTION -", length(value_diff_indices), "values changed in", column_name), "br_violet")

          # Show unique cases and their counts
          old_values <- old_column_data[value_diff_indices]
          new_values <- new_column_data[value_diff_indices]
          unique_changes <- unique(paste(old_values, "->", new_values))
          for (change in unique_changes[1:min(5, length(unique_changes))]) {
            change_count <- sum(paste(old_values, "->", new_values) == change)
            print_it(paste(change, paste0("(", change_count), "cases)"), indent = 2)
          }
        }
      }
    }

    return(values_changed)
  }

  # --- Main logic ---

  # Convert character columns to latin1 for consistent comparison
  convert_to_latin1 <- function(df) {
    for (col in names(df)) {
      if (is.character(df[[col]])) {
        df[[col]] <- iconv(df[[col]], from = "UTF-8", to = "latin1", sub = "")
      }
    }
    return(df)
  }
  new_data <- convert_to_latin1(new_data)
  comparison_data <- convert_to_latin1(comparison_data)

  # Store original comparison_data for debugging
  fixed_data <<- comparison_data

  # Get numerical variable names
  numeric_var_list <- as.character(std_names_list$Name[which(std_names_list$Type == "numeric")])

  # Remove fully empty columns from both datasets
  new_data_non_empty_cols <- sapply(new_data, function(col) !all(is.na(col)))
  comparison_non_empty_cols <- sapply(comparison_data, function(col) !all(is.na(col)))

  new_data <- new_data[, new_data_non_empty_cols, drop = FALSE]
  comparison_data <- comparison_data[, comparison_non_empty_cols, drop = FALSE]

  # Remove user and ncdrisc_version columns from old data
  # These are written when calling save_extraction()
  comparison_data <- comparison_data[, !colnames(comparison_data) %in% c("user", "ncdrisc_version"), drop = FALSE]

  # Compare row counts
  new_total <- nrow(new_data)
  old_total <- nrow(comparison_data)

  if (new_total != old_total) {
    any_change_found <- TRUE
    print_it("CAUTION - inconsistent number of rows:", "br_violet")
    print_it(paste("New:", new_total, "vs Old:", old_total), indent = 2)
  }

  # Identify columns that are new or have been removed
  new_file_columns <- colnames(new_data)
  old_file_columns <- colnames(comparison_data)

  new_columns <- setdiff(new_file_columns, old_file_columns)
  removed_columns <- setdiff(old_file_columns, new_file_columns)

  if (length(new_columns) > 0) {
    any_change_found <- TRUE
    print_it("CAUTION - added columns:", "br_violet")
    print_it(new_columns, indent = 2)

    # Save added columns dataframe to workspace to then run summary(added_columns) from the main script
    added_columns <<- new_data[, new_columns, drop = FALSE]
  }

  if (length(removed_columns) > 0) {
    any_change_found <- TRUE
    print_it("CAUTION - removed columns:", "br_violet")
    print_it(removed_columns, indent = 2)
  }

  # Match rows: try id first
  new_id_df <- data.frame(id = new_data[["id"]], .new_idx = seq_len(new_total))
  old_id_df <- data.frame(id = comparison_data[["id"]], .old_idx = seq_len(old_total))

  match_result <- match_rows(new_id_df, old_id_df, matching_options[["id"]], "id", new_total, old_total, fail_threshold = 0.1)

  # If id matching failed, try matching based on sample weight, age, sex, and height
  if (is.null(match_result)) {
    fallback_type <- names(matching_options)[2]
    print_it(paste("Trying to match based on", fallback_type), "yellow")

    match_columns <- intersect(matching_options[[fallback_type]], intersect(colnames(new_data), colnames(comparison_data)))

    if (length(match_columns) == 0) {
      print_it(paste("No matching possible based on", fallback_type), "br_red")
      return(any_change_found)
    }

    round_column <- function(df, col) {
      if (is.numeric(df[[col]])) {
        if (col %in% c("height", "height1", "height2", "height3")) {
          return(round(df[[col]], 0))
        } else {
          return(round(df[[col]]))
        }
      }
      return(df[[col]])
    }

    new_match_df <- data.frame(lapply(match_columns, function(k) round_column(new_data, k)))
    old_match_df <- data.frame(lapply(match_columns, function(k) round_column(comparison_data, k)))
    colnames(new_match_df) <- match_columns
    colnames(old_match_df) <- match_columns
    new_match_df$.new_idx <- seq_len(new_total)
    old_match_df$.old_idx <- seq_len(old_total)

    match_result <- match_rows(new_match_df, old_match_df, match_columns, fallback_type, new_total, old_total, fail_threshold = 0.1)

    if (is.null(match_result)) {
      return(any_change_found)
    }
  }

  if (match_result$any_change_found) {
    any_change_found <- TRUE
  }

  # Subset to matched rows and compare values
  new_data_matched <- new_data[match_result$new_idx, ]
  old_data_matched <- comparison_data[match_result$old_idx, ]

  if (compare_values(new_data_matched, old_data_matched, match_result$matched_on, match_result$match_columns, numeric_var_list)) {
    any_change_found <- TRUE
  }

  return(any_change_found)
}


#' Function to check for changes in data outputs
#'
#' This function compares new extraction data against previously extracted survey data to identify changes and differences.
#' It uses read_extracted_df to find comparison data and compare_dataframes to perform the actual comparison.
#' By default it reads from the extracted survey folder. Set read_from_extracted_data_dir to FALSE to compare with the dataframe written in the study folder.
#'
#' @param data data frame of new extraction data to be checked
#' @param filename name of files to be read. Use the format "Country Study_name Year_duration". For example: "USA NHANES 2005-2016"
#' @param study_dir location of study folder for reading extracted files
#' @param extracted_data_dir location of `extracted survey` folder: must be specified when read_from_extracted_data_dir is TRUE
#' @export
check_data_changes <- function(data, filename, study_dir = NULL, extracted_data_dir = NULL) {

  for (curr_dir in list(study_dir, extracted_data_dir)) {
    if (is.null(curr_dir)) next

    print_it("Checking changes in data in comparison with the following file...", "yellow")
    print_it(paste0(curr_dir, filename, ".csv"), indent = 2)

    # Load comparison data using read_extracted_df
    comparison_data <- read_extracted_df(filename, curr_dir)

    if (is.null(comparison_data)) {
      print_it(paste("ERROR - file does not exist"), "br_red")
      return(invisible())
    }

    # Call compare_dataframes to do the actual comparison
    found_any <- compare_dataframes(data, comparison_data)

    if (!found_any) print_it("No changes found", "yellow")

  }

  return(invisible())
}
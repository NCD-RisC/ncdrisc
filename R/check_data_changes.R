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
      return(read.csv(csv_path))
    }, error = function(e) {
      return(NULL)
    })
  }

  # If file doesn't exist or fails to load
  return(NULL)
}

#' Function to compare two dataframes for differences
#'
#' This function performs comprehensive comparison between new and existing extraction data.
#' It compares row counts, column names, and performs detailed value-by-value comparison.
#'
#' @param new_data data frame of new extraction data
#' @param comparison_data data frame of previously extracted data
compare_dataframes <- function(new_data, comparison_data) {

  # flag for if any change was found
  any_change_found <- FALSE

  # Convert both datasets to latin1
  new_data[] <- lapply(new_data, function(x) {
    if (is.character(x)) iconv(x, from = "latin1", to = "UTF-8", sub = "")
    else x
  })
  comparison_data[] <- lapply(comparison_data, function(x) {
    if (is.character(x)) iconv(x, from = "latin1", to = "UTF-8", sub = "")
    else x
  })

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
  new_row_count <- nrow(new_data)
  old_row_count <- nrow(comparison_data)

  if (new_row_count != old_row_count) {
    any_change_found <- TRUE
    print_it("CAUTION - inconsistent number of rows:", "br_violet")
    print_it(paste("New:", new_row_count, "vs Old:", old_row_count), indent = 2)
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

  # Order datasets before value-by-value comparison
  id_column <- "id"

  if (id_column %in% colnames(new_data) && id_column %in% colnames(comparison_data)) {
    # Both datasets have ID columns - check if IDs match
    new_ids <- sort(new_data[[id_column]])
    old_ids <- sort(comparison_data[[id_column]])

    # Determine sorting strategy
    new_data_ordered <- NULL
    comparison_data_ordered <- NULL

    if (identical(new_ids, old_ids)) {
      # STRATEGY A: Sort by ID order
      new_data_ordered <- new_data[order(new_data[[id_column]]), ]
      comparison_data_ordered <- comparison_data[order(comparison_data[[id_column]]), ]

    } else {
      # STRATEGY B: Fall back to sorting by columns with matching means
      common_columns <- intersect(colnames(new_data), colnames(comparison_data))
      matching_mean_columns <- c()

      for (column_name in common_columns) {
        if (is.numeric(new_data[[column_name]]) && is.numeric(comparison_data[[column_name]])) {
          new_mean <- mean(new_data[[column_name]], na.rm = TRUE)
          old_mean <- mean(comparison_data[[column_name]], na.rm = TRUE)
          mean_diff <- abs(new_mean - old_mean)

          if (mean_diff == 0) {
            matching_mean_columns <- c(matching_mean_columns, column_name)
          }
        }
      }

      if (length(matching_mean_columns) > 0) {
        new_data_ordered <- new_data[do.call(order, new_data[matching_mean_columns]), ]
        comparison_data_ordered <- comparison_data[do.call(order, comparison_data[matching_mean_columns]), ]
      }
    }

    # Perform value-by-value comparison
    if (!is.null(new_data_ordered) && !is.null(comparison_data_ordered)) {
      # Get columns that exist in both datasets for comparison
      common_columns <- intersect(colnames(new_data_ordered), colnames(comparison_data_ordered))
      min_length <- min(nrow(new_data_ordered), nrow(comparison_data_ordered))

      # Compare each common column value-by-value
      for (column_name in common_columns) {
        new_column_data <- new_data_ordered[[column_name]][1:min_length]
        old_column_data <- comparison_data_ordered[[column_name]][1:min_length]

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
          any_change_found <- TRUE
          print_it(paste("CAUTION -", length(na_to_nonNA_indices), "NA values changed to non-NA in column", column_name), "br_violet")
          # Show unique new values and their counts
          new_values_from_na <- new_column_data[na_to_nonNA_indices]
          unique_new_values <- unique(new_values_from_na)
          for (value in unique_new_values[1:min(5, length(unique_new_values))]) {
            value_count <- sum(new_values_from_na == value, na.rm = TRUE)
            print_it(paste("NA ->", value, paste0("(", value_count), "cases)"), indent = 2)
          }
        }
        nonNA_to_na_indices <- which(is.na(new_column_data) & !is.na(old_column_data))
        if (length(nonNA_to_na_indices) > 0) {
          any_change_found <- TRUE
          print_it(paste("CAUTION -", length(nonNA_to_na_indices), "non-NA values changed to NA in column", column_name), "br_violet")
          # Show unique old values and their counts
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
            any_change_found <- TRUE
            stop(paste("ERROR - Column", column_name, "has incompatible data types: new data is numeric but old data is not"))
          } else if (!new_is_numeric && old_is_numeric) {
            any_change_found <- TRUE
            stop(paste("ERROR - Column", column_name, "has incompatible data types: new data is not numeric but old data is numeric"))
          }

          # Count value differences (excluding changes from or to NA)
          value_diff_indices <- c()

          if (new_is_numeric && old_is_numeric) {
            # Both numeric: precision tolerance
            value_diff_indices <- which(both_not_na & abs(new_column_data - old_column_data) > 1e-6)
          } else if (!new_is_numeric && !old_is_numeric) {
            # Both non-numeric: exact comparison
            for (i in which(both_not_na)) {
              if (new_column_data[i] != old_column_data[i]) {
                value_diff_indices <- c(value_diff_indices, i)
              }
            }
          }

          if (length(value_diff_indices) > 0) {
            any_change_found <- TRUE
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
    }
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

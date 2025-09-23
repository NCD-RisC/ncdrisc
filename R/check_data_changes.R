# This script provides functions to compare new extraction data against previously extracted survey data to identify changes and differences

#' Function to read previously extracted dataframe
#'
#' This function checks any file in Extracted survey for the relevant country, and looks for an id_study that matches the one currently being checked.
#' It searches through CSV and RData files in the extracted survey directory to find matching study data.
#'
#' @param data data frame containing country and id_study columns for matching
#' @return data frame of previously extracted data for the matching study, or NULL if not found
read_extracted_df <- function(data) {

  required_columns <- c("country", "id_study")
  missing_columns <- setdiff(required_columns, colnames(data))

  if (length(missing_columns) > 0) {
    stop(paste("Missing required columns:", paste(missing_columns, collapse = ", ")))
  }

  # Extract study metadata from input data
  # Get unique values to handle multi-row datasets
  country_name <- unique(data$country)[1]
  id_study_value <- unique(data$id_study)[1]

  # Define the directory where extracted survey data is stored
  surveys_directory <- "S:/Projects/HeightProject/Original dataset/Data/Surveys/Extracted survey/"

  # Get all files in the extracted survey directory
  all_files <- list.files(surveys_directory, full.names = TRUE)

  # Filter files that start with the country name
  # This narrows down the search to relevant files
  matching_files <- all_files[grepl(paste0("^", country_name), basename(all_files))]

  # If no files found for this country, return NULL
  if (length(matching_files) == 0) {
    return(NULL)
  }

  # Search through matching files to find one containing the target study ID
  for (file_path in matching_files) {

    # Use tryCatch to handle file reading errors gracefully
    tryCatch({
      # Load data based on file extension
      if (grepl("\\.csv$", file_path)) {
        temp_data <- read.csv(file_path)
      } else if (grepl("\\.RData$", file_path)) {
        # For RData files, load and get the first object
        load(file_path)
        temp_data <- get(ls()[1])
      } else {
        # Skip unsupported file formats
        next
      }

      # Check if this file contains data for the target study ID
      if ("id_study" %in% colnames(temp_data) && id_study_value %in% temp_data$id_study) {
        # Filter to only the rows matching our target study
        comparison_data <- temp_data[temp_data$id_study == id_study_value, ]
        return(comparison_data)
      }

    }, error = function(e) {
      # Log file reading errors but continue searching
      next
    })
  }

  # If we reach here, no matching data was found
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

  # Compare row counts
  new_row_count <- nrow(new_data)
  old_row_count <- nrow(comparison_data)

  if (new_row_count != old_row_count) {
    print_it("CAUTION - inconsistent number of rows:", "br_violet")
    print_it(paste("New:", new_row_count, "vs Old:", old_row_count), indent = 2)
  }

  # Remove fully empty columns from both datasets
  new_data_non_empty_cols <- sapply(new_data, function(col) !all(is.na(col)))
  comparison_non_empty_cols <- sapply(comparison_data, function(col) !all(is.na(col)))

  new_data <- new_data[, new_data_non_empty_cols, drop = FALSE]
  comparison_data <- comparison_data[, comparison_non_empty_cols, drop = FALSE]

  # Identify columns that are new or have been removed
  new_file_columns <- colnames(new_data)
  old_file_columns <- colnames(comparison_data)

  new_columns <- setdiff(new_file_columns, old_file_columns)
  removed_columns <- setdiff(old_file_columns, new_file_columns)

  if (length(new_columns) > 0) {
    print("CAUTION - added columns:", "br_violet")
    print(new_columns, indent = 2)
  }

  if (length(removed_columns) > 0) {
    print("CAUTION - removed columns:", "br_violet")
    print(removed_columns, indent = 2)
  }

  # Order datasets before value-by-value comparison
  id_column <- "id"

  if (id_column %in% colnames(new_data) && id_column %in% colnames(comparison_data)) {
    # Both datasets have ID columns - check if IDs match
    new_ids <- sort(new_data[[id_column]])
    old_ids <- sort(comparison_data[[id_column]])

    # Determine sorting strategy
    new_data_ordered <- NULL
    comparison_ordered <- NULL

    if (identical(new_ids, old_ids)) {
      # STRATEGY A: Sort by ID order
      new_data_ordered <- new_data[order(new_data[[id_column]]), ]
      comparison_ordered <- comparison_data[order(comparison_data[[id_column]]), ]

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
        comparison_ordered <- comparison_data[do.call(order, comparison_data[matching_mean_columns]), ]
      }
    }

    # Perform value-by-value comparison
    if (!is.null(new_data_ordered) && !is.null(comparison_ordered)) {
      # Get columns that exist in both datasets for comparison
      common_columns <- intersect(colnames(new_data_ordered), colnames(comparison_ordered))
      min_length <- min(nrow(new_data_ordered), nrow(comparison_ordered))

      # Compare each common column value-by-value
      for (column_name in common_columns) {
        new_column_data <- new_data_ordered[[column_name]][1:min_length]
        old_column_data <- comparison_ordered[[column_name]][1:min_length]

        # Count changes from or to NA
        na_to_nonNA <- sum(!is.na(new_column_data) & is.na(old_column_data))
        if (na_to_nonNA > 0) {
          print(paste("CAUTION -", na_to_nonNA, "NA values changed to non-NA in column", column_name), "br_violet")
        }
        nonNA_to_na <- sum(is.na(new_column_data) & !is.na(old_column_data))
        if (nonNA_to_na > 0) {
          print(paste("CAUTION -", nonNA_to_na, "non-NA values changed to NA in column", column_name), "br_violet")
        }

        both_not_na <- !is.na(new_column_data) & !is.na(old_column_data)
        if (sum(both_not_na) > 0) {

          # Check type changes
          new_is_numeric <- is.numeric(new_column_data)
          old_is_numeric <- is.numeric(old_column_data)

          if (new_is_numeric && !old_is_numeric) {
            print(paste("CAUTION -", column_name, "column changed from non-numeric to numeric"), "br_violet")
          } else if (!new_is_numeric && old_is_numeric) {
            print(paste("CAUTION -", column_name, " column changed from numeric to non-numeric"), "br_violet")
          }

          # Count value differences (excluding changes from or to NA)
          value_diff_indices <- c()

          if (new_is_numeric && old_is_numeric) {
            # Both numeric: precision tolerance
            value_diff_indices <- which(both_not_na & abs(new_column_data - old_column_data) > 1e-10)
          } else if (!new_is_numeric && !old_is_numeric) {
            # Both non-numeric: exact comparison
            for (i in which(both_not_na)) {
              if (new_column_data[i] != old_column_data[i]) {
                value_diff_indices <- c(value_diff_indices, i)
              }
            }
          }

          if (length(value_diff_indices) > 0) {
            print(paste("CAUTION -", length(value_diff_indices), "values changed in", column_name), "br_violet")

            # Show first 5 rows of differences as examples
            example_indices <- value_diff_indices[1:min(5, length(value_diff_indices))]
            for (idx in example_indices) {
              print(paste("Row", idx, "- Old:", old_column_data[idx], "New:", new_column_data[idx]), indent = 2)
            }
          }
        }
      }
    }
  }

  return(invisible(NULL))
}


#' Function to check for changes in data outputs
#'
#' This function compares new extraction data against previously extracted survey data to identify changes and differences.
#' It uses read_extracted_df to find comparison data and compare_dataframes to perform the actual comparison.
#'
#' @param data data frame of new extraction data to be checked
#' @param id_study character string of study ID for identification
#' @export
check_data_changes <- function(data, id_study) {

  # Load comparison data using read_extracted_df
  comparison_data <- read_extracted_df(data)

  if (is.null(comparison_data)) {
    print_it(paste("CAUTION -", id_study, "does not exist among extracted surveys. Is this a new extraction?"), "br_violet")
    return(invisible(NULL))
  }

  print_it(paste(id_study, "was previously extracted. Checking changes in data"), "yellow")

  # Call compare_dataframes to do the actual comparison
  compare_dataframes(data, comparison_data)
}
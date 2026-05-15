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
      df_latin1 <- suppressWarnings(read.csv(csv_path, fileEncoding = "latin1"))
      df_utf8 <- suppressWarnings(read.csv(csv_path, fileEncoding = "UTF-8"))
      
      if (nrow(df_utf8) > nrow(df_latin1)) {
        return(df_utf8)
      } else {
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
#' This function compares the data being extracted against previously extracted data.
#' It:
#' - compares the number of rows in the two data frames
#' - compares column names and logs which ones were added/removed
#' - matches data line-by-line and compares every value.
#'
#' @param new_data data frame of the data being extracted
#' @param comparison_data data frame of previously extracted data
compare_dataframes <- function(new_data, comparison_data) {

  any_change_found <- FALSE

  # Convert character columns to latin1
  for (col in names(new_data)) {
    if (is.character(new_data[[col]])) new_data[[col]] <- iconv(new_data[[col]], from = "UTF-8", to = "latin1", sub = "")
  }
  for (col in names(comparison_data)) {
    if (is.character(comparison_data[[col]])) comparison_data[[col]] <- iconv(comparison_data[[col]], from = "UTF-8", to = "latin1", sub = "")
  }
  # Remove fully empty columns from both datasets
  new_data_non_empty_cols <- sapply(new_data, function(col) !all(is.na(col)))
  comparison_non_empty_cols <- sapply(comparison_data, function(col) !all(is.na(col)))

  new_data <- new_data[, new_data_non_empty_cols, drop = FALSE]
  comparison_data <- comparison_data[, comparison_non_empty_cols, drop = FALSE]

  # Remove user and ncdrisc_version columns from old data
  # These are written when calling save_extraction()
  comparison_data <- comparison_data[, !colnames(comparison_data) %in% c("user", "ncdrisc_version"), drop = FALSE]

  std_names_list <- ncdrisc::std_names_list
  common_columns <- intersect(colnames(new_data), colnames(comparison_data))
  numeric_var_list <- as.character(std_names_list$Name[which(std_names_list$Type == "numeric")])

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

  # Ask user how to match rows
  choice <- menu(c("id (use if there is a common id column in the data being extracted and in the previous extraction)",
                   "auto (use if there is no common id column; may be slow with >10,000 rows)"),
                 title = "How to match rows:")

  if (choice == 1) {
    # Match by id
    if (!"id" %in% common_columns) {
      print_it("No id column found", "br_red")
      return(any_change_found)
    }
    columns_for_matching <- "id"

  } else {
    # Find columns that uniquely identify rows
    # Exclude "id" and metadata columns

    available_columns <- common_columns[common_columns != "id"]
    available_columns <- available_columns[!grepl("^age_min_|^age_max_|^is_|_year$", available_columns)]
    available_columns <- available_columns[available_columns %in% numeric_var_list]

    columns_for_matching <- c()
    done <- FALSE

    for (column in available_columns) {
      columns_for_matching <- c(columns_for_matching, column)
      current_subset <- comparison_data[, columns_for_matching, drop = FALSE]

      if (anyDuplicated(current_subset) == 0) {
        done <- TRUE
        break
      }
    }

    if (!done) {
      print_it("Could not automatically match rows - rows are not unique", "br_red")
      print_it("This can be expected with large datasets (>10,000 rows)", indent = 2)
      print_it("If that is not the case, please check if having duplicate rows in the data being extracted is expected", indent = 2)
      print_it("If there is no common id column and automatic matching fails, consistency with previous extraction should be carefully checked manually", indent = 2)
      debug_matching_subset <<- current_subset
      return(any_change_found)
    }
  }
  
  # Semi-join: keep only rows in new_data that have a match in comparison_data, and vice versa
  new_keys <- do.call(paste, new_data[, columns_for_matching, drop = FALSE])
  old_keys <- do.call(paste, comparison_data[, columns_for_matching, drop = FALSE])

  new_in_old <- new_keys %in% old_keys
  old_in_new <- old_keys %in% new_keys

  new_rows <- sum(!new_in_old)
  removed_rows <- sum(!old_in_new)
  if (new_rows > 0) {
    any_change_found <- TRUE
    print_it(paste("CAUTION -", new_rows, "added rows"), "br_violet")
  }
  if (removed_rows > 0) {
    any_change_found <- TRUE
    print_it(paste("CAUTION -", removed_rows, "removed rows"), "br_violet")
  }

  new_data_ordered <- new_data[new_in_old, ]
  comparison_data_ordered <- comparison_data[old_in_new, ]

  # Sort both by matching columns
  new_data_ordered <- new_data_ordered[do.call(order, new_data_ordered[, columns_for_matching, drop = FALSE]), ]
  comparison_data_ordered <- comparison_data_ordered[do.call(order, comparison_data_ordered[, columns_for_matching, drop = FALSE]), ]

  print_it(paste("Comparing", nrow(new_data_ordered), "rows across", length(common_columns), "columns"), "yellow")

      # Compare each common column value-by-value
      for (column_name in common_columns) {
        new_column_data <- new_data_ordered[[column_name]]
        old_column_data <- comparison_data_ordered[[column_name]]

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
            value_diff_indices <- which(both_not_na & new_column_data != old_column_data)
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
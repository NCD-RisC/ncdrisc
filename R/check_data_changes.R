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
#' This function compares the data being extracted against previously extracted data.
#' It:
#' - compares the number of rows in the two data frames
#' - compares column names and logs which ones were added/removed
#' - matches data line-by-line and compares every value.
#'
#' @param new_data data frame of the data being extracted
#' @param old_data data frame of previously extracted data
compare_dataframes <- function(new_data, old_data) {

  any_change <- FALSE

  # Convert character columns to latin1
  for (col in names(new_data)) {
    if (is.character(new_data[[col]])) new_data[[col]] <- iconv(new_data[[col]], from = "UTF-8", to = "latin1", sub = "")
  }
  for (col in names(old_data)) {
    if (is.character(old_data[[col]])) old_data[[col]] <- iconv(old_data[[col]], from = "UTF-8", to = "latin1", sub = "")
  }
  numeric_var_list <- as.character(std_names_list$Name[which(std_names_list$Type == "numeric")])

  # Remove fully empty columns
  new_data <- new_data[, sapply(new_data, function(x) !all(is.na(x))), drop = FALSE]
  old_data <- old_data[, sapply(old_data, function(x) !all(is.na(x))), drop = FALSE]

  # Remove user and ncdrisc_version columns from old data
  # These are written when calling save_extraction()
  old_data <- old_data[, !colnames(old_data) %in% c("user", "ncdrisc_version"), drop = FALSE]
  
  # Compare column names
  new_cols <- setdiff(colnames(new_data), colnames(old_data))
  removed_cols <- setdiff(colnames(old_data), colnames(new_data))
  if (length(new_cols) > 0) {
    any_change <- TRUE
    print_it("CAUTION - Added columns:", "br_violet")
    print_it(new_cols, indent = 2)
    added_columns <<- new_data[, new_cols, drop = FALSE]
  }
  if (length(removed_cols) > 0) {
    any_change <- TRUE
    print_it("CAUTION - Removed columns:", "br_violet")
    print_it(removed_cols, indent = 2)
  }

  # Ask user how to match rows
  choice <- menu(c("id (use if there is a common id column in the data being extracted and in the previous extraction)",
                   "auto (use if there is no common id column; may be slow with >100,000 rows)"),
                 title = "How to match rows:")
  
  common_columns <- intersect(colnames(old_data), colnames(new_data))

  if (choice == 1) {
    # Match by id
    if (!"id" %in% common_columns) {
      print_it("No id column found", "br_red")
      return(any_change)
    }
    columns_for_matching <- "id"
    print_it("Matching by id", "yellow")

  } else {
    # Find columns that uniquely identify rows
    # Exclude "id" and metadata columns

    available_columns <- common_columns[common_columns != "id"]
    available_columns <- available_columns[!grepl("^age_min_|^age_max_|^is_|_year$", available_columns)]
    available_columns <- available_columns[available_columns %in% numeric_var_list]
    print_it(paste("Numeric columns available:", paste(available_columns, collapse = ", ")), "yellow")

    columns_for_matching <- c()
    done <- FALSE

    for (column in available_columns) {
      columns_for_matching <- c(columns_for_matching, column)
      current_subset <- old_data[, columns_for_matching, drop = FALSE]

      if (anyDuplicated(current_subset) == 0) {
        done <- TRUE
        break
      }
    }

    if (!done) {
      print_it("Could not automatically match rows - rows are not unique", "br_red")
      print_it("This can be expected with large datasets (>100,000 rows)", indent = 2)
      print_it("If that is not the case, please check if having duplicate rows in the data being extracted is expected", indent = 2)
      print_it("If there is no common id column and automatic matching fails, consistency with previous extraction should be carefully checked manually", indent = 2)
      debug_matching_subset <<- current_subset
      return(any_change)
    }
  }
  
  new_for_matching <- new_data[, columns_for_matching, drop = FALSE]
  new_for_matching$new_row_index <- seq_len(nrow(new_data))

  old_for_matching <- old_data[, columns_for_matching, drop = FALSE]
  old_for_matching$old_row_index <- seq_len(nrow(old_data))

  matched_data <- merge(new_for_matching, old_for_matching, by = columns_for_matching, all.x = TRUE)

  new_rows <- sum(is.na(matched_data$old_row_index))
  removed_rows <- nrow(old_data) - length(unique(na.omit(matched_data$old_row_index)))
  if (new_rows > 0) {
    any_change <- TRUE
    print_it(paste("CAUTION -", new_rows, "added rows"), "br_violet")
  }
  if (removed_rows > 0) {
    any_change <- TRUE
    print_it(paste("CAUTION -", removed_rows, "removed rows"), "br_violet")
  }

  matched_data <- matched_data[!is.na(matched_data$old_row_index), ]
  columns_to_compare <- intersect(colnames(new_data), colnames(old_data))

  print_it(paste("Comparing", nrow(matched_data), "rows across", length(columns_to_compare), "columns"), "yellow")

  new_data_matched <- new_data[matched_data$new_row_index, ]
  old_data_matched <- old_data[matched_data$old_row_index, ]

  for (column in columns_to_compare) {
    new_values <- new_data_matched[[column]]
    old_values <- old_data_matched[[column]]

    if (column %in% numeric_var_list) {
      new_values <- as.numeric(new_values)
      old_values <- as.numeric(old_values)
    } else {
      if (!is.character(new_values)) new_values <- as.character(new_values)
      if (!is.character(old_values)) old_values <- as.character(old_values)
    }

    # NA to value
    changed_indices <- which(!is.na(new_values) & is.na(old_values))
    if (length(changed_indices) > 0) {
      any_change <- TRUE
      print_it(paste("CAUTION -", length(changed_indices), "NA changed to value in", column), "br_violet")
      values <- new_values[changed_indices]
      for (v in unique(values)[1:min(5, length(unique(values)))]) {
        print_it(paste("NA ->", v, paste0("(", sum(values == v, na.rm = TRUE), ")")), indent = 2)
      }
    }

    # Value to NA
    changed_indices <- which(is.na(new_values) & !is.na(old_values))
    if (length(changed_indices) > 0) {
      any_change <- TRUE
      print_it(paste("CAUTION -", length(changed_indices), "values changed to NA in", column), "br_violet")
      values <- old_values[changed_indices]
      for (v in unique(values)[1:min(5, length(unique(values)))]) {
        print_it(paste(v, "-> NA", paste0("(", sum(values == v, na.rm = TRUE), ")")), indent = 2)
      }
    }

    # Value changes
    both_have_values <- !is.na(new_values) & !is.na(old_values)
    if (sum(both_have_values) == 0) next

    if (is.numeric(new_values)) {
      changed_indices <- which(both_have_values & abs(new_values - old_values) > 1e-6)
    } else {
      changed_indices <- which(both_have_values & new_values != old_values)
    }

    if (length(changed_indices) > 0) {
      any_change <- TRUE
      print_it(paste("CAUTION -", length(changed_indices), "values changed in", column), "br_violet")
      changes <- paste(old_values[changed_indices], "->", new_values[changed_indices])
      for (change in unique(changes)[1:min(5, length(unique(changes)))]) {
        print_it(paste(change, paste0("(", sum(changes == change), ")")), indent = 2)
      }
    }
  }

  return(any_change)
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

    # Load old data using read_extracted_df
    old_data <- read_extracted_df(filename, curr_dir)

    if (is.null(old_data)) {
      print_it(paste("ERROR - file does not exist"), "br_red")
      return(invisible())
    }

    # Call compare_dataframes to do the actual comparison
    found_any <- compare_dataframes(data, old_data)

    if (!found_any) print_it("No changes found", "yellow")

  }

  return(invisible())
}
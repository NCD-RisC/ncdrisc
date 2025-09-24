
#' Function to save extraction
#'
#' This function adds user information to extracted data, save extraction logs and save formatted extractions.
#' It has to be run from the study folder.
#'
#' @param data data frame to be saved
#' @param filename name of files to be saved. Use the format "Country Study_name Year_duration". For example: "USA NHANES 2005-2016"
#' @param study_dir location of study folder for saving extracted files
#' @param extracted_data_dir location of `extracted survey` folder: must be specified manually
#' @param save_to_extracted_data_dir whether save extracted files to `extracted survey` folder
#' @export
save_extraction <- function(data, filename, study_dir = NULL, save_to_study_dir = TRUE, extracted_data_dir = NULL, save_to_extracted_data_dir = TRUE) {

    # Specify study folder
    if (is.null(study_dir)) study_dir <- paste0(getwd(), "/")

    # Specify extracted survey folder
    if (is.null(extracted_data_dir) & save_to_extracted_data_dir) {
        stop('Please specify `extracted_data_dir = "S:/Projects/HeightProject/Original dataset/Data/Surveys/Extracted Survey/"` (or a custom location), or set `save_to_extracted_data_dir = FALSE`')
    }

    # Function to record message in the extraction log
    print_message <- function(prefix = '') {
        cat(prefix)
        cat(
            format(time, '%Y-%m-%d %H:%M:%S'),
            paste0('----- modified by ', user, ' using ncdrisc ', version, '\n'),
            paste0(prefix, filename, '.csv:'),
            paste(unique(data$id_study), collapse = ', '), '\n'
        )
    }

    # Add user name and ncdrisc package version to extraction data frame
    user <- Sys.getenv("USERNAME")
    if (user == '') stop("Running from non-Windows system is not supported - user name cannot be recorded.")
    version <- packageVersion('ncdrisc')
    data$user <- user
    data$ncdrisc_version <- version

    print_it("Username added to extraction:", "yellow")
    print_it(user, indent = 2)
    print_it("ncdrisc package version added to extraction:", "yellow")
    print_it(version, indent = 2)

    if (save_to_study_dir) {
        # Save RData file in study folder for fast loading of extracted data in the future
        save(data, file = paste0(study_dir, filename, ".RData"))
        print_it("Files saved:", "yellow")
        print_it(paste0(study_dir, filename, ".RData"), indent = 2)

        # Save CSV file in study folder
        write.csv(data, paste0(study_dir, filename, ".csv"), row.names=FALSE, fileEncoding="latin1")
        print_it(paste0(study_dir, filename, ".csv"), indent = 2)

        print_it("Set `study_dir` to the correct location if the above folder path is incorrect.", indent = 2)
    } else {
        print_it(paste0("Files are not saved in the study folder: ", study_dir), "yellow")
        print_it("Set `save_to_study_dir = TRUE` if the files should be written to the study folder.", indent = 2)
        print_it("Set `study_dir` to the correct location if the above folder path is incorrect.", indent = 2)
    }

    if (save_to_extracted_data_dir) {
        # Save CSV file in Extracted survey folder
        write.csv(data, paste0(extracted_data_dir, filename, ".csv"), row.names=FALSE, fileEncoding="latin1")
        print_it(paste0(extracted_data_dir, filename, ".csv"), indent = 2)

        # Add extraction log
        time <- Sys.time()
        sink(paste0(extracted_data_dir, 'logs/extraction_modification_log.txt'), append = TRUE)
        print_message()
        sink()

        # Print extraction log message in console
        print_it("Log updated:", "yellow")
        print_it(paste0(extracted_data_dir, "logs/extraction_modification_log.txt"), indent = 2)
        print_it("Log message:", "yellow")
        print_message(' ')

        print_it("Extraction done.", "yellow")
    } else {
        tmp_text <- ifelse(save_to_study_dir, "saved in the study folder, but not", "not saved")
        print_it(paste0("Files are ", tmp_text, " in the `extracted survey` folder."), "yellow")
        print_it("Set `save_to_extracted_data_dir = FALSE` if the files in `extracted survey` folder should be updated.", indent = 2)
    }

}


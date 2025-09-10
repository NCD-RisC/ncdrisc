
#' Function to save extraction
#' This function adds user information to extracted data, save extraction logs and save formatted extractions.
#' It has to be run from the study folder.
#' @param data data frame to be saved
#' @param filename name of files to be saved. Use the format "Country Study_name Year_duration". For example:
#'                 "USA NHANES 2005-2016"
#' @param extracted_data_dir location of extracted survey folder
#' @export
save_extraction <- function(data, filename, extracted_data_dir = "S:/Projects/HeightProject/Original dataset/Data/Surveys/Extracted Survey/") {
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
    print("Username added to extraction:", "yellow")
    print(user, indent = 2)
    print("ncdrisc package version added to extraction:", "yellow")
    print(version, indent = 2)

    # Save RData file in study folder for fast loading of extracted data in the future
    save(data, file = paste0(filename, ".RData"))
    print("Files saved:", "yellow")
    print(paste0(getwd(), "/", filename, ".RData"), indent = 2)

    # Save CSV file in study folder
    write.csv(data, paste0(filename, ".csv"), row.names=FALSE)
    print(paste0(getwd(), "/", filename, ".csv"), indent = 2)

    # Save CSV file in Extracted survey folder
    write.csv(data, paste0(extracted_data_dir, filename, ".csv"), row.names=FALSE)
    print(paste0(extracted_data_dir, filename, ".csv"), indent = 2)

    # Add extraction log
    time <- Sys.time()
    sink(paste0(extracted_data_dir, 'logs/extraction_modification_log.txt'), append = TRUE)
    print_message()
    sink()

    # Print extraction log message in console
    print("Log updated:", "yellow")
    print(paste0(extracted_data_dir, "logs/extraction_modification_log.txt"), indent = 2)
    print("with the following message:", "yellow")
    print_message(' ')
}



## Edit history
## Rosie May 2023
## Bin Jan 2024: added condition for checking age range; added checks for value constraints for BP and waist/hip
## YDDB Dec 2024: adding checks for variables that should be 0/1 - not completed yet (10/12/2024)
# ABP March 2025: Corrected line 125 to avoid error caused by only one variable of the pairs available
# BZ March 2025: corrected condition for pair wise comparison where integer overflow happened when multiplying two large Ns
# BZ March 2025: added checks for empty strings or 'NA' strings in variable values; added check for unique values in metadata columns; added more comments
# FD September 2025: added checks for hb altitude adjustment

#' Function to check extractions before saving
#'
#' This function checks formatted extractions and print out error, check and caution messages
#' when issues are identified. Errors need immediate attention and will stop the other checks.
#' Checks are usually issues that need amending and they are produced together in single run.
#' Cautions may not be a problem; they highlight something that require an active check and
#' decision to ignore.
#'
#' @param dataset data frame of extraction to be checked: can be a single study or multiple studies.
#' @return NULL
#' @export
check_extraction <- function(dataset) {

  # Load data:

  # standard country names
  # countrylist <- read.csv("S:/Projects/HeightProject/Original dataset/Covariates/country-list-2023-new.csv")
  countrylist <- ncdrisc::countrylist

  # standard variable name list - the list should be updated separately
  # std_names_list <- read.csv("S:/Projects/HeightProject/Original dataset/Data/Surveys/__Extraction Template/standard_variable_names.csv")
  std_names_list <- ncdrisc::std_names_list

  # List of high-altitutde countries that Hb needs adjusting
  # high_altitude_countries <- read.csv("S:/Projects/HeightProject/Original dataset/Anaemia/altitude info/high_altitude_countries.csv")
  high_altitude_countries <- ncdrisc::high_altitude_countries

  # Legacy naming issue
  # we use ha1c in extraction but hba1c downstream - rename hba1c variables to ha1c for these checks
  std_names_list$Name[std_names_list$Name == "device_hba1c"] <- "device_ha1c"
  std_names_list$Name[std_names_list$Name == "unit_hba1c"] <- "unit_ha1c"
  std_names_list$Name[std_names_list$Name == "hba1c"] <- "ha1c"

  # Get numerical variable names
  numeric_var_list <- as.character(std_names_list$Name[which(std_names_list$Type == "numeric")])
  # Get metadata variable names
  meta_var_list <- as.character(std_names_list$Name[which(std_names_list$If_metadata == "meta")])

  # In case more than one study in one data frame
  stud_id <- unique(as.character(dataset$id_study))

  # Fatal error: No id_study column
  if (sum(names(dataset) == 'id_study') == 0) {
    stop("File missing id_study")
  }

  # Check each study separately
  for (i in stud_id) {
    dat <- subset(dataset, dataset$id_study == i)

    print_it(paste("CHECKING ",i), "yellow")

    # Fatal error: more than one unique values in metadata variables
    unique_value_lengths <- sapply(intersect(meta_var_list, names(dat)), function(v) length(unique(dat[[v]])))
    if(any(unique_value_lengths)>1) {
      print_it("CHECK - non-unique values in metadata variables:", "br_red")
      print_it(names(unique_value_lengths)[unique_value_lengths>1], indent = 2)
    }

    # Inconsistency: iso in id_study
    if (any(unique(dat$iso) != substr(i, 1, 3))) {
      print_it("CHECK - inconsistent ISO with id_study:", "br_red")
      print_it(paste(i, "vs", paste(unique(dat$iso), collapse = ', ')))
    }

    # Fatal error: typo in iso and country name
    if (any(!unique(dat$iso) %in% countrylist$iso)) {
      print_it("CHECK - ISO not in country list:", "br_red")
      print_it(paste(i, "vs", paste(unique(dat$iso), collapse = ', ')))
    } else {
      if (any(!unique(dat$country) %in% countrylist$Country[countrylist$iso %in% unique(dat$iso)])) {
        print_it("CHECK - country name not in country list:", "br_red")
        print_it(paste(unique(dat$country), collapse = ', '))
      }
    }

    # Inconsistency: mid_year in id_study
    if (any(unique(dat$mid_year) != as.numeric(substr(i, 5, 8)))) {
      print_it("CHECK - inconsistent mid_year with id_study:", "br_red")
      print_it(paste(i, "vs", paste(unique(dat$mid_year), collapse = ', ')))
    }

    # Inconsistency: survey_short in id_study
    if (any(unique(dat$survey_short) != substr(i, 10, nchar(i)))) {
      print_it("CHECK - inconsistent survey_short with id_study:", "br_red")
      print_it(paste(i, "vs", paste(unique(dat$survey_short), collapse = ', ')))
    }

    # Fatal error: non-standard coding in survey_type
    if (any(!unique(dat$survey_type) %in% c("National", "Subnational", "Community", "mixed", "Mixed"))) {
      print_it("CHECK - non-standard coding for survey_type:", "br_red")
      print_it(paste(unique(dat$survey_type), collapse = ', '))
    }

    # Fatal error: non-standard coding in survey_type
    if (any(!unique(dat$urban_rural) %in% c("urban", "rural", "both", "mixed", "Mixed"))) {
      print_it("CHECK - non-standard coding for urban_rural:", "br_red")
      print_it(paste(unique(dat$urban_rural), collapse = ', '))

    }

    # Fatal error: duplicated column names
    if (any(grepl("\\.1", names(dat)))) {
      print_it("CHECK - variables with '.1' in names indicating duplicated columns in extraction", "br_red")
      print_it(grep("\\.1", names(dat), value = TRUE))
    }

    # Check numerical variables
    classes <- lapply(dat[, names(dat) %in% numeric_var_list], class)
    non_numeric_list <- names(classes[!classes %in% c("numeric", "integer", "logical")])
    if (length(non_numeric_list) > 0) {
      print_it("CHECK - numerical variables in non-numeric formats:", "br_red")
      print_it(non_numeric_list)
    }

    # Fatal error: sex variable missing or miscoded
    if(any(!dat$sex %in% c(1,2) & !is.na(dat$sex))){
      print_it("CHECK - sex miscoding", "br_red")
    }

    # Check: confirm that only one sex is avaiable or whether it was miscoded
    if(any(is.na(dat$sex))){
      if(length(unique(dat$sex)) ==2){
        print_it("CHECK - only one value for sex and NA: is sex coded correctly?")
      }
    }

    # Check age range variables
    if(any(is.na(dat[,grepl('age_min|age_max',names(dat))]))){
      if(length(unique(dat$sex)) ==2){
        print_it("CAUTION - age_min and age_max variables have NA in them: please verify this is intended", "br_violet")
      }
    }

    # Fatal error: no age variable
    if(sum(!is.na(dat$age)) == 0){
      if (!'age_mean' %in% names(dat) | ('age_mean' %in% names(dat) & sum(!is.na(dat$age_mean)) == 0)) {
        print_it("CHECK - age missing for all", "br_red")
      }
    }

    # Check height data is not in metre
    if('height' %in% names(dat) & sum(!is.na(dat$height)) > 0 ){
      if (mean(dat$height, na.rm=TRUE)<80) print_it("CHECK - mean value for height too low: is it extracted in metre or inch?", "br_red")
    } else if ('height1' %in% names(dat) & sum(!is.na(dat$height1)) > 0) {
      if (mean(dat$height1, na.rm=TRUE)<80) print_it("CHECK - mean value for height too low: is it extracted in metre or inch?", "br_red")
    }

    # Check sex if height data available
    if(any(dat$sex %in% c(1) &  dat$sex %in% c(2))){
      if (any(grepl("height", names(dat)))) {
        if (!"height" %in% names(dat)) {
          dat$height <- dat$height1
        }
        m_height <- mean(dat$height[which(dat$height>0 & dat$sex == 1)])
        f_height <- mean(dat$height[which(dat$height>0 & dat$sex == 2)])
        if (m_height <= f_height) {
          print_it("CHECK - male height smaller than female height: is sex coded correctly?", "br_red")
          base::print(tapply(dat$height, dat$sex, mean, na.rm=TRUE))
        }
      }
    }

    # check variable value constraints, allowing for multiple measures
    # in each pair c(A,B), check if A > B on average
    # Corrected by ABP March 2025 to avoid error caused by only one variable of the pairs available

    # Current checks: SBP > DBP; Hip > Waist
    check_list <- list(c("sbp", "dbp"), c("hip", "waist"))
    for (pair in check_list) {
      vars1 <- sort(grep(paste0("^", pair[1], "(([0-9]+$)|($)|(_avg$))"), names(dat), value = TRUE))
      vars2 <- sort(grep(paste0("^", pair[2], "(([0-9]+$)|($)|(_avg$))"), names(dat), value = TRUE))

      # Skip the pair if one of the variable lists is empty
      if (length(vars1) == 0 || length(vars2) == 0) {
        #cat("Skipping pair", pair[1], "-", pair[2], ": CHECK: One or both variables not found in the dataset.\n")
        next
      }

      cleaned_vars1 <- gsub(paste0("^", pair[1]), "", vars1)
      cleaned_vars2 <- gsub(paste0("^", pair[2]), "", vars2)

      if (!identical(cleaned_vars1, cleaned_vars2)) {
        print_it("CHECK - unexpected variable name patterns: are variables named correctly?")
        print_it(vars1, indent = 2)
        print_it(vars2, indent = 2)
        next
      }

      if (length(vars1) == 0) next

      failed_list <- c()
      for (i in 1:length(vars1)) {
        v1 <- dat[[vars1[i]]]
        v2 <- dat[[vars2[i]]]
        if (sum(!is.na(v1)) == 0 & sum(!is.na(v2)) == 0) next
        if ((all(is.na(v1)) | all(is.na(v2)))) {
          if (pair[1] != 'hip') {
            print_it(paste0("CHECK - only one of ", vars1[i], " and ", vars2[i], " exists in extraction", "br_red"))
          }
          next
        }
        if (mean(v1[v1>0], na.rm=TRUE) <= mean(v2[v2>0], na.rm=TRUE)) {
          failed_list <- c(failed_list, paste(vars1[i], '<=', vars2[i]))
        }
      }

      if (length(failed_list)>0) {
        print_it("CHECK - the following variables have unexpected relationships (the former should always > the latter):", "br_red")
        print_it(failed_list, indent = 2)
      }

    }

    # Check if urban_rural is 0 or 1 and urban_rural isn't "both"
    if (unique(dat$urban_rural) != "both") {
      if (unique(dat$iso) == "TKL") {
        print_it("CHECK - iso is TKL which has perurb=0, so we should have urban_rural = both", "br_red")
      } else {
        if (unique(dat$iso) %in% c("NRU","BMU","KWT") | (unique(dat$iso) == "SGP" & unique(dat$mid_year) > 2001)) {
          print_it("CHECK - perurb=1 for survey and country-year, so we should have urban_rural = both", "br_red")
        }
      }
    }

    # Check that the columns with the same sum are expected
    # if two measurement columns appear (eg sbp1 and sbp2), likely they have been duplicated in extraction
    classes <- subset(classes, classes %in% c("numeric", "integer"))
    # classes <- classes[!grepl('^age_m|^is_ldl|^is_multi|_year|is_plasma', names(classes))] #remove metadata columns
    classes <- classes[!classes %in% meta_var_list] #remove metadata columns
    col <- data.frame(sum = colSums(dat[names(classes)], na.rm = TRUE), var = names(classes))
    col <- subset(col, !col$sum %in% c(0, 1)) # remove as likely NA or metadata with 0/1 coding
    if (any(duplicated(col$sum)) == TRUE) {
      print_it("CAUTION - columns with the same sum - was the same variable extracted twice?", "br_violet")
      print_it("Look out for measurement columns below:", indent = 2)  # to downgrade message to grey if only samplewt variables
      dup_sum <- col$sum[duplicated(col$sum)]
      print_it(col$var[col$sum %in% dup_sum], indent = 2)
    }

    # TODO: check if drug and self variables are identical

    # Check against standard variable names
    nonstd_name <- setdiff(names(dat)[!names(dat) %in% std_names_list$Name & !grepl("\\.1", names(dat))], "mod_time")
    if (length(nonstd_name) > 0) {
      print_it("CHECK - non-standard variable names found:", "br_red")
      print_it(nonstd_name, indent = 2)
    }
    #col01 <- setdiff(grep("is_|self|averaged", colnames(dat), value = T),grep("_age|_year|standard", colnames(dat), value = T))
    #for(i in col01) {
    #  if(any(!dat[,i] %in% c(0,1)))
    #   print(i)
    #}

    # Check existence of white space and if any column has blank strings or "NA" coded as strings
    # for non-metadata variables
    tmpcheck <- lapply(setdiff(names(dat), meta_var_list), function(v) {
      x1 <- as.character(dat[[v]])
      x2 <- trimws(x1)

      res <- data.frame(ws = !identical(x1,x2),
                        empty = any(x2 == "", na.rm = TRUE),
                        na = any(x2 == "NA", na.rm = TRUE))
      rownames(res) <- v
      return(res)
    })
    tmpcheck <- do.call(rbind, tmpcheck)
    # white space
    if (any(tmpcheck$ws)) {
      print_it("CHECK - remove white space in the values of variables (using trimws function):", "br_red")
      print_it(rownames(tmpcheck)[tmpcheck$ws], indent = 2)
    }
    # empty string
    if (any(tmpcheck$empty)) {
      print_it("CHECK - recode empty or blank strings to NA:", "br_red")
      print_it(rownames(tmpcheck)[tmpcheck$empty], indent = 2)
    }
    # NA in value
    if (any(tmpcheck$na)) {
      print_it("CHECK - recode NA stored as string to NA:", "br_red")
      print_it(rownames(tmpcheck)[tmpcheck$na], indent = 2)
    }

    ### Haemoglobin related checks ###
    # Check if hb columns exist then requires_hb_adjustment must exist
    hb_columns <- c("hb", "hb1", "hb2", "adj_hb", "adj_hb1", "adj_hb2")
    existing_hb_columns <- intersect(hb_columns, names(dat))
    if (length(existing_hb_columns) > 0) {
      if (!"requires_hb_adjustment" %in% names(dat)) {
        # Fatal error: essential metadata missing for hb
        print_it("CHECK - hb columns present but requires_hb_adjustment missing:", "br_red")
        print_it(existing_hb_columns, indent = 2)
      }
    }

    # Check country against high altitude countries list
    if ("requires_hb_adjustment" %in% names(dat)) {
      tryCatch({
        current_iso <- unique(dat$iso)[1]
        is_high_altitude <- current_iso %in% high_altitude_countries$iso3
        current_requires_adjustment <- unique(dat$requires_hb_adjustment)[1]

        if (is_high_altitude && current_requires_adjustment != 1) {
          print_it("CAUTION - country is in the list of countries that require hb altitude adjustment but requires_hb_adjustment was set to 0:", "br_violet")
          print_it(paste("ISO:", current_iso, "- is this a community study in a low altitude area?"), indent = 2)
        } else if (!is_high_altitude && current_requires_adjustment != 0) {
          print_it("CHECK - country is not in the list of countries that require hb altitude adjustment but requires_hb_adjustment was set to 1:", "br_red")
          print_it(paste("ISO:", current_iso), indent = 2)
        }
      }, error = function(e) {
        print_it("ERROR - could not read high altitude countries file for validation:", "br_violet")
        print_it(paste("Error:", e$message), indent = 2)
      })
    }

    # Check if requires_hb_adjustment = 1 then required variables exist
    if ("requires_hb_adjustment" %in% names(dat)) {
      current_requires_adjustment <- unique(dat$requires_hb_adjustment)[1]
      if (current_requires_adjustment == 1) {
        adj_hb_vars <- c("adj_hb", "adj_hb1", "adj_hb2")
        method_vars <- c("hb_adjustment_method", "hb_adjustment_method2", "hb_adjustment_method3")
        hb_vars <- c("hb", "hb1", "hb2")

        has_adj_hb <- any(adj_hb_vars %in% names(dat))
        has_method <- any(method_vars %in% names(dat))
        has_hb <- any(hb_vars %in% names(dat))
        has_altitude <- "altitude" %in% names(dat)

        valid_combination <- (has_adj_hb && has_method) || (has_hb && has_altitude)

        if (!valid_combination) {
          print_it("CHECK - requires_hb_adjustment is set to 1 but required variables are missing:", "br_red")
          print_it("Need one of the two:\n  (1) adjusted haemoglobin measurements (adj_hb) and adjustment method (hb_adjustment_method)\n  (2) unadjusted haemoglobin measurements (hb) and altitude (altitude)", indent = 2)
        }
      }
    }
    ### End of haemoglobin related checks ###

    ## TODO: check if 0/1 variables are coded as 0/1
    ## TODO: check unit variables: we can add correct units to the standard variable list - start from just the anthro/lipids/glucose variables
    #        eg anthro has to be metric (and cm instead of m), biomarkers have to be either mmol/L or mg/dL (I think we allow mg/dl too), a1c in % or mmol/mol
    ## TODO: check special characters in columns other than survey name

    # Check for changes against previously extracted data
    check_data_changes(dat, i)

    print_it("DONE", "yellow")
  }
}


#' General cleaning function
#' @description Cleans a specific variable in the data frame. For fgl and ppg, the v
#' @param data The data frame to be cleaned.
#' @param var_name The name string of the variable to be cleaned. Currently supported variables:
#' * sex, age
#' * height, height1, height2, height3
#' * weight, weight1, weight2, weight3
#' * waist, waist1, waist2, waist3
#' * hip, hip1, hip2, hip3
#' * bmi, whr, whtr
#' * fgl, ppg, ha1c
#' * sbp1-13, sbp_avg, dbp1-13, dbp_avg
#' * chol, hdl, ldl, trg
#' * is_pregnant, is_pregnant_exam, is_urban, is_plasma, is_plasma_ppg
#' * self_diab, drug_diab, drug_diab_pill, drug_diab_insu
#' * self_hyper, self_hyper_12mos, self_hyper_preg, drug_hyper, drug_presc, drug_bp
#' * self_chol, drug_chol, drug_chol_stat, drug_chol_fibr
#' @param known_values_for_missing_data A list of non-NA values known to be used for missing data, which will be ignored when printing percentage cleaned.
#'
#' @return returns a cleaned variable
#' @seealso [clean_data_index()], [clean_categorical()], [clean_continuous()], [convert_unit()]
#' @examples
#' data$age_clean <- clean_data(data, 'age');
#' data$bmi_clean <- clean_data(data, 'bmi');
#' data$fgl_clean <- clean_data(data, 'fgl');
#' data$height_clean <- clean_data(data, 'height', c(-99,-1,0,999));
#' @export
clean_data <- function(data, var_name, known_values_for_missing_data = c() ) {

  # set values in `known_values_for_missing_data` list to NA
  clean_list <- which(data[[var_name]] %in% known_values_for_missing_data)
  if (length(clean_list) > 0) {
    cat(paste0('Pre-cleaning variable "', var_name, '":\n'))
    cat(paste0('  ', length(clean_list), ' of ', sum(!is.na(data[[var_name]])), ' removed containing the following values\n'))
    cat(paste0('  ', paste(known_values_for_missing_data, collapse = ', '), '\n'))

    # set values in `known_values_for_missing_data` to NA
    data[clean_list, var_name] <- NA
  }

  # convert unit
  # convert whole-blood to plasma equivalent for FPG (and exclude for PPG)
  data <- convert_unit(data, var_name)

  # clean data
  clean_index <- clean_data_index(data, var_name)

  v <- data[[var_name]]
  v[clean_index] <- NA
  return(v)
}

#' Get index of cleaned variables
#' @description Cleans a specific variable in the data frame
#' @param data The data frame to be cleaned.
#' @param variable The name of the variable to be cleaned.
#' @return A index of the values in the variable that should be cleaned.
#' @seealso [clean_data()]
#' @examples
#' cleaned_index <- clean_data_index(data, 'age');
#' cleaned_index <- clean_data_index(data, 'bmi');
#' @export
clean_data_index <- function(data, variable) {
  clean_list <- switch(variable,
                       sex    = clean_categorical(data[[variable]], variable, c(1,2), data$id_study),
                       age    = clean_age(data$age, data$id_study),

                       height1 = ,
                       height2 = ,
                       height3 = ,
                       height =
                         clean_height(data[[variable]], variable, data$age, data$id_study),
                       weight1 = ,
                       weight2 = ,
                       weight3 = ,
                       weight =
                         clean_weight(data[[variable]], variable, data$age, data$is_pregnant, data$id_study),
                       bmi =
                         clean_bmi(data[[variable]], variable, data$age, data$is_pregnant, data$id_study),

                       waist1 = ,
                       waist2 = ,
                       waist3 = ,
                       waist =
                         clean_waist(data[[variable]], variable, data$age, data$is_pregnant, data$id_study),
                       hip1 = ,
                       hip2 = ,
                       hip3 = ,
                       hip =
                         clean_hip(data[[variable]], variable, data$age, data$is_pregnant, data$id_study),

                       whtr = clean_wth(data[[variable]], data$age, data$is_pregnant, data$id_study),
                       whr  = clean_whr(data[[variable]], data$age, data$is_pregnant, data$id_study),

                       is_pregnant = , is_pregnant_exam =
                           clean_preg(data[[variable]], variable, data$age, data$sex, data$id_study),
                       is_urban = ,
                       is_plasma = , is_plasma_ppg =
                         clean_categorical(data[[variable]], variable, c(0,1), data$id_study),

                       drug_diab = ,
                       drug_diab_insu = ,
                       drug_diab_pill = ,
                       self_diab = ,
                       is_fasting =
                         clean_categorical(data[[variable]], variable, c(0,1), data$id_study),
                       fasting_time =
                         clean_continuous(data[[variable]], variable, 6, 24, data$id_study),

                       fgl    = clean_continuous(data[[variable]], variable, 2, 30, data$id_study),
                       ppg    = clean_continuous(data[[variable]], variable, 2, 30, data$id_study),
                       hba1c  = clean_continuous(data[[variable]], variable, 3, 18, data$id_study),

                       drug_hyper = ,
                       drug_presc = ,
                       drug_bp = ,
                       self_hyper_12mos = ,
                       self_hyper_preg = ,
                       self_hyper =
                         clean_categorical(data[[variable]], variable, c(0,1), data$id_study),

                       sbp1 = , sbp2 = , sbp3 = , sbp4 = , sbp5 = , sbp6 = , sbp7 = ,
                       sbp8 = , sbp9 = , sbp10 = , sbp11 = , sbp12 = , sbp13 = ,
                       sbp_avg =
                         clean_continuous(data[[variable]], variable, 70, 270, data$id_study),

                       dbp1 = , dbp2 = , dbp3 = , dbp4 = , dbp5 = , dbp6 = , dbp7 = ,
                       dbp8 = , dbp9 = , dbp10 = , dbp11 = , dbp12 = , dbp13 = ,
                       dbp_avg =
                         clean_continuous(data[[variable]], variable, 30, 150, data$id_study),

                       drug_chol = ,
                       drug_chol_stat = ,
                       drug_chol_fibr = ,
                       self_chol =
                         clean_categorical(data[[variable]], variable, c(0,1), data$id_study),

                       tc  = clean_continuous(data[[variable]], variable, 1.75, 20, data$id_study),
                       ldl = clean_continuous(data[[variable]], variable,  0.5, 10, data$id_study),
                       hdl = clean_continuous(data[[variable]], variable,  0.4,  5, data$id_study),
                       trg = clean_continuous(data[[variable]], variable,  0.2, 20, data$id_study),

                       { # for any variable not defined above
                         stop(paste0('Variable "', variable, '" is not expected: please check variable name and see documentation for currently supported variables."'))
                       }
  )
  return(clean_list)
}

# print messages for percent cleaned (not by study)
print_simple_message <- function(var_name, clean_list, var) {
  cat(paste0('Cleaning variable "', var_name, '":\n'))
  cat(paste0('  ', length(clean_list), ' of ', sum(!is.na(var)), ' removed\n'))
}

# print messages for percent cleaned by study
print_message <- function(var_name, clean_list, var, id_study, message = NULL) {
  if (is.null(message)) message <- '' else paste0(' ', message)
  cat(paste0('Cleaning variable "', var_name, message, '":\n'))
  if (length(clean_list)==0) {
    cat("  No records cleaned\n")
  } else {
    cat(paste("Percentage Cleaned (No. of cleaned/No. of non-NAs):", var_name, "(%)\n"))
    cln.table <- table(id_study[clean_list])/table(id_study[!is.na(var)])*100
    cat(sort(round(cln.table[which(cln.table!=Inf&cln.table>0)],2), decreasing=TRUE))
    cat("\n\n")
  }
}

# individual cleaning functions for each variable

clean_height <- function(height, var_name, age, id_study) {
  clean_list <- c()
  # Clean height according to age group #
  clean_list <- c(clean_list, which((age >= 5 & age < 10) & (height < 60 | height > 180)))
  clean_list <- c(clean_list, which((age >= 10 & age < 15) & (height < 80 | height > 200)))
  clean_list <- c(clean_list, which((age >= 15) & (height < 100 | height > 250)))
  clean_list <- unique(clean_list)
  print_message(var_name, clean_list, height, id_study)
  return(clean_list)
}

clean_weight <- function(weight, var_name, age, is_pregnant, id_study) {
  clean_list <- c()
  # Clean weight according to age group #
  clean_list <- c(clean_list, which((age >= 5 & age < 10) & (weight < 5 | weight > 90)))
  clean_list <- c(clean_list, which((age >= 10 & age < 15) & (weight < 8 | weight > 150)))
  clean_list <- c(clean_list, which((age >= 15) & (weight < 12 | weight > 300)))
  clean_list <- c(clean_list, which(is_pregnant == 1 & !is.na(weight)))
  clean_list <- unique(clean_list)
  print_message(var_name, clean_list, weight, id_study)
  return(clean_list)
}

clean_bmi <- function(bmi, var_name, age, is_pregnant, id_study) {
  clean_list <- c()
  # Clean bmi according to age group #
  clean_list <- c(clean_list, which((age >= 5 & age < 10) & (bmi < 6 | bmi > 40)))
  clean_list <- c(clean_list, which((age >= 10 & age < 15) & (bmi < 8 | bmi > 60)))
  clean_list <- c(clean_list, which((age >= 15) & (bmi < 10 | bmi > 80)))
  clean_list <- c(clean_list, which(is_pregnant == 1 & !is.na(bmi)))
  clean_list <- unique(clean_list)
  print_message(var_name, clean_list, bmi, id_study)
  return(clean_list)
}

clean_waist <- function(waist, var_name, age, is_pregnant, id_study) {
  clean_list <- c()
  # Clean waist according to age group #
  clean_list <- c(clean_list, which((age >= 5 & age < 10) & (waist < 20 | waist > 150)))
  clean_list <- c(clean_list, which((age >= 10 & age < 15) & (waist < 20 | waist > 200)))
  clean_list <- c(clean_list, which((age >= 15) & (waist < 30 | waist > 300)))
  clean_list <- c(clean_list, which(is_pregnant == 1 & !is.na(waist)))
  clean_list <- unique(clean_list)
  print_message(var_name, clean_list, waist, id_study)
  return(clean_list)
}

clean_wth <- function(wth, age, is_pregnant, id_study) {
  clean_list <- c()
  # Clean wth according to age group #
  clean_list <- c(clean_list, which((age >= 5 & age < 15) & (wth < 0.2 | wth > 1.5)))
  clean_list <- c(clean_list, which((age >= 15) & (wth < 0.2 | wth > 2.0)))
  clean_list <- c(clean_list, which(is_pregnant == 1 & !is.na(wth)))
  clean_list <- unique(clean_list)
  print_message('waist-to-height ratio', clean_list, wth, id_study)
  return(clean_list)
}


clean_hip <- function(hip, var_name, age, is_pregnant, id_study) {
  clean_list <- c()
  # Clean hip according to age group #
  clean_list <- c(clean_list, which((age >= 5 & age < 10) & (hip < 30 | hip > 180)))
  clean_list <- c(clean_list, which((age >= 10 & age < 15) & (hip < 30 | hip > 200)))
  clean_list <- c(clean_list, which((age >= 15) & (hip < 40 | hip > 300)))
  clean_list <- c(clean_list, which(is_pregnant == 1 & !is.na(hip)))
  clean_list <- unique(clean_list)
  print_message(var_name, clean_list, hip, id_study)
  return(clean_list)
}


clean_whr <- function(whr, age, is_pregnant, id_study) {
  clean_list <- c()
  # Clean whr according to age group #
  clean_list <- c(clean_list, which((age >= 5 & age < 15) & (whr < 0.4 | whr > 1.8)))
  clean_list <- c(clean_list, which((age >= 15) & (whr < 0.4 | whr > 2.0)))
  clean_list <- c(clean_list, which(is_pregnant == 1 & !is.na(whr)))
  clean_list <- unique(clean_list)
  print_message('waist-to-hip ratio', clean_list, whr, id_study)
  return(clean_list)
}

clean_age <- function(age, id_study) {
  clean_list <- which(age < 0 | age > 120)
  print_message('age', clean_list, age, id_study)
  return(clean_list)
}

#' Cleaning a continuous variable
#'
#' @param var a vector to be cleaned
#' @param var_name the name of the variable being cleaned
#' @param minv minimum plausible value
#' @param maxv maximum plausible value
#' @seealso [clean_data()]
#' @examples
#' fgl_clean <- clean_continuous(data[[variable]], variable, 2, 30)
#' @export
clean_continuous <- function(var, var_name, minv, maxv, id_study) {
  clean_list <- which(var < minv | var > maxv)
  print_message(var_name, clean_list, var, id_study)
  return(clean_list)
}

#' Cleaning a categorical variable
#'
#' @param var a vector to be cleaned
#' @param var_name the name of the variable being cleaned
#' @param values a vector of values that are valid for thie categorical value, e.g., c(0,1)
#' @seealso [clean_data()]
#' @export
clean_categorical <- function(var, var_name, values, id_study) {
  clean_list <- which(!var %in% values & !is.na(var))
  print_message(var_name, clean_list, var, id_study)
  return(clean_list)
}

clean_preg <- function(var, var_name, age, sex, id_study) {
    clean_list1 <- which(var != 0 & var != 1)
    # print(paste("Number of", var_name, "data recoded as NA:", length(clean_list1), "of", sum(!is.na(var))))
    print_message(var_name, clean_list1, var, id_study)

    sex <- ifelse(sex == 'male', 1, ifelse(sex == 'female', 2, sex))
    clean_list2 <- which(var == 1 & sex == 1)
    clean_list3 <- which(var == 1 & (sex == 2 & (age <10 | age >=50)))
    clean_list <- union(clean_list2, clean_list3)
    print_message(var_name, clean_list1, var, id_study, 'for implausible sex or age during pregnancy')
    # print(paste("Number of ", var_name, "data recoded as NA for implausible sex or age:", length(clean_list), "of", sum(!is.na(var))))

    clean_list <- union(clean_list, clean_list1)
    return(clean_list)
}

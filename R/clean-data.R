
#' General cleaning function
#' @description Cleans a specific variable in the data frame. For fgl and ppg, the v
#' @param data The data frame to be cleaned.
#' @param variable The name string of the variable to be cleaned. Currently supported variables:
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
#'
#' @return returns a cleaned variable
#' @seealso [clean_data_index()], [clean_categorical()], [clean_continuous()], [convert_unit()]
#' @examples
#' data$age_clean <- clean_data(data, 'age');
#' data$bmi_clean <- clean_data(data, 'bmi');
#' data$fgl_clean <- clean_data(data, 'fgl');
#' @export
clean_data <- function(data, variable) {

  # convert unit
  # convert whole-blood to plasma equivalent for FPG (and exclude for PPG)
  data <- convert_unit(data, variable)

  # clean data
  clean_index <- clean_data_index(data, variable)

  v <- data[[variable]]
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
                       sex    = clean_categorical(data[[variable]], variable, c(1,2)),
                       age    = clean_age(data$age),

                       height1 = ,
                       height2 = ,
                       height3 = ,
                       height =
                         clean_height(data[[variable]], variable, data$age),
                       weight1 = ,
                       weight2 = ,
                       weight3 = ,
                       weight =
                         clean_weight(data[[variable]], variable, data$age, data$is_pregnant),
                       bmi =
                         clean_bmi(data[[variable]], variable, data$age, data$is_pregnant),

                       waist1 = ,
                       waist2 = ,
                       waist3 = ,
                       waist =
                         clean_waist(data[[variable]], variable, data$age, data$is_pregnant),
                       hip1 = ,
                       hip2 = ,
                       hip3 = ,
                       hip =
                         clean_hip(data[[variable]], variable, data$age, data$is_pregnant),

                       whtr = clean_wth(data[[variable]], data$age, data$is_pregnant),
                       whr  = clean_whr(data[[variable]], data$age, data$is_pregnant),

                       is_pregnant = , is_pregnant_exam =
                           clean_preg(data[[variable]], variable, data$age, data$sex),
                       is_urban = ,
                       is_plasma = , is_plasma_ppg =
                         clean_categorical(data[[variable]], variable, c(0,1)),

                       drug_diab = ,
                       drug_diab_insu = ,
                       drug_diab_pill = ,
                       self_diab =
                         clean_categorical(data[[variable]], variable, c(0,1)),

                       fgl    = clean_continuous(data[[variable]], variable, 2, 30),
                       ppg    = clean_continuous(data[[variable]], variable, 2, 30),
                       hba1c  = clean_continuous(data[[variable]], variable, 3, 18),

                       drug_hyper = ,
                       drug_presc = ,
                       drug_bp = ,
                       self_hyper_12mos = ,
                       self_hyper_preg = ,
                       self_hyper =
                         clean_categorical(data[[variable]], variable, c(0,1)),

                       sbp1 = , sbp2 = , sbp3 = , sbp4 = , sbp5 = , sbp6 = , sbp7 = ,
                       sbp8 = , sbp9 = , sbp10 = , sbp11 = , sbp12 = , sbp13 = ,
                       sbp_avg =
                         clean_continuous(data[[variable]], variable, 70, 270),

                       dbp1 = , dbp2 = , dbp3 = , dbp4 = , dbp5 = , dbp6 = , dbp7 = ,
                       dbp8 = , dbp9 = , dbp10 = , dbp11 = , dbp12 = , dbp13 = ,
                       dbp_avg =
                         clean_continuous(data[[variable]], variable, 30, 150),

                       drug_chol = ,
                       drug_chol_stat = ,
                       drug_chol_fibr = ,
                       self_chol =
                         clean_categorical(data[[variable]], variable, c(0,1)),

                       tc  = clean_continuous(data[[variable]], variable, 1.75, 20),
                       ldl = clean_continuous(data[[variable]], variable,  0.5, 10),
                       hdl = clean_continuous(data[[variable]], variable,  0.4,  5),
                       trg = clean_continuous(data[[variable]], variable,  0.2, 20),

                       { # for any variable not defined above
                         stop(paste0('Variable "', variable, '" is not expected: please check variable name and see documentation for currently supported variables."'))
                       }
  )
  return(clean_list)
}

#' Convert variable units
#' @description Converts values of variables based on standard units
#' @param data The data frame to be cleaned.
#' @param variable A name string of the variable to be converted. If `unit == "EXClUDE"`, all values are set to NA. Currently supported variables and units are:
#' * height, heightX, wasit, wasitX, hip, hipX: "inch", "m", "mm" -> "cm"
#' * weight, weightX: "pound", "lbs" -> "kg"
#' * fgl: "mg/dL", "mg%" -> "mmol/L"; also converts to plasma equivalent if `is_plasma == 0`
#' * ppg: "mg/dL", "mg%" -> "mmol/L"; also sets ppg values to NA if `is_plasma_ppg == 0`
#' * hba1c: "mmol/mol" -> "%"
#' * tc, ldl, hdl, trg: "mg/dL", "mg%" -> "mmol/L"
#' @param user_conversion_func A converstion function, if not differnt to the above
#'   For example: to convert g to kg, set `user_conversion_func = function(x) x * 0.001`
#' @return A variable with converted values based on standard unit
#' @export
convert_unit <- function(data, variable, user_conversion_func = NULL) {
    # convert unit
    unit_var <- dplyr::case_when(
        variable == 'fgl' ~ 'unit_gl',
        variable %in% c(
            'ppg','hba1c','tc','hdl','ldl','trg'
        ) ~ paste0('unit_', variable),
        grepl('height', variable) ~ 'unit_height',
        grepl('weight', variable) ~ 'unit_weight',
        grepl('waist', variable) ~ 'unit_waist',
        grepl('hip', variable) ~ 'unit_hip',
        grepl('sbp|dbp', variable) ~ 'unit_bp',
        TRUE ~ NA
    )
    if (!is.na(unit_var)) {
        if (!unit_var %in% names(data)) stop(paste(unit_var, 'is not available in the data frame. Please add.'))

        # conversion functions
        func_NA <- function(x) NA
        func_m2cm <- function(x) x * 100 # metre
        func_mm2cm <- function(x) x * 0.1 # millimetre
        func_inch2cm <- function(x) x * 2.54     # inch
        func_lbs2kg <- function(x) x * 0.453592 # pound
        func_gl <- function(x) x * 0.0556   # glucose
        func_hba1c <- function(x) x * 0.0915 + 2.15  # HbA1c
        func_chol <- function(x) x * 0.0259   # cholesterol
        func_trg <- function(x) x * 0.0113   # triglycerides

        if (is.null(user_conversion_func)) {
            conversion_func <- switch(
                unit_var,
                unit_height = , unit_waist = , unit_hip =
                    list('inch' = func_inch2cm, 'cm' = identity, 'm' = func_m2cm, 'mm' = func_mm2cm, 'EXCLUDE' = func_NA),
                unit_weight =
                    list('pound' = func_lbs2kg, 'lbs' = func_lbs2kg, 'kg' = identity, 'EXCLUDE' = func_NA),
                unit_gl = , unit_ppg =
                    list('mg/dL' = func_gl, 'mg/dl' = func_gl, 'mg%' = func_gl, 'mmol/L' = identity, 'mmol/l' = identity, 'EXCLUDE' = func_NA),
                unit_hba1c =
                    list('mmol/mol' = func_hba1c, '%' = identity, 'EXCLUDE' = func_NA),
                unit_tc = , unit_ldl = , unit_hdl =
                    list('mg/dL' = func_chol, 'mg/dl' = func_chol, 'mg%' = func_chol, 'mmol/L' = identity, 'mmol/l' = identity, 'EXCLUDE' = func_NA),
                unit_trg =
                    list('mg/dL' = func_trg, 'mg/dl' = func_trg, 'mg%' = func_trg, 'mmol/L' = identity, 'mmol/l' = identity, 'EXCLUDE' = func_NA),
                list()
            )
        }

        if (length(conversion_func) > 0) {
            unique_units <- setdiff(unique(data[[unit_var]]), c(NA))
            tmp <- setdiff(unique_units, names(conversion_func))
            if (length(tmp)>0) stop(paste(unit_var, 'has unsupported unit(s):', paste(tmp, collapse = ', '), '\n  Change to a supported unit or supply `user_conversion_func`. Run : ?convert_unit'))

            for (u in unique_units) {
                func <- conversion_func[[u]]
                if (!identical(func, identity)) {
                    list <- which(data[[unit_var]] == u)
                    if (length(list) > 0) {
                        n_study <- length(unique(data$id_study[list]))
                        data[[variable]][list] <- func(data[[variable]][list])
                        switch(u,
                               EXCLUDE =
                                   message(paste("Number of", variable, "data excluded:", length(list), "rows in", n_study, "studies")),
                               message(paste("Number of", variable, "data in", u, "converted:", length(list), "rows in", n_study, "studies"))
                        )
            }}}
        } else {
            stop(paste(unit_var, ' has unsupported values. Please change to a supported unit (check [convert_unit()]) or supply `user_conversion_func`'))
        }

        # conversion for is_plasma for glucose
        if (unit_var == 'unit_gl') {
            list <- which(data$is_plasma == 0)
            if (length(list)>0) {
                n_study <- length(unique(data$id_study[list]))
                data[[variable]][list] <- data[[variable]][list] * 1.066 + 0.102
                message(paste("Number of fgl data converted from whole-blood values:", length(list), "rows in", n_study, "studies"))
            }
        }
        if (unit_var == 'unit_ppg' & 'is_plasma_ppg' %in% names(data)) {
            list <- which(data$is_plasma_ppg == 0)
            if (length(list)>0) {
                n_study <- length(unique(data$id_study[list]))
                data[[variable]][list] <- NA
                message(paste("Number of ppg data in whole-blood values excluded:", length(list), "rows in", n_study, "studies"))
            }
        }
    }

    return(data)
}


# print messages
print_message <- function(var_name, clean_list, var) {
  message(paste0('Cleaning variable "', var_name, '":'))
  cat(paste0('  ', length(clean_list), ' of ', sum(!is.na(var)), ' removed\n'))
}


# individual cleaning functions for each variable

clean_height <- function(height, var_name, age) {
  clean_list <- c()
  # Clean height according to age group #
  clean_list <- c(clean_list, which((age >= 5 & age < 10) & (height < 60 | height > 180)))
  clean_list <- c(clean_list, which((age >= 10 & age < 15) & (height < 80 | height > 200)))
  clean_list <- c(clean_list, which((age >= 15) & (height < 100 | height > 250)))
  clean_list <- unique(clean_list)
  print_message(var_name, clean_list, height)
  return(clean_list)
}

clean_weight <- function(weight, var_name, age, is_pregnant) {
  clean_list <- c()
  # Clean weight according to age group #
  clean_list <- c(clean_list, which((age >= 5 & age < 10) & (weight < 5 | weight > 90)))
  clean_list <- c(clean_list, which((age >= 10 & age < 15) & (weight < 8 | weight > 150)))
  clean_list <- c(clean_list, which((age >= 15) & (weight < 12 | weight > 300)))
  clean_list <- c(clean_list, which(is_pregnant == 1 & !is.na(weight)))
  clean_list <- unique(clean_list)
  print_message(var_name, clean_list, weight)
  return(clean_list)
}

clean_bmi <- function(bmi, var_name, age, is_pregnant) {
  clean_list <- c()
  # Clean bmi according to age group #
  clean_list <- c(clean_list, which((age >= 5 & age < 10) & (bmi < 6 | bmi > 40)))
  clean_list <- c(clean_list, which((age >= 10 & age < 15) & (bmi < 8 | bmi > 60)))
  clean_list <- c(clean_list, which((age >= 15) & (bmi < 10 | bmi > 80)))
  clean_list <- c(clean_list, which(is_pregnant == 1 & !is.na(bmi)))
  clean_list <- unique(clean_list)
  print_message(var_name, clean_list, bmi)
  return(clean_list)
}

clean_waist <- function(waist, var_name, age, is_pregnant) {
  clean_list <- c()
  # Clean waist according to age group #
  clean_list <- c(clean_list, which((age >= 5 & age < 10) & (waist < 20 | waist > 150)))
  clean_list <- c(clean_list, which((age >= 10 & age < 15) & (waist < 20 | waist > 200)))
  clean_list <- c(clean_list, which((age >= 15) & (waist < 30 | waist > 300)))
  clean_list <- c(clean_list, which(is_pregnant == 1 & !is.na(waist)))
  clean_list <- unique(clean_list)
  print_message(var_name, clean_list, waist)
  return(clean_list)
}

clean_wth <- function(wth, age, is_pregnant) {
  clean_list <- c()
  # Clean wth according to age group #
  clean_list <- c(clean_list, which((age >= 5 & age < 15) & (wth < 0.2 | wth > 1.5)))
  clean_list <- c(clean_list, which((age >= 15) & (wth < 0.2 | wth > 2.0)))
  clean_list <- c(clean_list, which(is_pregnant == 1 & !is.na(wth)))
  clean_list <- unique(clean_list)
  print_message('waist-to-height ratio', clean_list, wth)
  return(clean_list)
}


clean_hip <- function(hip, var_name, age, is_pregnant) {
  clean_list <- c()
  # Clean hip according to age group #
  clean_list <- c(clean_list, which((age >= 5 & age < 10) & (hip < 30 | hip > 180)))
  clean_list <- c(clean_list, which((age >= 10 & age < 15) & (hip < 30 | hip > 200)))
  clean_list <- c(clean_list, which((age >= 15) & (hip < 40 | hip > 300)))
  clean_list <- c(clean_list, which(is_pregnant == 1 & !is.na(hip)))
  clean_list <- unique(clean_list)
  print_message(var_name, clean_list, hip)
  return(clean_list)
}


clean_whr <- function(whr, age, is_pregnant) {
  clean_list <- c()
  # Clean whr according to age group #
  clean_list <- c(clean_list, which((age >= 5 & age < 15) & (whr < 0.4 | whr > 1.8)))
  clean_list <- c(clean_list, which((age >= 15) & (whr < 0.4 | whr > 2.0)))
  clean_list <- c(clean_list, which(is_pregnant == 1 & !is.na(whr)))
  clean_list <- unique(clean_list)
  print_message('waist-to-hip ratio', clean_list, whr)
  return(clean_list)
}

clean_age <- function(age) {
  clean_list <- which(age < 0 | age > 120)
  print_message('age', clean_list, age)
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
clean_continuous <- function(var, var_name, minv, maxv) {
  clean_list <- which(var < minv | var > maxv)
  print_message(var_name, clean_list, var)
  return(clean_list)
}

#' Cleaning a categorical variable
#'
#' @param var a vector to be cleaned
#' @param var_name the name of the variable being cleaned
#' @param values a vector of values that are valid for thie categorical value, e.g., c(0,1)
#' @seealso [clean_data()]
#' @export
clean_categorical <- function(var, var_name, values) {
  clean_list <- which(!var %in% values)
  print_message(var_name, clean_list, var)
  return(clean_list)
}

clean_preg <- function(var, var_name, age, sex) {
    clean_list1 <- which(var != 0 & var != 1)
    print(paste("Number of", var_name, "data recoded as NA:", length(clean_list1), "of", sum(!is.na(var))))

    sex <- ifelse(sex == 'male', 1, ifelse(sex == 'female', 2, sex))
    clean_list2 <- which(var == 1 & sex == 1)
    clean_list3 <- which(var == 1 & (sex == 2 & (age <10 | age >=50)))
    clean_list <- union(clean_list2, clean_list3)
    print(paste("Number of ", var_name, "data recoded as NA for implausible sex or age:", length(clean_list), "of", sum(!is.na(var))))

    clean_list <- union(clean_list, clean_list1)
    return(clean_list)
}


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
          list('inch' = func_inch2cm,
               'cm' = identity,
               'm' = func_m2cm,
               'mm' = func_mm2cm,
               'EXCLUDE' = func_NA),
        unit_weight =
          list('pound' = func_lbs2kg,
               'lbs' = func_lbs2kg,
               'kg' = identity,
               'EXCLUDE' = func_NA),
        unit_gl = , unit_ppg =
          list('mg/dL' = func_gl,
               'mg/dl' = func_gl,
               'mg%' = func_gl,
               'mmol/L' = identity,
               'mmol/l' = identity,
               'EXCLUDE' = func_NA),
        unit_hba1c =
          list('mmol/mol' = func_hba1c,
               '%' = identity,
               'EXCLUDE' = func_NA),
        unit_tc = , unit_ldl = , unit_hdl =
          list('mg/dL' = func_chol,
               'mg/dl' = func_chol,
               'mg%' = func_chol,
               'mmol/L' = identity,
               'mmol/l' = identity,
               'EXCLUDE' = func_NA),
        unit_trg =
          list('mg/dL' = func_trg,
               'mg/dl' = func_trg,
               'mg%' = func_trg,
               'mmol/L' = identity,
               'mmol/l' = identity,
               'EXCLUDE' = func_NA),
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


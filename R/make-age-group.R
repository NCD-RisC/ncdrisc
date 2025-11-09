
#' Generate age group for each individual accounting for the age range of study
#'
#' @param age A vector of number.
#' @param age_design_min A vector of number.
#' @param age_design_max A vector of number.
#' @return The 10-year age group and corresponding mean age of \code{age} accounting for \code{age_design_min} and \code{age_design_max}.
#' @examples
#' make_age_group(29, 18, 200)
#' make_age_group(c(29,32,50,80,12,18,80), c(18,18,55,55,10,10,18), c(200,35,80,80,200,200,200))
#' make_age_group(c(29,32,50,80,12,18,80), c(18,18,55,55,10,10,18), c(200,35,80,80,200,200,200), anthro = TRUE)
#' make_age_group_anthro(c(29,32,50,80,12,18,80), c(18,18,55,55,10,10,18), c(200,35,80,80,200,200,200))
#' @seealso [make_age_group_anthro()]
#'
#' data$age_min <- with(data, ifelse(sex=="male", age_min_anthro_M, age_min_anthro_F)) # use appropriate variables depending on purpose
#' data$age_max <- with(data, ifelse(sex=="male", age_max_anthro_M, age_max_anthro_F))
#' ages <- make_age_groups(data$age, data$age_min, data$age_max)
#' data$age_mean <- ages$age_mean
#' data$age_group <- ages$age_group
#'
#' @export
make_age_group <- function(age, age_design_min, age_design_max, anthro = FALSE) {

  # check dimension
  if ((length(age_design_min) != 1 & length(age_design_min) != length(age)) | (length(age_design_max) != 1 & length(age_design_max) != length(age))) {
    stop('Check dimension of age_design_min, age_design_max, age: the first two should either have length of 1 or the same length as age')
  }
  # check age range values
  if (any(is.na(age_design_min)) | any(is.na(age_design_max))) {
    stop('NA values not allowed in age_design_min and age_design_max')
  }

  if (length(age_design_min) == 1) age_design_min <- rep(age_design_min, length(age))
  if (length(age_design_max) == 1) age_design_max <- rep(age_design_max, length(age))

  # mean age for open age groups
  # pre-determined using world life table (calculated by Mariachiara Di Cesare)
  mean_age_open_age_group <-
    data.frame(
      min_age = 65:100,
      mean_age = c(
        73.81, 74.52, 75.23, 75.95, 76.67,
        77.40, 78.14, 78.88, 79.62, 80.37, 81.11, 81.86, 82.61, 83.37, 84.14,
        84.91, 85.69, 86.48, 87.27, 88.07, 88.88, 89.69, 90.50, 91.33, 92.15,
        92.98, 93.81, 94.64, 95.47, 96.30, 97.12, 97.94, 98.75, 99.52, 100.15, 100.50
      )
    )

  # naive grouping by 18-19 then 10 year intervals till 80+
  # for 18+; adolescent ages are dealt with later: they are NA if present
  age_group0 <- findInterval(age, c(18, seq(20, 80, by = 10), 200), right = FALSE)  # findInterval is faster than cut; returns numerics starting 0
  age_min <- c(0, 18, seq(20, 80, by = 10))[age_group0 + 1]
  age_max <- c(18, seq(20, 80, by = 10), 201)[age_group0 + 1] -1

  # update the first and last age group according to age range
  age_min_list <- which(age_min < age_design_min)
  age_min[age_min_list] <- age_design_min[age_min_list]
  age_max_list <- which(age_max > age_design_max)
  age_max[age_max_list] <- age_design_max[age_max_list]

  # generate age group and mean age
  age_group <- stringi::stri_c(age_min, "-", age_max)  # stri_c is faster than paste0

  # calculate mean age naively
  age_mean <- (age_min + age_max + 1) / 2

  # use pre-determined mean age for for open-ended age groups
  age_mean_list1 <- which(age_max == 200)
  age_mean[age_mean_list1] <- mean_age_open_age_group$mean_age[match(age_min[age_mean_list1], mean_age_open_age_group$min_age)]
  # single year age groups: use nominal age rather than xx.5 as mid-age
  age_mean_list2 <- which(age_max - age_min == 0)
  age_mean[age_mean_list2] <- age_min[age_mean_list2]

  # deal with adolescents who should have single year age groups
  # for <20 for anthro; for <18 for other RFs (TBC as of October 2021)
  adolescent_list <- which(age < ifelse(anthro, 20, 18))
  if (length(adolescent_list) > 0) {
    f_age <- floor(age[adolescent_list])
    age_min[adolescent_list] <- f_age
    age_max[adolescent_list] <- f_age
    age_group[adolescent_list] <- stringi::stri_c(f_age, "-", f_age)
    age_mean[adolescent_list] <- floor(age[adolescent_list])
  }

  age_group <- gsub('-200', '+', age_group)

  return(data.frame(age_mean, age_group, agemin = age_min, agemax = age_max))
}

#' Generate age group for anthro data
#' @seealso [make_age_group()]
#' @export
make_age_group_anthro <- function(...) {
  return(make_age_group(..., anthro = TRUE))
}


#' Clean age based on age range
#'
#' @param age A vector of number.
#' @param id_study A vector of id_study for message purpose.
#' @param age_design_min A vector of number.
#' @param age_design_max A vector of number.
#' @return A cleaned \code{age} vector accounting for \code{age_design_min} and \code{age_design_max}.
#'
#' @example
#' data$age_min <- with(data, ifelse(sex=="male", age_min_anthro_M, age_min_anthro_F)) # use appropriate variables depending on purpose
#' data$age_max <- with(data, ifelse(sex=="male", age_max_anthro_M, age_max_anthro_F))
#' age_clean <- clean_age_range(data$age, data$age_min, data$age_max)
#'
#' @export
clean_age_range <- function(age, id_study, age_design_min, age_design_max) {

  # check dimension
  if ((length(age_design_min) != 1 & length(age_design_min) != length(age)) | (length(age_design_max) != 1 & length(age_design_max) != length(age))) {
    stop('Check dimension of age_design_min, age_design_max, age: the first two should either have length of 1 or the same length as age')
  }
  # check age range values
  if (any(is.na(age_design_min)) | any(is.na(age_design_max))) {
    stop('NA values not allowed in age_design_min and age_design_max')
  }

  if (length(age_design_min) == 1) age_design_min <- rep(age_design_min, length(age))
  if (length(age_design_max) == 1) age_design_max <- rep(age_design_max, length(age))

  # set age to NA for those outside of min/max
  clean_list <- which((age > age_design_max) | (age < age_design_min))
  print_message('age', clean_list, age, id_study, message = 'according to design age range')

  age[clean_list] <- NA
  return(age)
}

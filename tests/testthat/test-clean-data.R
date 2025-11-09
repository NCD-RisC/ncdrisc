
test_that("clean_data preserve number of rows", {
  expect_equal(length(clean_data(example_data, 'height')), nrow(example_data))

})

test_that("convert_unit function works", {
  expect_equal(abs(mean(convert_unit(example_data, 'ldl')$ldl, na.rm=TRUE) - 2.767491) < 1e-4, TRUE)

})

test_that("age-specific anthro cleaning works", {
  wgt <- clean_data(example_data, 'weight')
  expect_equal(sum(example_data$age>=5 & example_data$age<10 & (wgt<5 | wgt>90), na.rm=TRUE), 0)

})

test_that("pre-cleaning works", {
  xx <- example_data
  xx$height[1] <- 999
  xx$height[2] <- -99
  xx2 <- clean_data(xx, 'height', known_values_for_missing_data = c(999,-99))
  expect_equal(sum(xx2 %in% c(999)), 0)

})

test_that("categorical cleaning works", {
  sex <- clean_data(example_data, 'sex')
  expect_equal(as.vector(table(sex, exclude=NULL)), c(4557,4697))

  cat1 <- clean_data(example_data, 'self_diab')
  expect_equal(as.vector(table(cat1, exclude=NULL)), c(8000,893,361))
})


test_that("age range cleaning works", {
  agemin <- ifelse(example_data$sex == 1, example_data$age_min_anthro_M, example_data$age_min_anthro_F)
  agemax <- ifelse(example_data$sex == 1, example_data$age_max_anthro_M, example_data$age_max_anthro_F)

  age_cleaned1 <- clean_age_range(example_data$age, example_data$id_study, agemin, agemax)

  age_cleaned2 <- example_data$age
  age_cleaned2[which(age_cleaned2 < agemin | age_cleaned2 > agemax)] <- NA

  expect_equal(identical(age_cleaned1, age_cleaned2), TRUE)

})

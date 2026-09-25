

test_that("age group function works", {
  agemin <- ifelse(example_data$sex == 1, example_data$age_min_anthro_M, example_data$age_min_anthro_F)
  agemax <- ifelse(example_data$sex == 1, example_data$age_max_anthro_M, example_data$age_max_anthro_F)

  age <- clean_age_range(example_data$age, example_data$id_study, agemin, agemax)

  age_groups1 <- make_age_group(age, agemin, agemax)
  age_groups2 <- make_age_group_anthro(age, agemin, agemax)

  expect_equal(age_groups1, age_group_output1)
  expect_equal(age_groups2, age_group_output2)

})

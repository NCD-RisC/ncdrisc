
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

test_that("Bootstrap return error with incorrect input", {
  expect_error(Bootstrap("string"))
})

test_that("Print Bootstrap with no error", {
  expect_no_error(print(Bootstrap(rnorm(100), statistic = mean)))
})

test_that("Summary Bootstrap with incorrect input", {
  expect_condition(summary(Bootstrap("Error")))
})

test_that("Print Summary Bootstrap will return a warning from the Bootstrap function", {
  expect_warning(print(summary(Bootstrap(matrix(rnorm(200), nc = 2), statistic = var))))
})

test_that("Bootstrap return error with incorrect input", {
  expect_no_warning(plot(Bootstrap(rnorm(100), statistic = mean)))
})

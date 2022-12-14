test_that("incorrect or missing variables", {
  expect_error(DD_to_DMS(DD_input = "a", axis = "vertical"),
               'DD_input must be numeric')
  expect_error(DD_to_DMS(DD_input = c(63.5, 68.6), axis = "vertical"),
               'DD_input must be of length 1')
  expect_error(DD_to_DMS(DD_input = 63.5, axis = "other"),
               'axis must be one of horizontal or vertical')
  expect_error(DD_to_DMS(axis = "vertical"),
               'argument "DD_input" is missing, with no default')
  expect_error(DD_to_DMS(DD_input = 63.5),
               'argument "axis" is missing, with no default')
})

test_that("typical use case works", {
  expect_type(DD_to_DMS(DD_input = 63.5, axis = "vertical"), "character")
  expect_equal(DD_to_DMS(DD_input = 63.5, axis = "vertical"), "63° 30' 0\" N")
  expect_length(DD_to_DMS(DD_input = 63.5, axis = "vertical"), 1)
  expect_type(DD_to_DMS(DD_input = 178.5, axis = "horizontal"), "character")
  expect_equal(DD_to_DMS(DD_input = 178.5, axis = "horizontal"), "178° 30' 0\" E")
  expect_length(DD_to_DMS(DD_input = 178.5, axis = "horizontal"), 1)
})

test_that("edge cases", {
  expect_warning(val1 <- DD_to_DMS(DD_input = 100, axis = "vertical"))
  expect_equal(val1, NA)
  expect_warning(val2 <- DD_to_DMS(DD_input = 200, axis = "horizontal"))
  expect_equal(val2, NA)
})
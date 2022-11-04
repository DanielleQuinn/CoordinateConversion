# TODO: Need to add tests for ellipsoid constraints but don't have a clear grasp on how this can be achieved at the moment
# TODO: Consider argument to specify how easting and northing should be rounded
test_that("incorrect or missing variables", {
  expect_error(DD_to_UTM(x = c(178.5, 145.6), y = 63.5),
               'x must be of length 1')
  expect_error(DD_to_UTM(x = 178.5, y = c(63.5, 45.5)),
               'y must be of length 1')
  expect_error(DD_to_UTM(x = "a", y = 63.5),
               'x must be numeric')
  expect_error(DD_to_UTM(x = 178.5, y = "b"),
               'y must be numeric')
  expect_error(DD_to_UTM(x = 200, y = 63.5),
               "x must be between -180 and 180")
  expect_error(DD_to_UTM(x = 178.5, y =  100),
               "y must be between -90 and 90")
  expect_error(DD_to_UTM(y = 63.5),
               'argument "x" is missing, with no default')
  expect_error(DD_to_UTM(x = 178.5),
               'argument "y" is missing, with no default')
  expect_error(DD_to_UTM(x = 178.5, y = 63.5, return = "none"),
               "return must be one of all, easting, northing, zone, or hemisphere")
})

test_that("typical use case works", {
  expect_length(suppressWarnings(DD_to_UTM(x = 178.5, y = 63.5, return = "easting")),
                1)
  expect_length(suppressWarnings(DD_to_UTM(x = 178.5, y = 63.5, return = "northing")),
                1)
  expect_length(suppressWarnings(DD_to_UTM(x = 178.5, y = 63.5, return = "zone")),
                1)
  expect_length(suppressWarnings(DD_to_UTM(x = 178.5, y = 63.5, return = "hemisphere")),
                1)
  expect_length(suppressWarnings(DD_to_UTM(x = 178.5, y = 63.5)),
                4)
  expect_equal(round(suppressWarnings(DD_to_UTM(x = 178.5, y = 63.5, return = "easting")), 0),
               574671)
  expect_equal(round(suppressWarnings(DD_to_UTM(x = 178.5, y = 63.5, return = "northing")), 0),
               7042175)
  expect_equal(suppressWarnings(DD_to_UTM(x = 178.5, y = 63.5, return = "zone")),
               60)
  expect_equal(suppressWarnings(DD_to_UTM(x = 178.5, y = 63.5, return = "hemisphere")),
               "N")
  expect_equal(suppressWarnings(DD_to_UTM(x = 178.5, y = 63.5)),
               list(easting = "574671.296523511",
                    northing = "7042174.54794636",
                    zone ="60",
                    hemisphere = "N"))
})
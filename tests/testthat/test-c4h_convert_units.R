make_test_s2dv_cube <- function(nvar = 1) {
  # Dimensions
  lon <- seq(-80, -70, length.out = 10)
  lat <- seq(-10,   0,  length.out = 10)

  dims <- c(
    dataset   = 1,
    var       = nvar,
    sdate     = 1,
    time      = 1,
    ensemble  = 1,
    latitude  = length(lat),
    longitude = length(lon)
  )

  # Data: simple gradient, easy to see thresholding
  data_array <- array(
    seq(280, 300, length.out = prod(dims)),
    dim = dims
  )

  if (nvar == 1) {
    varName <- "t2m"
    units <- c(t2m = "K")
  } else if (nvar == 2) {
    varName <- c("t2m", "d2m")
    units <- c(t2m = "K", d2m = "K")
  } else if (nvar == 3) {
    varName <- c("t2m", "d2m", "u10")
    units <- c(t2m = "K", d2m = "K", u10 = "m s-1")
  }

  cube <- list(
    data = data_array,
    dims = dims,
    coords = list(
      dataset = "test",
      var       = varName,
      sdate     = as.Date("2010-01-01"),
      time      = as.Date("2010-01-01"),
      ensemble  = 1,
      latitude  = lat,
      longitude = lon
    ),
    attrs = list(
      Variable = list(
        varName = varName,
        metadata = list(variable = varName,
                        units   = units)
      ),
      # Dates must be a matrix where rows = time, cols = sdate
      Dates = matrix(as.POSIXct("2010-01-01"), nrow = 1, ncol = 1)
    )
  )

  class(cube) <- "s2dv_cube"
  cube
}

test_that("1. Failure if data is not an s2dv_cube", {
  cube <- make_test_s2dv_cube()
  class(cube) <- "not_s2dv_cube"
  expect_error(c4h_convert_units(cube))
})

test_that("2. Failure if variable is not contained within data", {
  cube <- make_test_s2dv_cube()
  expect_error(c4h_convert_units(cube, variable = "invalid_variable"))
})

test_that("3. Failure if to is not 1 or equal to number of variables", {
  cube <- make_test_s2dv_cube()
  expect_error(c4h_convert_units(cube, variable = "t2m",
                                 to = c("celsius", "fahrenheit")))
})

test_that("3. Failure if from does not match variable units", {
  cube <- make_test_s2dv_cube()
  expect_error(c4h_convert_units(cube, variable = "t2m", from = "not_K",
                                 to = c("celsius")))
})

test_that("4. Failure if units are not convertible", {
  cube <- make_test_s2dv_cube()
  expect_error(c4h_convert_units(cube, variable = "t2m", from = "K",
                                 to = c("m s-1")))
})

test_that("5. Display current units when 'to' is NULL", {
  cube <- make_test_s2dv_cube()
  expect_message(c4h_convert_units(cube, variable = "t2m", to = NULL),
                 "The current variable units are 't2m': K.")
})

test_that("6. Converting units from K to Celsius for data with one var", {
  cube <- make_test_s2dv_cube()
  res <- c4h_convert_units(cube, variable = "t2m",
                           from = "K", to = c("celsius"))

  # Check that the unit conversion is correct
  expected_data <- cube$data - 273.15
  expect_equal(res$data, expected_data)
  # Check dimensions
  expect_equal(res$dims, cube$dims)
  # Check that the variable metadata is updated correctly
  expect_equal(res$attrs$Variable$metadata$units, c(t2m = "celsius"))
})

test_that("7. Converting units of multiple variables to one unit", {
  cube <- make_test_s2dv_cube(nvar = 2)
  res <- c4h_convert_units(cube, to = c("celsius"))

  # Check that the unit conversion is correct
  expected_data <- cube$data - 273.15
  expect_equal(res$data, expected_data)
  # Check dimensions
  expect_equal(res$dims, cube$dims)
  # Check that the variable metadata is updated correctly
  expect_equal(res$attrs$Variable$metadata$units, c(t2m = "celsius",
                                                    d2m = "celsius"))
})

test_that("8. Converting units of one variable in multivar data to one unit", {
  cube <- make_test_s2dv_cube(nvar = 2)
  res <- c4h_convert_units(cube, variable = "t2m", to = c("celsius"))

  # Check that the unit conversion is correct
  expected_data <- cube$data
  expected_data[, 1, , , , , ] <- expected_data[, 1, , , , , ] - 273.15
  expect_equal(res$data, expected_data)
  # Check dimensions
  expect_equal(res$dims, cube$dims)
  # Check that the variable metadata is updated correctly
  expect_equal(res$attrs$Variable$metadata$units, c(t2m = "celsius",
                                                    d2m = "K"))
})

test_that("9. Converting units in multivar data to different units", {
  cube <- make_test_s2dv_cube(nvar = 3)
  res <- c4h_convert_units(cube, variable = c("t2m", "d2m", "u10"),
                           to = c("celsius", "kelvin", "km/h"))

  # Check that the unit conversion is correct
  expected_data <- cube$data
  expected_data[, 1, , , , , ] <- expected_data[, 1, , , , , ] - 273.15
  expected_data[, 3, , , , , ] <- expected_data[, 3, , , , , ] * 3.6
  expect_equal(res$data, expected_data)
  # Check dimensions
  expect_equal(res$dims, cube$dims)
  # Check that the variable metadata is updated correctly
  expect_equal(res$attrs$Variable$metadata$units, c(t2m = "celsius",
                                                    d2m = "kelvin",
                                                    u10 = "km/h"))
})

test_that("10. Warning when units do not exist in metadata", {
  cube <- make_test_s2dv_cube()
  cube$attrs$Variable$metadata <- list(variable = "t2m", units = NULL)

  expect_warning(res <- c4h_convert_units(cube, variable = "t2m", from = "K",
                                          to = c("celsius")),
                 paste0("The variable 't2m' in 'data' does not have units ",
                        "metadata. Proceeding with the conversion assuming ",
                        "the specified 'from' units are correct."))

  # Check that the unit conversion is correct
  expected_data <- cube$data - 273.15
  expect_equal(res$data, expected_data)
  # Check dimensions
  expect_equal(res$dims, cube$dims)
  # Check that the variable metadata is updated correctly
  expect_equal(res$attrs$Variable$metadata$units, c(t2m = "celsius"))
})
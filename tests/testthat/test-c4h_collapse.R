make_test_s2dv_cube <- function(ensemble_size = 10) {
  # Dimensions

  dims <- c(
    dataset = 1,
    var = 1,
    sdate = 10,
    time = 12,
    ensemble = ensemble_size,
    latitude = 1,
    longitude = 1
  )

  # Data: simple gradient, easy to see thresholding
  data_array <- array(
    seq(280, 300, length.out = prod(dims)),
    dim = dims
  )
  # Randomly set ~1% of values to NA
  na_idx <- sample(prod(dims), size = floor(0.01 * prod(dims)))
  data_array[na_idx] <- NA

  # Create the dates array
  dates <- t(array(
    seq(as.POSIXct("2000-01-01"), as.POSIXct("2009-12-01"), by = "month"),
    dim = c(dims["time"], dims["sdate"])
  ))
  names(dim(dates)) <- c("sdate", "time")

  cube <- list(
    data = data_array,
    dims = dims,
    coords = list(
      dataset = "test",
      var = "t2m",
      sdate = seq_along(nrow(dates)),
      time = seq_along(ncol(dates)),
      ensemble = seq_along(dims["ensemble"]),
      latitude = 1,
      longitude = 1
    ),
    attrs = list(
      Variable = list(
        varName = "t2m",
        units = "K"
      ),
      # Dates must be a matrix where rows = time, cols = sdate
      Dates = as.POSIXct(dates)
    )
  )

  class(cube) <- "s2dv_cube"
  cube
}

test_that("1. Failure if data is not an s2dv_cube", {
  cube <- make_test_s2dv_cube()
  class(cube) <- "not_s2dv_cube"
  expect_error(c4h_collapse(cube))
})

test_that("2. Failure if dim is not a valid dimension name", {
  cube <- make_test_s2dv_cube()
  expect_error(c4h_collapse(cube, dim = "invalid_dim"))
})

test_that("3. Failure if dim is not a dimension name within data", {
  cube <- make_test_s2dv_cube()
  names(cube$dims)[5] <- "invalid_dim"
  expect_error(c4h_collapse(cube, dim = "ensemble"))
})

test_that("4. Failure if dim has length <= 1 in data", {
  cube <- make_test_s2dv_cube(ensemble_size = 1)
  expect_error(c4h_collapse(cube, dim = "ensemble"))
})

test_that("5. Failure if fun is not a valid function", {
  cube <- make_test_s2dv_cube()
  expect_error(c4h_collapse(cube, dim = "ensemble", fun = "invalid_function"))
})

test_that("6. Collapsing ensemble dimension with median and na.rm = TRUE", {
  cube <- make_test_s2dv_cube()
  res <- c4h_collapse(cube, dim = "ensemble", fun = "median")

  # Check dimensions
  expect_equal(res$dims[["ensemble"]], 1)

  # Check that the data is the median across the ensemble dimension
  expected_data <- apply(cube$data, c(1, 2, 3, 4, 6, 7), median, na.rm = TRUE)
  expected_data <- array(
    expected_data,
    dim = c(dim(expected_data)[1:4], ensemble = 1, dim(expected_data)[5:6])
  )
  expect_equal(res$data, expected_data)

  # Check that the ensemble coordinate is updated correctly
  expect_equal(res$coords[["ensemble"]], 1)
})

test_that("7. Collapsing time dimension with mean and na.rm = FALSE", {
  cube <- make_test_s2dv_cube()
  res <- c4h_collapse(cube, dim = "time", fun = "mean", na.rm = FALSE)

  # Check dimensions
  expect_equal(res$dims[["time"]], 1)

  # Check that the Dates attribute is updated correctly
  expected_dates <- cube$attrs$Dates[, 1, drop = FALSE]
  dim(expected_dates) <- c(sdate = cube$dims[["sdate"]], time = 1)
  expect_equal(res$attrs$Dates, expected_dates)

  # Check that the data is the mean across the time dimension
  expected_data <- apply(cube$data, c(1, 2, 3, 5, 6, 7), mean, na.rm = FALSE)
  expected_data <- array(
    expected_data,
    dim = c(dim(expected_data)[1:3], time = 1, dim(expected_data)[4:6])
  )
  expect_equal(res$data, expected_data)

  # Check that the time coordinate is updated correctly
  expect_equal(res$coords[["time"]], 1)
})

test_that("8. Collapsing sdate dimension with mean and na.rm = FALSE", {
  cube <- make_test_s2dv_cube()
  res <- c4h_collapse(cube, dim = "sdate", fun = "mean", na.rm = FALSE)

  # Check dimensions
  expect_equal(res$dims[["sdate"]], 1)

  # Check that the Dates attribute is updated correctly
  expected_dates <- cube$attrs$Dates[1, , drop = FALSE]
  dim(expected_dates) <- c(sdate = 1, time = cube$dims[["time"]])
  expect_equal(res$attrs$Dates, expected_dates)

  # Check that the data is the mean across the sdate dimension
  expected_data <- apply(cube$data, c(1, 2, 4, 5, 6, 7), mean, na.rm = FALSE)
  expected_data <- array(
    expected_data,
    dim = c(dim(expected_data)[1:2], sdate = 1, dim(expected_data)[3:6])
  )
  expect_equal(res$data, expected_data)

  # Check that the sdate coordinate is updated correctly
  expect_equal(res$coords[["sdate"]], 1)
})

# test_that("Collapse examples run without problems", {
#   expect_no_error({
#     # Load a forecast
#     fcst <- c4h_load(
#       system.file("extdata", "forecast", package = "clim4health"),
#       var = "t2m",
#       year = 2025,
#       month = 1,
#       ext = 'nc'
#     )
#     fcst$dims
#
#     # Ensemble median of a forecast
#     fcst_coll <- c4h_collapse(fcst, dim = "ensemble", fun = "median")
#     fcst_coll$dims
#
#     # Load an observational time-series dataset
#     stations_path <- system.file("extdata", "stations", package = "clim4health")
#     stations <- c4h_load(stations_path, var = "temp_mean", ext = "csv")
#     stations$dims
#
#     # Compute time-mean (average temp conditions)
#     stations_coll <- c4h_collapse(stations, dim = "time")
#     stations_coll$dims
#
#     # Load an observational dataset aligned to a hindcast
#     obs_path <- system.file("extdata", "reanalysis", package = "clim4health")
#     obs <- c4h_load(
#       obs_path,
#       var = "t2m",
#       ext = "nc",
#       year = 2010:2012,
#       month = 1,
#       leadtime_month = 1:3
#     )
#     obs$dims
#
#     # Compute climatology (average over years)
#     obs_coll <- c4h_collapse(obs, dim = "sdate")
#     obs_coll$dims
#   })
# })

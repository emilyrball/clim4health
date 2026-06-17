suppressWarnings({
  # Objects for testing
  library("sf")
  fcst_path <- system.file("extdata", "forecast", package = "clim4health")
  fcst <- c4h_load(fcst_path, variable = "t2m", ext = 'nc')
  fcst <- c4h_collapse(fcst, "ensemble")
  stations_path <- system.file("extdata", "stations", package = "clim4health")
  stations <- c4h_load(stations_path, var = "temp_mean", ext = "csv")
  mun_path <- system.file(
    "extdata",
    "areas",
    "munip_vallecauca.gpkg",
    package = "clim4health"
  )
  mun <- read_sf(mun_path)
  fcst_aggr <- c4h_space(fcst, mun)
})


test_that("1. failure if data format not correct", {
  expect_error(
    c4h_save(1:10),
    "Data must be an s2dv_cube"
  )
})

test_that("2. incorrect extensions", {
  expect_error(
    c4h_save(fcst, "test.txt"),
    "Invalid file extension."
  )
  expect_error(
    c4h_save(fcst, "test.gpkg"),
    "Invalid extension for gridded data."
  )
  expect_error(
    c4h_save(fcst_aggr, "test.nc"),
    "Invalid extension for areal data."
  )
  expect_error(
    c4h_save(stations, "test.gpkg"),
    "Invalid extension for point data."
  )
})

test_that("3. Write gridded data successfully", {
  out_path1 <- tempfile(fileext = ".nc")
  out_path2 <- tempfile(fileext = ".csv")
  out_path3 <- tempfile(fileext = ".tif")

  c4h_save(fcst, out_path1)
  c4h_save(fcst, out_path2)
  c4h_save(fcst, out_path3)

  # Check that file was created
  expect_true(file.exists(out_path1))
  expect_true(file.exists(out_path2))
  expect_true(file.exists(out_path3))

  # Clean up
  file.remove(out_path1)
  file.remove(out_path2)
  file.remove(out_path3)
})

test_that("4. Write areal data successfully", {
  out_path <- tempfile(fileext = ".gpkg")
  c4h_save(fcst_aggr, out_path)

  # Check that file was created
  expect_true(file.exists(out_path))

  # Clean up
  file.remove(out_path)
})

test_that("4. Write point data successfully", {
  out_path <- tempfile(fileext = ".csv")
  c4h_save(stations, out_path)

  # Check that file was created
  expect_true(file.exists(out_path))

  # Clean up
  file.remove(out_path)
})

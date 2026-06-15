#testthat::skip_on_covr()
#testthat::skip_on_cran()  # optional if they are heavy

test_that("c4h_convert: terra integration works", {
  fcst_path <- system.file("extdata", "forecast", package = "clim4health")
  fcst <- c4h_load(fcst_path, variable = "t2m", ext = "nc")

  # subsetting for speed
  fcst_small <- fcst
  fcst_small$data <- fcst$data[1, 1, 1, 1, 1, 1:5, 1:5, drop = FALSE]
  fcst_small$dims[c("dataset", "var", "sdate", "time", "ensemble")] <- 1
  fcst_small$dims[c("latitude", "longitude")] <- 5
  fcst_small$coords$latitude <- fcst$coords$latitude[1:5]
  fcst_small$coords$longitude <- fcst$coords$longitude[1:5]

  r <- c4h_convert(fcst_small, "terra")

  expect_s4_class(r, "SpatRaster")
  expect_equal(terra::nlyr(r), 1)

  # debugging
  # c4h_plot(fcst, time = 1, ensemble = 1, coordgrid=TRUE)
  # terra::plot(out[[1]], col = hcl.colors(100, "RdBu", rev = TRUE), grid=TRUE,  xlab = "Longitude",
  #             ylab = "Latitude")
})

test_that("c4h_convert: csv integration works", {
  fcst_path <- system.file("extdata", "forecast", package = "clim4health")
  fcst <- c4h_load(fcst_path, variable = "t2m", ext = "nc")

  # subsetting for speed
  fcst_small <- fcst
  fcst_small$data <- fcst$data[1, 1, 1, 1, 1, 1:3, 1:3, drop = FALSE]
  fcst_small$dims[c("dataset", "var", "sdate", "time", "ensemble")] <- 1
  fcst_small$dims[c("latitude", "longitude")] <- 3
  fcst_small$coords$latitude <- fcst$coords$latitude[1:3]
  fcst_small$coords$longitude <- fcst$coords$longitude[1:3]

  df <- c4h_convert(fcst_small, "csv")

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 9)
})

test_that("c4h_convert: polygons integration works", {
  fcst_path <- system.file("extdata", "forecast", package = "clim4health")
  fcst <- c4h_load(fcst_path, variable = "t2m", ext = "nc")

  # subsetting for speed
  fcst_small <- fcst
  fcst_small$data <- fcst$data[1, 1, 1, 1, 1, 1:3, 1:3, drop = FALSE]
  fcst_small$dims[c("dataset", "var", "sdate", "time", "ensemble")] <- 1
  fcst_small$dims[c("latitude", "longitude")] <- 3
  fcst_small$coords$latitude <- fcst$coords$latitude[1:3]
  fcst_small$coords$longitude <- fcst$coords$longitude[1:3]

  sf_obj <- c4h_convert(fcst_small, "polygons")

  expect_s3_class(sf_obj, "sf")
  expect_true("geometry" %in% names(sf_obj))
})

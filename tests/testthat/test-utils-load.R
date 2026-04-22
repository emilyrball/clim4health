#' ------------------------------------------------------------------------------
# Global Health Resilience (GHR)
# Barcelona Supercomputing Center (BSC-CNS)
#
# Script Name: test-utils-load.R
# Description: Unit test for utils-load functions.
#
# Author(s): Rebeca Nunes
# Date created: 2026-03-26
# Last updated: 2026-03-26
# Environment: local/HPC/remote (hub)
# Requirements: Please see R version and dependencies in the repo README.
#' ------------------------------------------------------------------------------

# Mock functions ----

create_nc <- function(filename,
                       lon,
                       lat,
                       time,
                       varname = "var",
                       units = "unknown") {
  
  library(ncdf4)
  file_path <- file.path(tempdir(), filename)
  
  # Generate gradient values (280 → 300) across unique (latitude, time) pairs
  values <- 280 + seq(0, 20, length.out = length(lon) * length(time))
  data <- array(
    rep(values, each = length(lat)),  # Repeat each value for longitude points
    dim = c(length(lat), length(lon), length(time))
  )
  
  # convert time to POSIXct UTC
  time <- as.POSIXct(time, tz = "UTC")
  
  # NetCDF time definition (seconds since epoch)
  time_units <- "seconds since 1970-01-01 00:00:00"
  time_vals <- as.numeric(time)
  
  # define dimensions
  dim_lon  <- ncdim_def("x", "degrees_east", vals = lon)
  dim_lat  <- ncdim_def("y", "degrees_north", vals = lat)
  dim_time <- ncdim_def("valid_time", time_units, vals = time_vals, unlim = TRUE)
  
  # variable definition
  var_def <- ncvar_def(
    name = varname,
    units = units,
    dim = list(dim_lat, dim_lon, dim_time),
    missval = NA,
    prec = "double"
  )
  
  # create file
  nc <- nc_create(file_path, vars = var_def)
  
  # write data
  ncvar_put(nc, var_def, data)
  
  # add CF-style metadata
  ncatt_put(nc, "x", "standard_name", "longitude")
  ncatt_put(nc, "y", "standard_name", "latitude")
  ncatt_put(nc, "valid_time", "standard_name", "time")

  nc_close(nc)
  return(file_path)
}

# Test cases ----

## .c4h_load_csv ----

test_that("1. Happy path .c4h_load_csv", {
  file_path <- system.file("extdata/stations/", package = "clim4health")
  file_path <- paste0(file_path, "/temp_vallecauca.csv")

  res <- .c4h_load_csv(file_list = file_path, variable = "temp_mean")
  expect_s3_class(res, "s2dv_cube")

  # Check dimensions
  expect_equal(res$dims[["dataset"]], 1)
  expect_equal(res$dims[["var"]], 1)
  expect_equal(res$dims[["sdate"]], 1)
  expect_equal(res$dims[["time"]], 1096)
  expect_equal(res$dims[["ensemble"]], 1)
  expect_equal(res$dims[["location"]], 9)
  # Preserves data value
  expect_equal(16.625, as.vector(res$data)[1])
})

test_that("2. Multiple leadtimes .c4h_load_csv", {
  file_path <- system.file("extdata/stations/", package = "clim4health")
  file_path <- paste0(file_path, "/temp_vallecauca.csv")

  res <- .c4h_load_csv(file_list = file_path, variable = "temp_mean", leadtime_month = c(2,3))
  expect_s3_class(res, "s2dv_cube")

  # Check dimensions
  expect_equal(res$dims[["dataset"]], 1)
  expect_equal(res$dims[["var"]], 1)
  expect_equal(res$dims[["sdate"]], 1096)
  expect_equal(res$dims[["time"]], 2)
  expect_equal(res$dims[["ensemble"]], 1)
  expect_equal(res$dims[["location"]], 9)
})

test_that("3. Skip header lines .c4h_load_csv", {
  tmp_file <- tempfile(fileext = ".csv")

  # Generate 5 header lines + 1 CSV header line + 1 data line
  lines <- c(
    rep("header", 5),
    paste(c("station_code", "lat", "lon", "date", "temp_mean", "temp_min", "temp_max"), collapse = ","),
    paste(c(26055100, 3.416055556, -76.6515, "2010-01-01", 16.625, 13.4, 23.4), collapse = ",")
  )

  writeLines(lines, tmp_file)
  res <- .c4h_load_csv(file_list = tmp_file, variable = "temp_mean")
  expect_s3_class(res, "s2dv_cube")
})

test_that("4. Fix dates with not exact hour and missing lat and lon", {
  tmp_file <- tempfile(fileext = ".csv")

  lines <- c(
    paste(c("station_code", "date", "temp_mean", "temp_min", "temp_max"), collapse = ","),
    paste(c(26055100, "2010-01-01 00:00:01", 16.625, 13.4, 23.4), collapse = ",")
  )

  writeLines(lines, tmp_file)
  res <- .c4h_load_csv(file_list = tmp_file, variable = "temp_mean")
  expect_s3_class(res, "s2dv_cube")
})

test_that("5. Failure one column data .c4h_load_csv", {
  tmp_file <- tempfile(fileext = ".csv")

  # Generate 11 header lines (non-CSV) and 1 data line
  lines <- c(
    rep("header", 11),  # 11 header lines (no commas)
    "data1"             # Data line (non-CSV, will be read as 1 column)
  )

  # Write to CSV (no commas; each line is 1 column)
  writeLines(lines, tmp_file)
  error_message <- "Error reading the CSV file list"
  expect_error(.c4h_load_csv(file_list = tmp_file), error_message)
})

test_that("6. Failure when variable is invalid .c4h_load_csv", {
  file_path <- system.file("extdata/stations/", package = "clim4health")
  file_path <- paste0(file_path, "/temp_vallecauca.csv")

  expect_error(.c4h_load_csv(file_path))
  expect_error(.c4h_load_csv(file_path, variable = NULL))
  expect_error(.c4h_load_csv(file_path, variable = "dummy"))
})

## .c4h_load_nc_stars ----

test_that("7. Happy path .c4h_load_nc_stars hourly observations", {
  lon <- seq(-10, 10, length.out = 5)
  lat <- seq(40, 50, length.out = 4)

  time <- seq(
    as.POSIXct("2020-01-01 00:00", tz="UTC"),
    as.POSIXct("2020-01-02 23:00", tz="UTC"),
    by = "1 hour"
  )

  nc_file <- create_nc(
    filename = "test_hourly.nc",
    lon = lon,
    lat = lat,
    time = time,
    varname = "temperature",
    units = "K"
  )

  res <- .c4h_load_nc_stars(file_list = nc_file, variable = "temperature")

  expect_s3_class(res, "s2dv_cube")

  # Check dimensions
  expect_equal(res$dims[["dataset"]], 1)
  expect_equal(res$dims[["var"]], 1)
  expect_equal(res$dims[["sdate"]], 1)
  expect_equal(res$dims[["time"]], 48)
  expect_equal(res$dims[["ensemble"]], 1)
  expect_equal(res$dims[["latitude"]], 5)
  expect_equal(res$dims[["longitude"]], 4)
  # Preserves data value
  # expect_equal(16.625, as.vector(res$data)[1])
})


test_that("8. Multiple leadtimes .c4h_load_nc_stars hourly observations", {
  lon <- seq(-10, 10, length.out = 5)
  lat <- seq(40, 50, length.out = 4)

  time <- seq(
    as.POSIXct("2020-01-01 00:00", tz="UTC"),
    as.POSIXct("2020-01-02 23:00", tz="UTC"),
    by = "1 hour"
  )

  nc_file <- create_nc(
    filename = "test_hourly.nc",
    lon = lon,
    lat = lat,
    time = time,
    varname = "temperature",
    units = "K"
  )

  res <- .c4h_load_nc_stars(file_list = nc_file, variable = "temperature", leadtime_month = 1:3)
  expect_s3_class(res, "s2dv_cube")

  # Check dimensions
  expect_equal(res$dims[["dataset"]], 1)
  expect_equal(res$dims[["var"]], 1)
  expect_equal(res$dims[["sdate"]], 8928)
  expect_equal(res$dims[["time"]], 3)
  expect_equal(res$dims[["ensemble"]], 1)
  expect_equal(res$dims[["latitude"]], 5)
  expect_equal(res$dims[["longitude"]], 4)
  # Preserves data value
  # expect_equal(16.625, as.vector(res$data)[1])
})

test_that("9. Happy path .c4h_load_nc_stars daily observations", {

  lon <- seq(-10, 10, length.out = 5)
  lat <- seq(40, 50, length.out = 4)

  time <- seq(
    as.POSIXct("2020-01-01", tz = "UTC"),
    as.POSIXct("2020-01-10", tz = "UTC"),
    by = "1 day"
  )

  nc_file <- create_nc(
    filename = "test_daily.nc",
    lon = lon,
    lat = lat,
    time = time,
    varname = "temperature",
    units = "K"
  )

  res <- .c4h_load_nc_stars(file_list = nc_file, variable = "temperature")
  expect_s3_class(res, "s2dv_cube")

  # Check dimensions
  expect_equal(res$dims[["dataset"]], 1)
  expect_equal(res$dims[["var"]], 1)
  expect_equal(res$dims[["sdate"]], 1)
  expect_equal(res$dims[["time"]], 10)
  expect_equal(res$dims[["ensemble"]], 1)
  expect_equal(res$dims[["latitude"]], 5)
  expect_equal(res$dims[["longitude"]], 4)
  # expect_equal(16.625, as.vector(res$data)[1])
})


test_that("10. Multiple leadtimes .c4h_load_nc_stars daily observations", {
  lon <- seq(-10, 10, length.out = 5)
  lat <- seq(40, 50, length.out = 4)

  time <- seq(
    as.POSIXct("2020-01-01", tz = "UTC"),
    as.POSIXct("2020-01-10", tz = "UTC"),
    by = "1 day"
  )

  nc_file <- create_nc(
    filename = "test_daily.nc",
    lon = lon,
    lat = lat,
    time = time,
    varname = "temperature",
    units = "K"
  )

  res <- .c4h_load_nc_stars(file_list = nc_file, variable = "temperature", leadtime_month = 1:3)
  expect_s3_class(res, "s2dv_cube")

  # Check dimensions
  expect_equal(res$dims[["dataset"]], 1)
  expect_equal(res$dims[["var"]], 1)
  expect_equal(res$dims[["sdate"]], 372) # why 372 and not 10
  expect_equal(res$dims[["time"]], 3)
  expect_equal(res$dims[["ensemble"]], 1)
  expect_equal(res$dims[["latitude"]], 5)
  expect_equal(res$dims[["longitude"]], 4)
  # expect_equal(16.625, as.vector(res$data)[1])
})

test_that("11. Happy path .c4h_load_nc_stars monthly observations", {
  lon <- seq(-10, 10, length.out = 5)
  lat <- seq(40, 50, length.out = 4)

  time <- seq(
    as.POSIXct("2020-01-01", tz = "UTC"),
    as.POSIXct("2020-10-01", tz = "UTC"),
    by = "1 month"
  )

  nc_file <- create_nc(
    filename = "test_monthly.nc",
    lon = lon,
    lat = lat,
    time = time,
    varname = "temperature",
    units = "K"
  )

  res <- .c4h_load_nc_stars(file_list = nc_file, variable = "temperature")
  expect_s3_class(res, "s2dv_cube")

  # Check dimensions
  expect_equal(res$dims[["dataset"]], 1)
  expect_equal(res$dims[["var"]], 1)
  expect_equal(res$dims[["sdate"]], 1)
  expect_equal(res$dims[["time"]], 10)
  expect_equal(res$dims[["ensemble"]], 1)
  expect_equal(res$dims[["latitude"]], 5)
  expect_equal(res$dims[["longitude"]], 4)

  # expect_equal(16.625, as.vector(res$data)[1])
})

test_that("12. Multiple leadtimes .c4h_load_nc_stars monthly observations", {
  lon <- seq(-10, 10, length.out = 5)
  lat <- seq(40, 50, length.out = 4)

  time <- seq(
    as.POSIXct("2020-01-01", tz = "UTC"),
    as.POSIXct("2022-10-01", tz = "UTC"),
    by = "1 month"
  )

  nc_file <- create_nc(
    filename = "test_monthly.nc",
    lon = lon,
    lat = lat,
    time = time,
    varname = "temperature",
    units = "K"
  )

  res <- .c4h_load_nc_stars(file_list = nc_file, variable = "temperature", leadtime_month = 1:3)
  expect_s3_class(res, "s2dv_cube")

  # Check dimensions
  expect_equal(res$dims[["dataset"]], 1)
  expect_equal(res$dims[["var"]], 1)
  expect_equal(res$dims[["sdate"]], 34)
  expect_equal(res$dims[["time"]], 3) # leadtimes
  expect_equal(res$dims[["ensemble"]], 1)
  expect_equal(res$dims[["latitude"]], 5)
  expect_equal(res$dims[["longitude"]], 4)

  # expect_equal(16.625, as.vector(res$data)[1])
})

test_that("13. Happy path .c4h_load_nc_stars forecast", {
  fcst_path <- system.file("extdata/forecast/", package = "clim4health")
  fcst_path <- paste0(fcst_path, "/t2m_20250101.nc")
  
  res <- .c4h_load_nc_stars(file_list = fcst_path, variable = "t2m")
  expect_s3_class(res, "s2dv_cube")
  
  # Check dimensions
  expect_equal(res$dims[["dataset"]], 1)
  expect_equal(res$dims[["var"]], 1)
  expect_equal(res$dims[["sdate"]], 1)
  expect_equal(res$dims[["time"]], 3)
  expect_equal(res$dims[["ensemble"]], 51)
  expect_equal(res$dims[["latitude"]], 5)
  expect_equal(res$dims[["longitude"]], 5)
  # Preserves data value
  # expect_equal(298.8726, as.vector(res$data)[1])
})


test_that("14. Multiple leadtimes .c4h_load_nc_stars forecast", {
  fcst_path <- system.file("extdata/forecast/", package = "clim4health")
  fcst_path <- paste0(fcst_path, "/t2m_20250101.nc")

  res <- .c4h_load_nc_stars(file_list = fcst_path, variable = "t2m", leadtime_month = c(1,2,3))
  expect_s3_class(res, "s2dv_cube")

  # Check dimensions
  expect_equal(res$dims[["dataset"]], 1)
  expect_equal(res$dims[["var"]], 1)
  expect_equal(res$dims[["sdate"]], 1)
  expect_equal(res$dims[["time"]], 3) 
  expect_equal(res$dims[["ensemble"]], 51)
  expect_equal(res$dims[["latitude"]], 5)
  expect_equal(res$dims[["longitude"]], 5)
  # Preserves data value
  # expect_equal(298.8726, as.vector(res$data)[1])
})

test_that("15. Failure when variable is invalid", {
  fcst_path <- system.file("extdata/forecast/", package = "clim4health")
  file_path <- paste0(fcst_path, "/t2m_20250101.nc")
  
  expect_error(.c4h_load_nc_stars(file_path))
  expect_error(.c4h_load_nc_stars(file_path, variable = NULL))
  expect_error(.c4h_load_nc_stars(file_path, variable = "dummy"))  
})

## Helper functions ----
test_that("get_csv_dims", {
  names <- c("y", "lat", "latitude")
  data <- data.table::fread("lat, lon, temp
                       1,2,3
                      4,5,6")
  headers <- colnames(data)
  
  out <- .get_csv_dims(headers, data, names)
  expect_equal(out$val, c(1,4))
  expect_equal(out$col, 1)
  
  names <- c("dummy")
  out <- .get_csv_dims(headers, data, names)
  expect_equal(out$val, c(1,1))
  expect_equal(out$col, NULL)
})


test_that("get_variables_nc", {
  fcst_path <- system.file("extdata/forecast/", package = "clim4health")
  fcst_path <- paste0(fcst_path, "/t2m_20250101.nc")
  
  expect_message(.get_variables_nc(fcst_path, "t2m"), "Requested variables are")
  
  expect_error(.get_variables_nc(fcst_path, variable = NULL), "Please specify the variable name")
})

test_that(".subset_file_list", {
  lon <- seq(-10, 10, length.out = 5)
  lat <- seq(40, 50, length.out = 4)
  
  time <- seq(
    as.POSIXct("2020-01-01 00:00", tz="UTC"),
    as.POSIXct("2020-01-02 23:00", tz="UTC"),
    by = "1 hour"
  )
  
  nc_file <- create_nc(
    filename = "test_hourly.nc",
    lon = lon,
    lat = lat,
    time = time,
    varname = "precipitation",
    units = "K"
  )
  
  fcst_path <- system.file("extdata/forecast/", package = "clim4health")
  fcst_path <- paste0(fcst_path, "/t2m_20250101.nc")
  
  res_precipitation <- .subset_file_list(file_list = c(nc_file, fcst_path), in_var = "precipitation")
  expect_contains(basename(res_precipitation), "test_hourly.nc")
  
  res_temperature <- .subset_file_list(file_list = c(nc_file, fcst_path), in_var = "temperature")
  expect_contains(basename(res_temperature), "t2m_20250101.nc")
  
  res <- .subset_file_list(file_list = c(nc_file, fcst_path), in_var = c("precipitation", "temperature"))
  expect_contains(basename(res), "test_hourly.nc")
  expect_contains(basename(res), "t2m_20250101.nc")
})

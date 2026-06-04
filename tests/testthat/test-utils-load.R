#' ------------------------------------------------------------------------------
# Global Health Resilience (GHR)
# Barcelona Supercomputing Center (BSC-CNS)
#
# Script Name: test-utils-load.R
# Description: Unit test for utils-load functions.
#
# Author(s): Rebeca Nunes, Emily Ball
# Date created: 2026-03-26
# Last updated: 2026-04-27
# Environment: local/HPC/remote (hub)
# Requirements: Please see R version and dependencies in the repo README.
#' ------------------------------------------------------------------------------

# Mock functions ----

create_nc <- function(
  filename,
  lon,
  lat,
  time,
  varname = "var",
  units = "unknown"
) {
  library(ncdf4)
  file_path <- file.path(tempdir(), filename)

  # Generate gradient values (280 → 300) across unique (latitude, time) pairs
  values <- 280 + seq(0, 20, length.out = length(lon) * length(time))
  data <- array(
    rep(values, each = length(lat)), # Repeat each value for longitude points
    dim = c(length(lat), length(lon), length(time))
  )

  # convert time to POSIXct UTC
  time <- as.POSIXct(time, tz = "UTC")

  # NetCDF time definition (seconds since epoch)
  time_units <- "seconds since 1970-01-01 00:00:00"
  time_vals <- as.numeric(time)

  # define dimensions
  dim_lon <- ncdim_def("x", "degrees_east", vals = lon)
  dim_lat <- ncdim_def("y", "degrees_north", vals = lat)
  dim_time <- ncdim_def(
    "valid_time",
    time_units,
    vals = time_vals,
    unlim = TRUE
  )

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

  res <- .c4h_load_csv(
    file_list = file_path,
    variable = "temp_mean",
    leadtime_month = c(2, 3)
  )
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
    paste(
      c(
        "station_code",
        "lat",
        "lon",
        "date",
        "temp_mean",
        "temp_min",
        "temp_max"
      ),
      collapse = ","
    ),
    paste(
      c(26055100, 3.416055556, -76.6515, "2010-01-01", 16.625, 13.4, 23.4),
      collapse = ","
    )
  )

  writeLines(lines, tmp_file)
  res <- .c4h_load_csv(file_list = tmp_file, variable = "temp_mean")
  expect_s3_class(res, "s2dv_cube")
})

test_that("4. Fix dates with not exact hour and missing lat and lon", {
  tmp_file <- tempfile(fileext = ".csv")

  lines <- c(
    paste(
      c("station_code", "date", "temp_mean", "temp_min", "temp_max"),
      collapse = ","
    ),
    paste(
      c(26055100, "2010-01-01 00:00:01", 16.625, 13.4, 23.4),
      collapse = ","
    )
  )

  writeLines(lines, tmp_file)
  res <- .c4h_load_csv(file_list = tmp_file, variable = "temp_mean")
  expect_s3_class(res, "s2dv_cube")
})

test_that("5. Failure one column data .c4h_load_csv", {
  tmp_file <- tempfile(fileext = ".csv")

  # Generate 11 header lines (non-CSV) and 1 data line
  lines <- c(
    rep("header", 11), # 11 header lines (no commas)
    "data1" # Data line (non-CSV, will be read as 1 column)
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
    as.POSIXct("2020-01-01 00:00", tz = "UTC"),
    as.POSIXct("2020-01-02 23:00", tz = "UTC"),
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
    as.POSIXct("2020-01-01 00:00", tz = "UTC"),
    as.POSIXct("2020-01-02 23:00", tz = "UTC"),
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

  res <- .c4h_load_nc_stars(
    file_list = nc_file,
    variable = "temperature",
    leadtime_month = 1:3
  )
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

  res <- .c4h_load_nc_stars(
    file_list = nc_file,
    variable = "temperature",
    leadtime_month = 1:3
  )
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

  res <- .c4h_load_nc_stars(
    file_list = nc_file,
    variable = "temperature",
    leadtime_month = 1:3
  )
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

  res <- .c4h_load_nc_stars(
    file_list = fcst_path,
    variable = "t2m",
    leadtime_month = c(1, 2, 3)
  )
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
  data <- data.table::fread(
    "lat, lon, temp
                       1,2,3
                      4,5,6"
  )
  headers <- colnames(data)

  out <- .get_csv_dims(headers, data, names)
  expect_equal(out$val, c(1, 4))
  expect_equal(out$col, 1)

  names <- c("dummy")
  out <- .get_csv_dims(headers, data, names)
  expect_equal(out$val, c(1, 1))
  expect_equal(out$col, NULL)
})


test_that("get_variables_nc", {
  fcst_path <- system.file("extdata/forecast/", package = "clim4health")
  fcst_path <- paste0(fcst_path, "/t2m_20250101.nc")

  expect_message(.get_variables_nc(fcst_path, "t2m"), "Requested variables are")

  expect_error(
    .get_variables_nc(fcst_path, variable = NULL),
    "Please specify the variable name"
  )
})

test_that(".subset_file_list", {
  lon <- seq(-10, 10, length.out = 5)
  lat <- seq(40, 50, length.out = 4)

  time <- seq(
    as.POSIXct("2020-01-01 00:00", tz = "UTC"),
    as.POSIXct("2020-01-02 23:00", tz = "UTC"),
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

  res_precipitation <- .subset_file_list(
    file_list = c(nc_file, fcst_path),
    in_var = "precipitation"
  )
  expect_contains(basename(res_precipitation), "test_hourly.nc")

  res_temperature <- .subset_file_list(
    file_list = c(nc_file, fcst_path),
    in_var = "temperature"
  )
  expect_contains(basename(res_temperature), "t2m_20250101.nc")

  res <- .subset_file_list(
    file_list = c(nc_file, fcst_path),
    in_var = c("precipitation", "temperature")
  )
  expect_contains(basename(res), "test_hourly.nc")
  expect_contains(basename(res), "t2m_20250101.nc")
})

# test esarchive fail conditions
test_that("16. Failure with invalid path", {
  # fail if dir does not exist
  expect_error(
    .c4h_load_esarchive(path = "invalid_path/"),
    paste0(
      "To load data from esarchive, please provide",
      " a single directory path."
    )
  )
  # fail if multiple paths provided
  fcst_path <- system.file("extdata/forecast/", package = "clim4health")
  rean_path <- system.file("extdata/reanalysis/", package = "clim4health")
  expect_error(
    .c4h_load_esarchive(path = c(fcst_path, rean_path)),
    paste0(
      "To load data from esarchive, please provide",
      " a single directory path."
    )
  )
  # fail if not monthly data
  expect_error(
    .c4h_load_esarchive(path = fcst_path),
    paste0(
      "Only able to load monthly mean data from",
      " esarchive at this time."
    )
  )
  # fail if multiple variables requested
  monthly_dir <- withr::local_tempdir(tmpdir = "monthly_mean/")
  expect_error(
    .c4h_load_esarchive(path = monthly_dir, variable = c("t2m", "prlr")),
    paste0(
      "Currently only able to load one variable at ",
      "a time from esarchive."
    )
  )
  # fail if year is not specified
  expect_error(
    .c4h_load_esarchive(path = monthly_dir, variable = "t2m"),
    paste0("Please specify the range of years to ", "load from esarchive.")
  )
  # fail if not ERA5, ERA5Land or SEAS5.1
  expect_error(
    .c4h_load_esarchive(path = monthly_dir, variable = "t2m", year = 2010:2012),
    paste0(
      "Currently, only ERA5, ERA5Land, and SEAS5.1 are ",
      "supported for loading from esarchive."
    )
  )
})

create_esarchive_recon <- function(
  path,
  lon,
  lat,
  times,
  varname = "varname",
  units = "unknown"
) {
  library(ncdf4)
  library(lubridate)

  origin <- as.POSIXct("1850-01-01", tz = "UTC")
  labs <- paste0(
    lubridate::year(times),
    sprintf("%02d", lubridate::month(times))
  )
  times <- as.numeric(difftime(times, origin, units = "hours"))

  for (t in seq_along(times)) {
    # Generate gradient values (280 → 300) across unique (latitude, time) pairs
    values <- 280 + seq(t, 20, length.out = length(lon) * 1)
    data <- array(
      rep(values, each = length(lat)), # Repeat each value for longitude points
      dim = c(length(lat), length(lon), 1)
    )

    # convert time to POSIXct UTC
    time <- as.POSIXct(times[t], tz = "UTC")
    file_path <- file.path(path, paste0("/", varname, "_", labs[t], ".nc"))

    # NetCDF time definition (seconds since epoch)
    time_units <- "hours since 1850-01-01"
    time_vals <- as.numeric(time)

    # define dimensions
    dim_lon <- ncdim_def("lon", "degrees_east", vals = lon)
    dim_lat <- ncdim_def("lat", "degrees_north", vals = lat)
    dim_time <- ncdim_def("time", time_units, vals = time_vals) #, unlim = TRUE)

    # variable definition
    var_def <- ncvar_def(
      name = varname,
      units = units,
      dim = list(dim_time, dim_lat, dim_lon),
      missval = NA,
      prec = "double"
    )

    # create file
    nc <- nc_create(file_path, vars = var_def)

    # write data
    ncvar_put(nc, var_def, data)

    # add CF-style metadata
    ncatt_put(nc, "lon", "standard_name", "longitude")
    ncatt_put(nc, "lat", "standard_name", "latitude")
    ncatt_put(nc, "time", "calendar", "gregorian")
    ncatt_put(nc, "time", "standard_name", "time")

    nc_close(nc)
  }
  return(path)
}

test_that("17. happy esarchive/recon", {
  lon <- seq(-10, 10, length.out = 5)
  lat <- seq(40, 50, length.out = 10)

  times <- seq(
    as.POSIXct("2018-01-01", tz = "UTC"),
    as.POSIXct("2020-12-01", tz = "UTC"),
    by = "1 month"
  )
  base_tmp <- withr::local_tempdir()
  nc_dir <- file.path(base_tmp, "esarchive", "recon", "era5", "monthly_mean/")
  dir.create(nc_dir, recursive = TRUE)

  nc_file <- create_esarchive_recon(
    path = nc_dir,
    lon = lon,
    lat = lat,
    times = times,
    varname = "tas",
    units = "K"
  )

  res <- .c4h_load_esarchive(
    path = nc_file,
    year = 2018:2020,
    variable = "tas",
    leadtime_month = 1:3,
    month = 1,
    lat_selection = c(42, 48),
    lon_selection = c(-8, 8)
  )
  expect_s3_class(res, "s2dv_cube")

  # Check dimensions
  expect_equal(res$dims[["dataset"]], 1)
  expect_equal(res$dims[["var"]], 1)
  expect_equal(res$dims[["sdate"]], 3)
  expect_equal(res$dims[["time"]], 3) # leadtimes
  expect_equal(res$dims[["ensemble"]], 1)

  expect_lte(max(res$coords$latitude), 48)
  expect_gte(min(res$coords$latitude), 42)
  expect_lte(max(res$coords$longitude), 8)
  expect_gte(min(res$coords$longitude), -8)

  # test load timeseries
  res <- .c4h_load_esarchive(path = nc_file, year = 2018:2020, variable = "tas")
  expect_s3_class(res, "s2dv_cube")

  # Check dimensions
  expect_equal(res$dims[["dataset"]], 1)
  expect_equal(res$dims[["var"]], 1)
  expect_equal(res$dims[["sdate"]], 1)
  expect_equal(res$dims[["time"]], 36)
  expect_equal(res$dims[["ensemble"]], 1)
  expect_equal(res$dims[["latitude"]], 10)
  expect_equal(res$dims[["longitude"]], 5)
})

create_esarchive_exp <- function(
  path,
  lon,
  lat,
  sdates,
  varname = "varname",
  units = "unknown"
) {
  library(ncdf4)
  library(lubridate)

  labs <- paste0(
    lubridate::year(sdates),
    sprintf("%02d", lubridate::month(sdates)),
    "01"
  )

  for (t in seq_along(sdates)) {
    # Generate gradient values (280 → 300) across unique
    # (latitude, time, ensemble) pairs
    values <- 280 + seq(t, 20, length.out = length(lon) * 8 * 51)
    data <- array(
      rep(values, each = length(lat)), # Repeat each value for longitude points
      dim = c(length(lat), length(lon), 8, 51)
    )

    # convert time to POSIXct UTC
    file_path <- file.path(path, paste0("/", varname, "_", labs[t], ".nc"))

    # NetCDF time definition (seconds since epoch)
    time_units <- paste0("hours since ", sdates[t], " 00:00:00")
    time_vals <- seq(
      as.POSIXct(sdates[t], tz = "UTC"),
      as.POSIXct(
        lubridate::add_with_rollback(sdates[t], months(7)),
        tz = "UTC"
      ),
      by = "1 month"
    )

    time_vals <- as.numeric(difftime(
      time_vals,
      as.POSIXct(sdates[t], tz = "UTC"),
      units = "hours"
    ))
    # define dimensions
    dim_lon <- ncdim_def("lon", "degrees_east", vals = lon)
    dim_lat <- ncdim_def("lat", "degrees_north", vals = lat)
    dim_time <- ncdim_def("time", time_units, vals = time_vals) #, unlim = TRUE)
    dim_ens <- ncdim_def("ensemble", "member", vals = 1:51)

    # variable definition
    var_def <- ncvar_def(
      name = varname,
      units = units,
      dim = list(dim_time, dim_lat, dim_lon, dim_ens),
      missval = NA,
      prec = "double"
    )

    # create file
    nc <- nc_create(file_path, vars = var_def)

    # write data
    ncvar_put(nc, var_def, data)

    # add CF-style metadata
    ncatt_put(nc, "lon", "standard_name", "longitude")
    ncatt_put(nc, "lat", "standard_name", "latitude")
    ncatt_put(nc, "time", "calendar", "proleptic_gregorian")
    ncatt_put(nc, "time", "standard_name", "time")

    nc_close(nc)
  }
  return(path)
}

test_that("17. happy esarchive/exp", {
  lon <- seq(-2, 2, length.out = 5)
  lat <- seq(4, 5, length.out = 10)

  sdates <- seq(
    as.POSIXct("2018-01-01", tz = "UTC"),
    as.POSIXct("2020-12-01", tz = "UTC"),
    by = "1 year"
  )
  base_tmp <- withr::local_tempdir()
  nc_dir <- file.path(
    base_tmp,
    "esarchive",
    "exp",
    "system51c3s",
    "monthly_mean/"
  )
  dir.create(nc_dir, recursive = TRUE)

  nc_file <- create_esarchive_exp(
    path = nc_dir,
    lon = lon,
    lat = lat,
    sdates = sdates,
    varname = "tas",
    units = "K"
  )

  res <- .c4h_load_esarchive(
    path = nc_file,
    year = 2018:2020,
    variable = "tas",
    leadtime_month = 1:3,
    month = 1
  )
  expect_s3_class(res, "s2dv_cube")

  # Check dimensions
  expect_equal(res$dims[["dataset"]], 1)
  expect_equal(res$dims[["var"]], 1)
  expect_equal(res$dims[["sdate"]], 3)
  expect_equal(res$dims[["time"]], 3) # leadtimes
  expect_equal(res$dims[["ensemble"]], 51)
  expect_equal(res$dims[["latitude"]], 10)
  expect_equal(res$dims[["longitude"]], 5)

  # test load timeseries
  res <- .c4h_load_esarchive(
    path = nc_file,
    year = 2018:2020,
    variable = "tas",
    month = 1
  )
  expect_s3_class(res, "s2dv_cube")

  # Check dimensions
  expect_equal(res$dims[["dataset"]], 1)
  expect_equal(res$dims[["var"]], 1)
  expect_equal(res$dims[["sdate"]], 3)
  expect_equal(res$dims[["time"]], 8)
  expect_equal(res$dims[["ensemble"]], 51)
  expect_equal(res$dims[["latitude"]], 10)
  expect_equal(res$dims[["longitude"]], 5)
})

#' ------------------------------------------------------------------------------
# Global Health Resilience (GHR)
# Barcelona Supercomputing Center (BSC-CNS)
#
# Script Name: test-c4h_load.R
# Description: Unit test for c4h_load functions.
#
# Author(s): Rebeca Nunes
# Date created: 2026-04-22
# Last updated: 2026-04-22
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

test_that("Failure due to invalid parameter", {
    # ext parameter
    error_message <- "file extension using the 'ext' parameter."
    expect_error(c4h_load(), error_message)
    expect_error(c4h_load(ext = "txt"), error_message)

    # bbox
    error_message <- "'bbox' must be a numeric vector of length 4"
    expect_error(c4h_load(ext = "csv", bbox = c(1,2,3)), error_message)

    # path single directory is empty
    error_message <- "No files found in the specified 'path'."
    expect_error(c4h_load(path = tempdir(), ext = "csv"), error_message)

    # path doesnt exist
    error_message <- "does not exist"
    expect_error(c4h_load(path = "dummy", ext = "csv"), error_message)

    # path is list of files, but a file doesnt exist
    tmp_file <- tempfile(fileext = ".csv")
    lines <- c(
      paste(c("station_code", "date", "temp_mean", "temp_min", "temp_max"), collapse = ","),
      paste(c(26055100, "2010-01-01 00:00:01", 16.625, 13.4, 23.4), collapse = ",")
    )
    writeLines(lines, tmp_file)

    error_message <- "do not exist: dummy, dummy1"
    expect_error(c4h_load(path = c("dummy", "dummy1"), ext = "csv"), error_message)
    error_message <- "do not exist: dummy"
    expect_error(c4h_load(path = c(tmp_file, "dummy"), ext = "csv"), error_message)
})

test_that("Success loading CSV", {
  tmp_file <- tempfile(fileext = ".csv")
  tmp_file2 <- tempfile(fileext = ".csv")
  lines <- c(
    paste(c("station_code", "date", "temp_mean", "temp_min", "temp_max"), collapse = ","),
    paste(c(26055100, "2010-01-01 00:00:01", 16.625, 13.4, 23.4), collapse = ",")
  )
  writeLines(lines, tmp_file)
  writeLines(lines, tmp_file2)

  mock_cube <- function(...) "cube_result"

  #bbox null, single file
  with_mocked_bindings(
    .c4h_load_csv = mock_cube,
    {
      out <- c4h_load(path = tmp_file, ext = ".csv")
      expect_equal(out, "cube_result")
    }
  )

  # path is single directory with files
  with_mocked_bindings(
    .c4h_load_csv = mock_cube,
    {
      out <- c4h_load(path = tempdir(), ext = "csv")
      expect_equal(out, "cube_result")
    }
  )

  # path is a list of files
  with_mocked_bindings(
    .c4h_load_csv = mock_cube,
    {
      out <- c4h_load(path = c(tmp_file, tmp_file2), ext = "csv")
      expect_equal(out, "cube_result")
    }
  )

  # bbox : c(lat1, lon1, lat2, lon2)
  with_mocked_bindings(
    .c4h_load_csv = mock_cube,
    {
      out <- c4h_load(path = tmp_file, ext = "csv", bbox = c(1,2,3,4))
      expect_equal(out, "cube_result")
    }
  )
})

test_that("Success loading NetCDF", {
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
  mock_cube <- function(...) "cube_result"
  mock_get_variables_nc <- function(...) return(NULL)
  with_mocked_bindings(
    .c4h_load_nc_stars = mock_cube,
    .get_variables_nc = mock_get_variables_nc,
    {
      out <- c4h_load(path = nc_file, ext = "nc")
      expect_equal(out, "cube_result")

      out <- c4h_load(path = tempdir(), ext = "nc")
      expect_equal(out, "cube_result")
    }
  )
})

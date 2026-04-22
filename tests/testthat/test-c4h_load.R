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

test_that("1. Failure if 'ext' is not provided", {
  expect_error(c4h_load(path = "some_path"),
               "Please specify the file extension using the 'ext' parameter.")
})

test_that("2. Failure if 'ext' is invalid", {
  expect_error(c4h_load(path = "some_path", ext = "invalid_ext"),
               paste0("Please specify a valid file extension using the 'ext'",
                      " parameter. Valid options are 'nc', 'csv', '.nc', and ",
                      "'.csv'."))
})

test_that("3. Failure if 'bbox' is invalid", {
  # bbox should have 4 values: c(ymax, xmin, ymin, xmax)
  expect_error(c4h_load(path = "some_path", ext = "nc", bbox = c(1, 2, 3)),
               paste0("Parameter 'bbox' must be a numeric vector of length 4 ",
                      "\\(c\\(lat_max, lon_min, lat_min, lon_max\\)\\)."))
  # bbox should be numeric
  expect_error(c4h_load(path = "some_path", ext = "nc",
                        bbox = c("a", "b", "c", "d")),
               paste0("Parameter 'bbox' must be a numeric vector of length 4 ",
                      "\\(c\\(lat_max, lon_min, lat_min, lon_max\\)\\)."))
})

test_that("4. Failure if path is dir but no files found with ext", {
  # create a temporary directory with no files
  temp_dir <- withr::local_tempdir()
  expect_error(c4h_load(path = temp_dir, ext = "nc"),
               "No files found in the specified 'path'.")
})

test_that("5. Failure if path is neither dir nor existing file", {
  # create a temporary directory with no files
  temp_dir <- withr::local_tempdir()
  expect_error(c4h_load(path = paste0(temp_dir, "/sub.nc"), ext = "nc"),
               "The specified 'path' does not exist.")
})

test_that("6. Failure if path specifies multiple files but one does not exist",
          {
            files <- c("file1", "file2")

            expect_error(c4h_load(path = files, ext = "nc"),
                         paste0("The following files do not exist: ",
                                paste(files[!file.exists(files)],
                                      collapse = ", ")))
          })

test_that("7. No variable specified when loading valid nc files", {
  rean_path <- system.file("extdata/reanalysis/", package = "clim4health")
  expect_error(c4h_load(path = rean_path, ext = "nc"),
               paste0("Please specify the variable name. ",
                      "Variables found in 'path': 't2m'."))
})

test_that("8. Variable specified but not found in nc files", {
  # check if path is to a file
  rean_path <- system.file("extdata/reanalysis/",
                           package = "clim4health")
  rean_path <- paste0(rean_path, "/t2m_201001.nc")
  expect_error(c4h_load(path = rean_path, ext = ".nc",
                        variable = "invalid_var"),
               paste0("No valid data found."))
})

test_that("9. Variables and years found in directory (nc)", {
  # check if path is to a file
  rean_path <- system.file("extdata/reanalysis/",
                           package = "clim4health")
  expect_no_error(c4h_load(path = rean_path, ext = "nc",
                           variable = "t2m", year = 2010))
})

test_that("10. Happy path csv", {
  # check if path is to a file
  stat_path <- system.file("extdata/stations/",
                           package = "clim4health")
  res <- c4h_load(path = stat_path, ext = "csv",
                  variable = "temp_mean", year = 2010,
                  month = 1, day = 1,
                  bbox = c(4, -78, 3.3, -76))

  expect_equal(class(res), "s2dv_cube")
  # check that expected number of locations are loaded
  # (5 stations in the specified bbox)
  expect_equal(length(res$coords$location), 5)
  expect_true(all(res$attrs$location$latitude >= 3.3))
  expect_true(all(res$attrs$location$latitude <= 4.0))
  expect_true(all(res$attrs$location$longitude >= -78))
  expect_true(all(res$attrs$location$longitude <= -76))

})

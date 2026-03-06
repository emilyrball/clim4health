test_that("multiplication works", {
  expect_equal(2 * 2, 4)
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

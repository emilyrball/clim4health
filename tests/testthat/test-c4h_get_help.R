# 0. Setup Mock Data for Catalog
# We mock .datasets_info to control exactly what the help function sees
mock_datasets_info <- list(
  "test-dataset-1" = list(
    product_type = c("reanalysis", "monthly_mean"),
    variable = c("2m_temperature", "total_precipitation")
  ),
  "test-dataset-2" = list(
    originating_centre = c("ecmwf", "meteo_france")
  )
)

testthat::local_mocked_bindings(
  .datasets_info = mock_datasets_info,
  .package = "clim4health"
)

test_that("1. c4h_get_help: Dataset-level help", {

  # Case: No dataset provided - check message and listing
  expect_message(c4h_get_help(), "Available climate datasets:")
  expect_output(c4h_get_help(), "- test-dataset-1 \\n- test-dataset-2 ")

  # Case: Incorrect dataset provided - check stop error
  expect_error(
    c4h_get_help("invalid-data"),
    "Incorrect dataset, available climate datasets are"
  )
})

test_that("2. c4h_get_help: Parameter-level help", {
  # Case: Correct dataset, missing parameter
  expect_message(c4h_get_help("test-dataset-1"), "Required parameters:")
  expect_output(c4h_get_help("test-dataset-1"), "- product_type \\n- variable ")

  # Case: Correct dataset, incorrect parameter
  expect_error(
    c4h_get_help("test-dataset-1", "wrong_param"),
    "Incorrect parameter. Required parameters for test-dataset-1"
  )
})

test_that("3. c4h_get_help: Option-level help", {
  # Case: Correct dataset and correct parameter - check printed options
  expect_output(
    c4h_get_help("test-dataset-1", "product_type"),
    "Possible options for product_type:\\n- reanalysis \\n- monthly_mean "
  )
  
  expect_output(
    c4h_get_help("test-dataset-2", "originating_centre"),
    "Possible options for originating_centre:\\n- ecmwf \\n- meteo_france "
  )
})
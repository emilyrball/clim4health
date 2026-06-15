testthat::local_mocked_bindings(
  # mock CDS auth
  wf_set_key = function(key) {
    invisible(TRUE)
  },

  # mock download request
  wf_request = function(request, path) {
    expect_true(is.list(request))
    invisible(TRUE)
  },

  .package = "ecmwfr"
)

test_that("1. Missing input parameters", {
  expect_error(
    c4h_get(
      pat = c(),
      dataset = c(),
      originating_centre = c(),
      system = c(),
      variable = c(),
      product_type = c(),
      year = c(),
      month = c(),
      leadtime_month = c(),
      bbox = c(),
      outname = c()
    ),
    "missing value where TRUE/FALSE needed"
  )

  expect_error(
    c4h_get(dataset = "reanalysis-era5-land"),
    "You are missing parameters."
  )
})

test_that("2. Invalid input parameters", {
  expect_error(
    c4h_get(dataset = "unkown-random"),
    "Dataset is not valid."
  )

  expect_error(
    c4h_get(
      dataset = "reanalysis-era5-land",
      variable = c("2m_temperature"),
      product_type = c("monthly_mean"),
      year = c(2025),
      bbox = c(33, -93, -23, -17)
    ),
    "Your value for product_type is not valid."
  )

  expect_error(
    c4h_get(
      dataset = "reanalysis-era5-land",
      variable = c("2m_temperature"),
      product_type = c("reanalysis"),
      year = c(2025),
      bbox = c()
    ),
    "Coordinates are out of bound"
  )

  # Year
  expect_error(
    c4h_get(
      dataset = "reanalysis-era5-land",
      product_type = "reanalysis",
      variable = "2m_temperature",
      year = "foo",
      bbox = c(33, -93, -23, -17)
    )
  )

  # ERA5 can be from 1940 to current year
  expect_error(
    c4h_get(
      dataset = "reanalysis-era5-single-levels",
      variable = c("2m_temperature"),
      product_type = c("reanalysis"),
      year = c(1939),
      bbox = c(33, -93, -23, -17)
    ),
    "Your value for year is not valid."
  )

  expect_error(
    c4h_get(
      dataset = "reanalysis-era5-single-levels",
      variable = c("2m_temperature"),
      product_type = c("reanalysis"),
      year = c(2100),
      bbox = c(33, -93, -23, -17)
    ),
    "Your value for year is not valid."
  )

  # ERA5Land can be from 1950 to current year
  expect_error(
    c4h_get(
      dataset = "reanalysis-era5-land",
      variable = c("2m_temperature"),
      product_type = c("reanalysis"),
      year = c(1949),
      bbox = c(33, -93, -23, -17)
    ),
    "Your value for year is not valid."
  )

  expect_error(
    c4h_get(
      dataset = "reanalysis-era5-land",
      variable = c("2m_temperature"),
      product_type = c("reanalysis"),
      year = c(2100),
      bbox = c(33, -93, -23, -17)
    ),
    "Your value for year is not valid."
  )

  # Month
  expect_error(
    c4h_get(
      dataset = "reanalysis-era5-land",
      variable = c("2m_temperature"),
      product_type = c("reanalysis"),
      year = c(2025),
      month = "dummy",
      bbox = c(33, -93, -23, -17)
    ),
    "Your value for month is not valid."
  )

  expect_error(
    c4h_get(
      dataset = "reanalysis-era5-land",
      variable = c("2m_temperature"),
      product_type = c("reanalysis"),
      year = c(2025),
      month = "0",
      bbox = c(33, -93, -23, -17)
    ),
    "Your value for month is not valid."
  )

  expect_error(
    c4h_get(
      dataset = "reanalysis-era5-land",
      variable = c("2m_temperature"),
      product_type = c("reanalysis"),
      year = c(2025),
      month = -1,
      bbox = c(33, -93, -23, -17)
    ),
    "Your value for month is not valid."
  )

  expect_error(
    c4h_get(
      dataset = "reanalysis-era5-land",
      variable = c("2m_temperature"),
      product_type = c("reanalysis"),
      year = c(2025),
      month = 13,
      bbox = c(33, -93, -23, -17)
    ),
    "Your value for month is not valid."
  )

  # Day
  expect_error(
    c4h_get(
      dataset = "reanalysis-era5-land",
      variable = c("2m_temperature"),
      product_type = c("reanalysis"),
      year = c(2025),
      month = -1,
      day = -1,
      bbox = c(33, -93, -23, -17)
    ),
    "Your value for month is not valid."
  )

  # Time
  expect_error(
    c4h_get(
      dataset = "reanalysis-era5-land",
      variable = c("2m_temperature"),
      product_type = c("reanalysis"),
      year = c(2025),
      time = -1,
      bbox = c(33, -93, -23, -17)
    ),
    "Your value for time is not valid."
  )

  expect_error(
    c4h_get(
      dataset = "reanalysis-era5-land",
      variable = c("2m_temperature"),
      product_type = c("reanalysis"),
      year = c(2025),
      time = 24,
      bbox = c(33, -93, -23, -17)
    ),
    "Your value for time is not valid."
  )

  expect_error(
    c4h_get(
      dataset = "reanalysis-era5-land",
      variable = c("2m_temperature"),
      product_type = c("reanalysis"),
      year = c(2025),
      time = "",
      bbox = c(33, -93, -23, -17)
    ),
    "Your value for time is not valid."
  )

  # This test is time sensitive
  # expect_error(
  #   c4h_get(dataset = "reanalysis-era5-land",
  #                variable = c("2m_temperature"),
  #                product_type = c("reanalysis"),
  #                year = c(2025),
  #                bbox = c(33, -93, -23, -17)),
  #   "Cannot download data for a future date."
  #   )
})

test_that("3. Valid input parameters", {
  # valid call, accept unformatted name
  expect_no_error(
    c4h_get(
      dataset = "reanalysis-era5-single-levels",
      variable = c("2m_temperature"),
      product_type = c("reanalysis"),
      year = c(2025),
      month = c(4),
      day = c(1),
      time = c(12),
      bbox = c(33, -93, -23, -17),
      outpath = "./",
      outname = "ERA5 land: April data_2025_04.nc"
    )
  )

  # monthly dataset, outname without nc
  expect_no_error(
    c4h_get(
      dataset = "seasonal-monthly-single-levels",
      originating_centre = c("ecmwf"),
      system = c("51"),
      variable = c("2m_temperature"),
      product_type = c("monthly_mean"),
      year = c(2010, 2011, 2012),
      month = c(4),
      leadtime_month = c(1, 2),
      bbox = c(33, -93, -23, -17),
      outpath = "./",
      outname = "hindcast"
    )
  )

  expect_output(
    c4h_get(
      dataset = "reanalysis-era5-single-levels",
      variable = c("2m_temperature"),
      product_type = c("reanalysis"),
      year = c(2025),
      month = c(4, 5, 6),
      day = c(1:15),
      time = c(10:14),
      bbox = c(33, -93, -23, -17),
      outpath = "./",
      outname = "test.nc"
    ),
    "test_202504.nc.*test_202505.nc.*test_202506.nc"
  )
})

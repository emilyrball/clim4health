make_test_s2dv_cube <- function() {
  # Dimensions
  lon <- seq(-80, -70, length.out = 10)
  lat <- seq(-10, 0, length.out = 10)

  dims <- c(
    dataset = 1,
    var = 1,
    sdate = 1,
    time = 1,
    ensemble = 1,
    latitude = length(lat),
    longitude = length(lon)
  )

  # Data: simple gradient, easy to see thresholding
  data_array <- array(
    seq(280, 300, length.out = prod(dims)),
    dim = dims
  )

  cube <- list(
    data = data_array,
    dims = dims,
    coords = list(
      dataset = "test",
      var = "t2m",
      sdate = as.Date("2010-01-01"),
      time = as.Date("2010-01-01"),
      ensemble = 1,
      latitude = lat,
      longitude = lon
    ),
    attrs = list(
      Variable = list(
        varName = "t2m",
        units = "K"
      ),
      # Dates must be a matrix where rows = time, cols = sdate
      Dates = matrix(as.POSIXct("2010-01-01"), nrow = 1, ncol = 1)
    )
  )

  class(cube) <- "s2dv_cube"
  cube
}

test_that("1. No thresholds returns unchanged data", {
  cube <- make_test_s2dv_cube()
  res <- c4h_index(cube)

  expect_identical(res$data, cube$data)
})

test_that("2. Valid lower threshold and return_mask = TRUE returns binary mask", {
  cube <- make_test_s2dv_cube()
  res <- c4h_index(
    cube,
    return_mask = TRUE,
    lower_threshold = 290
  )

  # As a mask, all values must be 0s or 1s
  expect_true(all(res$data %in% c(0L, 1L)))
  expect_true(any(res$data == 1L))
  expect_true(any(res$data == 0L))
})


test_that("3. Valid lower threshold and return_mask = FALSE masks values correctly", {
  cube <- make_test_s2dv_cube()
  res <- c4h_index(cube, lower_threshold = 290)

  # Check if no value below 290 is present
  expect_true(all(is.na(res$data[res$data < 290])))
  # At least one value in the result is not NA
  expect_true(any(!is.na(res$data)))
})


test_that("4. Out of bounds lower threshold masks all values", {
  cube <- make_test_s2dv_cube()
  res <- c4h_index(
    cube,
    lower_threshold = 400
  )

  expect_true(all(is.na(res$data)))
})

test_that("5. Valid upper threshold masks values above it", {
  cube <- make_test_s2dv_cube()
  res <- c4h_index(
    cube,
    upper_threshold = 290
  )

  # Check if no value above 290 is present
  expect_true(all(is.na(res$data[res$data > 290])))
  expect_true(any(!is.na(res$data)))
})

test_that("6. Combined lower and upper thresholds define a band", {
  cube <- make_test_s2dv_cube()
  res <- c4h_index(
    cube,
    lower_threshold = 285,
    upper_threshold = 295
  )

  # No value below 285 or above 295
  expect_true(all(is.na(res$data[res$data > 295])))
  expect_true(all(is.na(res$data[res$data < 285])))
  expect_true(any(!is.na(res$data)))
})

test_that("7. Closed vs open interval behaves differently", {
  cube <- make_test_s2dv_cube()
  # guaranteed existing value
  # dataset, var, sdate, time, ensemble, middle latitude (out of 10), middle longitude (out of 10)
  existing_thr <- cube$data[1, 1, 1, 1, 1, 5, 5]

  closed <- c4h_index(
    cube,
    lower_threshold = existing_thr,
    closed = TRUE
  )

  open <- c4h_index(
    cube,
    lower_threshold = existing_thr,
    closed = FALSE
  )

  # The value must exist for closed interval but not for the open interval
  expect_true(existing_thr %in% closed$data)
  expect_false(existing_thr %in% open$data)
})

test_that("8. Index metadata is updated", {
  cube <- make_test_s2dv_cube()
  res <- c4h_index(cube, lower_threshold = 290)

  expect_equal(res$attrs$index$lower_threshold, ">=290")
})

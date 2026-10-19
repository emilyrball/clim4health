make_test_s2dv_cube <- function(ensemble_size = 10, sdate_size = 10) {
  # Dimensions
  dims <- c(
    dataset = 1,
    var = 1,
    sdate = sdate_size,
    time = 12,
    ensemble = ensemble_size,
    latitude = 1,
    longitude = 1
  )

  # Data: simple gradient, easy to see thresholding
  data_array <- array(
    seq(280, 300, length.out = prod(dims)),
    dim = dims
  )
  # Set ~1% of values to NA, at fixed positions to keep tests deterministic
  na_idx <- seq(7, prod(dims), by = 100)
  data_array[na_idx] <- NA

  # Create the dates array, with dimensions (sdate, time)
  dates <- t(array(
    seq(
      as.POSIXct("2000-01-01"),
      by = "month",
      length.out = dims[["sdate"]] * dims[["time"]]
    ),
    dim = c(dims["time"], dims["sdate"])
  ))
  names(dim(dates)) <- c("sdate", "time")

  cube <- list(
    data = data_array,
    dims = dims,
    coords = list(
      dataset = "test",
      var = "t2m",
      sdate = seq_len(dims[["sdate"]]),
      time = seq_len(dims[["time"]]),
      ensemble = seq_len(dims[["ensemble"]]),
      latitude = 1,
      longitude = 1
    ),
    attrs = list(
      Variable = list(
        varName = "t2m",
        units = "K"
      ),
      Dates = as.POSIXct(dates)
    )
  )

  class(cube) <- "s2dv_cube"
  cube
}

# Reference implementation: collapse `dim` with `apply()`, keeping it with
# length one
expected_collapse <- function(cube, dim, fun, na.rm) {
  idx <- match(dim, names(cube$dims))
  out <- apply(cube$data, seq_along(cube$dims)[-idx], fun, na.rm = na.rm)
  new_dims <- cube$dims
  new_dims[idx] <- 1
  array(out, dim = new_dims)
}

# Input validation ----

test_that("1. Failure if data is not an s2dv_cube", {
  cube <- make_test_s2dv_cube()
  class(cube) <- "not_s2dv_cube"
  expect_error(
    c4h_collapse(cube, dim = "ensemble"),
    "only accepts s2dv_cube objects"
  )
})

test_that("2. Failure if dim is not a valid dimension name", {
  cube <- make_test_s2dv_cube()
  expect_error(
    c4h_collapse(cube, dim = "invalid_dim"),
    "can only collapse"
  )
})

test_that("3. Failure if dim is not a dimension name within data", {
  cube <- make_test_s2dv_cube()
  names(cube$dims)[5] <- "invalid_dim"
  names(dim(cube$data))[5] <- "invalid_dim"
  expect_error(
    c4h_collapse(cube, dim = "ensemble"),
    "doesn't have a ensemble dimension"
  )
})

test_that("4. Failure if dim has length <= 1 in data", {
  cube <- make_test_s2dv_cube(ensemble_size = 1)
  expect_error(
    c4h_collapse(cube, dim = "ensemble"),
    "must be larger than one"
  )
})

test_that("5. Failure if fun is not a valid function", {
  cube <- make_test_s2dv_cube()
  expect_error(
    c4h_collapse(cube, dim = "ensemble", fun = "invalid_function"),
    "Valid 'fun'"
  )
})

test_that("6. Failure if dim or fun are not single character strings", {
  cube <- make_test_s2dv_cube()
  expect_error(c4h_collapse(cube), "'dim' must be a single character")
  expect_error(
    c4h_collapse(cube, dim = c("ensemble", "time")),
    "'dim' must be a single character"
  )
  expect_error(c4h_collapse(cube, dim = 1), "'dim' must be a single character")
  expect_error(
    c4h_collapse(cube, dim = NA_character_),
    "'dim' must be a single character"
  )
  expect_error(
    c4h_collapse(cube, dim = "ensemble", fun = mean),
    "'fun' must be a single character"
  )
  expect_error(
    c4h_collapse(cube, dim = "ensemble", fun = c("mean", "max")),
    "'fun' must be a single character"
  )
  expect_error(
    c4h_collapse(cube, dim = "ensemble", fun = NA_character_),
    "'fun' must be a single character"
  )
})

test_that("6b. Failure if collapsing time when sdate has length > 1", {
  cube <- make_test_s2dv_cube(sdate_size = 10)
  expect_error(
    c4h_collapse(cube, dim = "time"),
    "'sdate' dimension must have length one"
  )
  # Collapsing sdate does not require time to have length one
  expect_no_error(c4h_collapse(cube, dim = "sdate"))
})

test_that("7. Failure if data array is inconsistent with dims", {
  # Different dimension sizes
  cube <- make_test_s2dv_cube()
  cube$dims[["ensemble"]] <- 5
  expect_error(c4h_collapse(cube, dim = "ensemble"), "do not match")

  # Same sizes but different dimension order
  cube <- make_test_s2dv_cube()
  cube$data <- aperm(cube$data, c(2, 1, 3:7))
  cube$dims[1:2] <- cube$dims[2:1]
  names(cube$dims)[1:2] <- c("dataset", "var")
  expect_error(c4h_collapse(cube, dim = "ensemble"), "do not match")
})

# Collapsing ----

test_that("8. Collapsing ensemble dimension with median and na.rm = TRUE", {
  cube <- make_test_s2dv_cube()
  res <- c4h_collapse(cube, dim = "ensemble", fun = "median")

  # Check dimensions
  expect_equal(res$dims[["ensemble"]], 1)
  expect_equal(dim(res$data), res$dims)

  # Check that the data is the median across the ensemble dimension
  expect_equal(
    res$data,
    expected_collapse(cube, "ensemble", median, na.rm = TRUE)
  )

  # Check that the ensemble coordinate is updated correctly
  expect_equal(res$coords[["ensemble"]], 1)
})

test_that("9. Collapsing time dimension with mean and na.rm = FALSE", {
  cube <- make_test_s2dv_cube(sdate_size = 1)
  res <- c4h_collapse(cube, dim = "time", fun = "mean", na.rm = FALSE)

  # Check dimensions
  expect_equal(res$dims[["time"]], 1)

  # Check that the Dates attribute is updated correctly
  expected_dates <- cube$attrs$Dates[, 1, drop = FALSE]
  dim(expected_dates) <- c(sdate = cube$dims[["sdate"]], time = 1)
  expect_equal(res$attrs$Dates, expected_dates)

  # Check that the data is the mean across the time dimension
  expect_equal(res$data, expected_collapse(cube, "time", mean, na.rm = FALSE))

  # Check that the time coordinate is updated correctly
  expect_equal(res$coords[["time"]], 1)
})

test_that("10. Collapsing sdate dimension with mean and na.rm = FALSE", {
  cube <- make_test_s2dv_cube()
  res <- c4h_collapse(cube, dim = "sdate", fun = "mean", na.rm = FALSE)

  # Check dimensions
  expect_equal(res$dims[["sdate"]], 1)

  # Check that the Dates attribute is updated correctly
  expected_dates <- cube$attrs$Dates[1, , drop = FALSE]
  dim(expected_dates) <- c(sdate = 1, time = cube$dims[["time"]])
  expect_equal(res$attrs$Dates, expected_dates)

  # Check that the data is the mean across the sdate dimension
  expect_equal(res$data, expected_collapse(cube, "sdate", mean, na.rm = FALSE))

  # Check that the sdate coordinate is updated correctly
  expect_equal(res$coords[["sdate"]], 1)
})

test_that("11. All supported functions work for every dimension", {
  for (dim in c("ensemble", "time", "sdate")) {
    # 'time' can only be collapsed when 'sdate' has length one
    cube <- make_test_s2dv_cube(sdate_size = if (dim == "time") 1 else 10)
    for (fun in c("mean", "median", "min", "max")) {
      res <- c4h_collapse(cube, dim = dim, fun = fun, na.rm = TRUE)
      expect_equal(
        res$data,
        expected_collapse(cube, dim, fun, na.rm = TRUE),
        info = paste(dim, fun)
      )
    }
  }
})

test_that("12. na.rm = FALSE propagates NA, na.rm = TRUE ignores it", {
  cube <- make_test_s2dv_cube()
  # Only one missing member in a single cell
  cube$data[] <- 290
  cube$data[1, 1, 1, 1, 3, 1, 1] <- NA

  res_keep <- c4h_collapse(cube, dim = "ensemble", na.rm = FALSE)
  res_drop <- c4h_collapse(cube, dim = "ensemble", na.rm = TRUE)

  expect_true(is.na(res_keep$data[1, 1, 1, 1, 1, 1, 1]))
  expect_equal(sum(is.na(res_keep$data)), 1)
  expect_false(anyNA(res_drop$data))
  expect_true(all(res_drop$data == 290))
})

test_that("13. All-NA cells return NA for every function with na.rm = TRUE", {
  for (fun in c("mean", "median", "min", "max")) {
    cube <- make_test_s2dv_cube()
    # Make all ensemble members missing for one cell and valid for another
    cube$data[1, 1, 1, 1, , 1, 1] <- NA
    cube$data[1, 1, 2, 1, , 1, 1] <- 290

    expect_no_warning(
      res <- c4h_collapse(cube, dim = "ensemble", fun = fun, na.rm = TRUE)
    )
    expect_true(is.na(res$data[1, 1, 1, 1, 1, 1, 1]), info = fun)
    expect_false(is.nan(res$data[1, 1, 1, 1, 1, 1, 1]), info = fun)
    expect_equal(res$data[1, 1, 2, 1, 1, 1, 1], 290, info = fun)
    # No infinite values are introduced
    expect_false(any(is.infinite(res$data)), info = fun)
  }
})

test_that("14. Default arguments compute the mean with na.rm = TRUE", {
  cube <- make_test_s2dv_cube()
  expect_equal(
    c4h_collapse(cube, dim = "ensemble"),
    c4h_collapse(cube, dim = "ensemble", fun = "mean", na.rm = TRUE)
  )
})

test_that("15. Dimension order of the data does not matter", {
  cube <- make_test_s2dv_cube()
  res <- c4h_collapse(cube, dim = "ensemble")

  # Move the ensemble dimension to the front
  perm <- c(5, 1:4, 6:7)
  cube_perm <- cube
  cube_perm$data <- aperm(cube$data, perm)
  cube_perm$dims <- cube$dims[perm]
  res_perm <- c4h_collapse(cube_perm, dim = "ensemble")

  expect_equal(names(res_perm$dims), names(cube_perm$dims))
  expect_equal(aperm(res_perm$data, order(perm)), res$data)
})

test_that("16. Unnamed data array dimensions are named from dims", {
  cube <- make_test_s2dv_cube()
  expected <- c4h_collapse(cube, dim = "ensemble")
  dim(cube$data) <- unname(dim(cube$data))
  res <- c4h_collapse(cube, dim = "ensemble")
  expect_equal(res$data, expected$data)
  expect_equal(names(dim(res$data)), names(cube$dims))
})

test_that("17. Cubes with few dimensions can be collapsed", {
  cube <- make_test_s2dv_cube()
  cube$data <- array(seq_len(12), dim = c(ensemble = 4, time = 3))
  cube$dims <- c(ensemble = 4, time = 3)
  cube$coords <- list(ensemble = 1:4, time = 1:3)

  res <- c4h_collapse(cube, dim = "ensemble", fun = "max")
  expect_equal(res$dims, c(ensemble = 1, time = 3))
  expect_equal(dim(res$data), c(ensemble = 1, time = 3))
  expect_equal(as.vector(res$data), c(4, 8, 12))
})

# Metadata ----

test_that("18. The input cube is not modified", {
  for (dim in c("ensemble", "time", "sdate")) {
    cube <- make_test_s2dv_cube(sdate_size = if (dim == "time") 1 else 10)
    cube_orig <- cube
    c4h_collapse(cube, dim = dim)
    expect_identical(cube, cube_orig, info = dim)
  }
})

test_that("19. The result is an s2dv_cube with the expected elements", {
  cube <- make_test_s2dv_cube()
  res <- c4h_collapse(cube, dim = "ensemble")
  expect_s3_class(res, "s2dv_cube")
  expect_named(res, names(cube))
  expect_equal(names(res$coords), names(cube$coords))
  expect_equal(names(res$dims), names(cube$dims))
})

test_that("20. Only the collapsed dimension is modified in dims and coords", {
  for (dim in c("ensemble", "time", "sdate")) {
    cube <- make_test_s2dv_cube(sdate_size = if (dim == "time") 1 else 10)
    res <- c4h_collapse(cube, dim = dim)
    other_dims <- setdiff(names(cube$dims), dim)
    expect_equal(res$dims[other_dims], cube$dims[other_dims], info = dim)
    expect_equal(res$coords[other_dims], cube$coords[other_dims], info = dim)
    expect_equal(res$dims[[dim]], 1, info = dim)
  }
})

test_that("21. Dates and other attributes are kept when collapsing ensemble", {
  cube <- make_test_s2dv_cube()
  res <- c4h_collapse(cube, dim = "ensemble")
  expect_identical(res$attrs, cube$attrs)
})

test_that("22. Non-Dates attributes are kept when collapsing time or sdate", {
  for (dim in c("time", "sdate")) {
    cube <- make_test_s2dv_cube(sdate_size = if (dim == "time") 1 else 10)
    res <- c4h_collapse(cube, dim = dim)
    expect_identical(res$attrs$Variable, cube$attrs$Variable, info = dim)
  }
})

test_that("23. Attributes of the collapsed coordinate are preserved", {
  cube <- make_test_s2dv_cube()
  attr(cube$coords$ensemble, "units") <- "member"
  res <- c4h_collapse(cube, dim = "ensemble")
  expect_equal(attr(res$coords$ensemble, "units"), "member")
})

test_that("24. Dates keep their class and dimension names", {
  for (dim in c("time", "sdate")) {
    cube <- make_test_s2dv_cube(sdate_size = if (dim == "time") 1 else 10)
    res <- c4h_collapse(cube, dim = dim)
    expect_s3_class(res$attrs$Dates, "POSIXct")
    expect_named(dim(res$attrs$Dates), c("sdate", "time"))
  }
})

# Helper ----

test_that("25. .apply_keepdim keeps the collapsed dimension with length 1", {
  x <- array(
    seq_len(24),
    dim = c(a = 2, b = 3, c = 4)
  )
  res <- .apply_keepdim(x, margins = c(1, 3), fun = "sum", na.rm = TRUE)
  expect_equal(dim(res), c(a = 2, b = 1, c = 4))
  expect_equal(as.vector(res), as.vector(apply(x, c(1, 3), sum)))
})

test_that("26. .apply_keepdim works when only one dimension is kept", {
  x <- array(seq_len(6), dim = c(a = 2, b = 3))
  res <- .apply_keepdim(x, margins = 1, fun = "mean", na.rm = TRUE)
  expect_equal(dim(res), c(a = 2, b = 1))
  expect_equal(as.vector(res), c(3, 4))
})

# Packaged example data ----

test_that("27. Collapsing the ensemble of the packaged forecast works", {
  path <- system.file("extdata", "forecast", package = "clim4health")
  skip_if(path == "", "Packaged example data not available")

  fcst <- suppressMessages(
    c4h_load(path, var = "t2m", year = 2025, month = 1, ext = "nc")
  )
  res <- c4h_collapse(fcst, dim = "ensemble", fun = "median")

  expect_s3_class(res, "s2dv_cube")
  expect_equal(res$dims[["ensemble"]], 1)
  expect_equal(dim(res$data), res$dims)
  expect_equal(
    res$dims[names(res$dims) != "ensemble"],
    fcst$dims[names(fcst$dims) != "ensemble"]
  )
})

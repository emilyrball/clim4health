# Gridded ----
mock_s2dv_grid <- function(
  nd = 1,
  nv = 1,
  nt = 1,
  ns = 1,
  nm = 1,
  nlat = 5,
  nlon = 5
) {
  # spatial coords
  lon <- seq(-78, -74, length.out = nlon)
  lat <- seq(6, 2, length.out = nlat) # descending latitude

  dims <- c(
    dataset = nd,
    var = nv,
    sdate = ns,
    time = nt,
    ensemble = nm,
    latitude = nlat,
    longitude = nlon
  )

  # Dates
  # Always a matrix: rows = sdate, cols = time
  Dates <- outer(
    as.Date("2025-01-01") + seq_len(ns) - 1,
    seq_len(nt) - 1,
    `+`
  )
  Dates <- as.POSIXct(Dates, origin = "1970-01-01")
  dim(Dates) <- c(sdate = ns, time = nt)

  # data
  data <- array(
    runif(prod(dims), min = -4, max = 1),
    dim = dims
  )

  # coords
  coords <- list(
    dataset = if (nd == 1) 1 else seq_len(nd),
    var = if (nv == 1) "t2m" else paste0("var", seq_len(nv)),
    sdate = seq_len(ns),
    time = seq_len(nt),
    ensemble = 0:(nm - 1), # real fcst starts at 0
    latitude = lat,
    longitude = lon
  )

  data_cube <- list(
    data = data,
    dims = dims,
    coords = coords,
    attrs = list(
      Dates = Dates,
      Variable = list(varName = coords$var[1])
    )
  )
  class(data_cube) <- "s2dv_cube"
  return(data_cube)
}

test_that("errors when exp or obs is NULL", {
  expect_error(
    c4h_postprocess(NULL, NULL, metrics = "BSS"),
    "must be specified"
  )
})

test_that("errors on invalid metrics", {
  exp <- mock_s2dv_grid()
  obs <- mock_s2dv_grid()

  expect_error(
    c4h_postprocess(exp, obs, metrics = "INVALID"),
    "metrics.*must be specified"
  )
})

test_that("errors when ncores exceeds available cores", {
  exp <- mock_s2dv_grid()
  obs <- mock_s2dv_grid()

  expect_error(
    c4h_postprocess(exp, obs, metrics = "BSS", ncores = 9999),
    "cannot be equal to or greater"
  )
})

test_that("errors when dimension names differ", {
  exp <- mock_s2dv_grid()
  obs <- mock_s2dv_grid()
  obs$dims[["extra"]] <- 1

  expect_error(
    c4h_postprocess(exp, obs, metrics = "BSS"),
    "same named dimensions"
  )
})

test_that("errors when dimension lengths differ", {
  exp <- mock_s2dv_grid()
  obs <- mock_s2dv_grid()
  obs$dims[["sdate"]] <- 5

  expect_error(
    c4h_postprocess(exp, obs, metrics = "BSS"),
    "different lengths"
  )
})

test_that("BSS metric returns expected structure", {
  exp <- mock_s2dv_grid()
  obs <- mock_s2dv_grid()

  res <- c4h_postprocess(
    exp,
    obs,
    metrics = "BSS",
    brier_thresholds = c(0.1, 0.9)
  )

  expect_true("BSS10" %in% names(res))
  expect_true("BSS90" %in% names(res))

  expect_named(res$BSS10, c("bss", "sign"))
})

test_that("errors when exp or obs is NULL", {
  expect_error(
    c4h_postprocess(NULL, NULL, metrics = "BSS"),
    "must be specified"
  )
})

test_that("errors on invalid metrics", {
  exp <- mock_s2dv_grid()
  obs <- mock_s2dv_grid()

  expect_error(
    c4h_postprocess(exp, obs, metrics = "INVALID"),
    "metrics.*must be specified"
  )
})

test_that("errors when ncores exceeds available cores", {
  exp <- mock_s2dv_grid()
  obs <- mock_s2dv_grid()

  expect_error(
    c4h_postprocess(exp, obs, metrics = "BSS", ncores = 9999),
    "cannot be equal to or greater"
  )
})

test_that("errors when dimension names differ", {
  exp <- mock_s2dv_grid()
  obs <- mock_s2dv_grid()
  obs$dims[["extra"]] <- 1

  expect_error(
    c4h_postprocess(exp, obs, metrics = "BSS"),
    "same named dimensions"
  )
})


test_that("errors when exp or obs is NULL", {
  expect_error(
    c4h_postprocess(NULL, NULL, metrics = "BSS"),
    "must be specified"
  )
})

test_that("errors on invalid metrics", {
  exp <- mock_s2dv_grid()
  obs <- mock_s2dv_grid()

  expect_error(
    c4h_postprocess(exp, obs, metrics = "INVALID"),
    "metrics.*must be specified"
  )
})

test_that("errors when ncores exceeds available cores", {
  exp <- mock_s2dv_grid()
  obs <- mock_s2dv_grid()

  expect_error(
    c4h_postprocess(exp, obs, metrics = "BSS", ncores = 9999),
    "cannot be equal to or greater"
  )
})

test_that("errors when dimension names differ", {
  exp <- mock_s2dv_grid()
  obs <- mock_s2dv_grid()
  obs$dims[["extra"]] <- 1

  expect_error(
    c4h_postprocess(exp, obs, metrics = "BSS"),
    "same named dimensions"
  )
})

test_that("errors when dimension lengths differ", {
  exp <- mock_s2dv_grid()
  obs <- mock_s2dv_grid()
  obs$dims[["sdate"]] <- 5

  expect_error(
    c4h_postprocess(exp, obs, metrics = "BSS"),
    "different lengths"
  )
})

test_that("BSS returns expected structure", {
  exp <- mock_s2dv_grid()
  obs <- mock_s2dv_grid()

  res <- c4h_postprocess(
    exp,
    obs,
    metrics = "BSS",
    brier_thresholds = c(0.3, 0.99)
  )

  expect_true("BSS30" %in% names(res))
  expect_true("BSS99" %in% names(res))

  expect_named(res$BSS30, c("bss", "sign"))
})

test_that("Default RPSS gridded", {
  exp <- mock_s2dv_grid()
  obs <- mock_s2dv_grid()

  res <- c4h_postprocess(
    exp,
    obs,
    metrics = "RPSS"
  )

  expect_true("RPSS" %in% names(res))
  expect_named(res$RPSS, c("rpss", "sign"))
})

test_that("Default CRPSS gridded", {
  exp <- mock_s2dv_grid()
  obs <- mock_s2dv_grid()

  res <- c4h_postprocess(
    exp,
    obs,
    metrics = "CRPSS"
  )

  expect_true("CRPSS" %in% names(res))
  expect_named(res$CRPSS, c("crpss", "sign"))
})

test_that("Default AbsBiasSS gridded", {
  exp <- mock_s2dv_grid()
  obs <- mock_s2dv_grid()

  res <- c4h_postprocess(
    exp,
    obs,
    metrics = "AbsBiasSS"
  )

  expect_true("AbsBiasSS" %in% names(res))
  expect_named(res$AbsBiasSS, c("biasSS", "sign"))
})

test_that("Default MSE gridded", {
  exp <- mock_s2dv_grid(ns = 4)
  obs <- mock_s2dv_grid(ns = 4)

  res <- c4h_postprocess(
    exp,
    obs,
    metrics = "MSE"
  )

  expect_true("MSE" %in% names(res))
  expect_named(res$MSE, c("mse", "conf.lower", "conf.upper"))
})

test_that("Default MSSS gridded", {
  exp <- mock_s2dv_grid(nt = 4, ns = 4)
  obs <- mock_s2dv_grid(nt = 4, ns = 4)

  res <- c4h_postprocess(
    exp,
    obs,
    ref = exp,
    metrics = "MSSS"
  )

  expect_true("MSSS" %in% names(res))
  expect_named(res$MSSS, c("msss", "p.val", "sign"))
})

test_that("Default RMSE gridded", {
  exp <- mock_s2dv_grid(nt = 4, ns = 4)
  obs <- mock_s2dv_grid(nt = 4, ns = 4)

  res <- c4h_postprocess(
    exp,
    obs,
    ref = exp,
    metrics = "RMSE"
  )

  expect_true("RMSE" %in% names(res))
  expect_named(res$RMSE, c("rmse", "conf.lower", "conf.upper"))
})

test_that("Default RMSSS gridded", {
  exp <- mock_s2dv_grid(nt = 4, ns = 4)
  obs <- mock_s2dv_grid(nt = 4, ns = 4)

  res <- c4h_postprocess(
    exp,
    obs,
    ref = exp,
    metrics = "RMSSS"
  )

  expect_true("RMSSS" %in% names(res))
  expect_named(res$RMSSS, c("rmsss", "p.val", "sign"))
})

# test_that("Default ROCSS gridded", {
#   exp <- mock_s2dv_grid(nt = 4, ns = 4)
#   obs <- mock_s2dv_grid(nt = 4, ns = 4)
#
#   res <- c4h_postprocess(
#     exp,
#     obs,
#     ref = exp,
#     metrics = "ROCSS"
#   )
#
#   expect_true("ROCSS" %in% names(res))
#   expect_named(res$ROCSS, c("rocss", "p.val", "sign"))
# })

# Areal ----

# Pending, play with the optional parameters

# Points ----

# Pending, do many metrics at once

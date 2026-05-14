make_mock_fcst_cube <- function(
  nd = 1, # datasets
  nv = 1, # variables
  ns = 1, # start dates
  nt = 1, # lead times
  nm = 1, # ensemble members
  nlat = 5,
  nlon = 5,
  start_date = as.Date("2025-01-01")
) {
  # --- spatial coords (match real fcst quirks)
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

  # --- data
  data <- array(
    runif(prod(dims), min = 298, max = 300),
    dim = dims
  )

  # --- coords
  coords <- list(
    dataset = if (nd == 1) 1 else seq_len(nd),
    var = if (nv == 1) "t2m" else paste0("var", seq_len(nv)),
    sdate = if (ns == 1) 1 else seq_len(ns),
    time = seq_len(nt),
    ensemble = 0:(nm - 1), # real fcst starts at 0
    latitude = lat,
    longitude = lon
  )

  # --- Dates
  # Always a matrix: rows = sdate, cols = time
  Dates <- outer(
    start_date + seq_len(ns) - 1,
    seq_len(nt) - 1,
    `+`
  )
  Dates <- as.POSIXct(Dates, origin = "1970-01-01")

  cube <- list(
    data = data,
    dims = dims,
    coords = coords,
    attrs = list(
      Dates = Dates,
      Variable = list(varName = coords$var[1])
    )
  )

  class(cube) <- "s2dv_cube"
  cube
}

test_that("1. failure if invalid dimension name", {
  cube <- make_mock_fcst_cube()
  expect_error(
    .subset_array_named(cube$data, invalid_dim = 1),
    "Invalid dimension name: invalid_dim"
  )
})

test_that("2. expect ensemble dimension subsetted", {
  cube <- make_mock_fcst_cube(nm = 3)
  res <- .subset_array_named(cube$data, ensemble = 2)
  expect_equal(dim(res)[["ensemble"]], 1)
  expect_equal(names(dim(res)), names(dim(cube$data)))
  expect_equal(res[,,,,,,], cube$data[,,,, 2, , ])
})

test_that("3. expect absolute values returned with correct formatting", {
  x <- c(-10, -5, 0, 5.3, 10)
  res <- .abs_labels(x)
  expect_equal(res, c("-10", "-5", "0", "5.3", "10"))
})

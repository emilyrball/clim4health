# Mock function: Gridded ----
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

# Mock functions: Area ----
mock_s2dv_area <- function(
  nd = 1,
  nv = 1,
  nt = 1,
  ns = 1,
  nm = 1,
  narea = 5
) {
  # spatial coords
  area <- seq_len(narea)

  dims <- c(
    dataset = nd,
    var = nv,
    sdate = ns,
    time = nt,
    ensemble = nm,
    area = narea
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
    area = area
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

# Input validation ----
test_that("errors if exp is not s2dv_cube", {
  expect_error(
    c4h_downscale("Interpolation", exp = list(), obs = list()),
    "Parameter 'exp' must be an s2dv_cube object loaded "
  )
})
test_that("errors if exp is not a grid or a polygon s2dv", {
  exp <- mock_s2dv_grid()
  exp$dims$latitude <- NULL
  obs <- mock_s2dv_grid()
  expect_error(
    c4h_downscale("Interpolation", exp = exp, obs = obs),
    "The 'exp' s2dv_cube object must have 'latitude'"
  )
})
test_that("errors with invalid downscale_function", {
  exp <- mock_s2dv_grid()
  obs <- mock_s2dv_grid()
  expect_error(
    c4h_downscale("INVALID", exp, obs),
    "Please specify a downscaling function"
  )
})
test_that("Intbc requires 'method_remap' for grids", {
  exp <- mock_s2dv_grid()
  obs <- mock_s2dv_grid()
  expect_error(
    c4h_downscale("Intbc", exp, obs),
    "'method_remap' must be specified"
  )
})
test_that("not 'Intbc' for areal data", {
  exp <- mock_s2dv_area()
  obs <- mock_s2dv_grid()
  expect_error(
    c4h_downscale("Interpolation", exp, obs),
    "Method 'Interpolation' is not valid for this type of data"
  )
})
test_that("NULL bc method for areal data", {
  exp <- mock_s2dv_area()
  obs <- mock_s2dv_grid()
  expect_message(
    expect_error(
      c4h_downscale("Intbc", exp, obs),
      "'bc_method' must be specified"
    ),
    "Only bias adjustment or calibration will be applied"
  )
})
test_that("invalid point locations", {
  exp <- mock_s2dv_grid()
  obs <- mock_s2dv_grid()
  points <- list("invalid")
  expect_error(
    c4h_downscale("Interpolation", exp, obs, points = points),
    "points' must be a list with two elements named 'latitude' and 'longitude'"
  )
})
test_that("point locations but missing 'method_point_interp'", {
  exp <- mock_s2dv_grid()
  obs <- mock_s2dv_grid()
  points <- list(longitude = 0, latitude = 0)
  expect_error(
    c4h_downscale("Interpolation", exp, obs, points = points),
    "'method_point_interp' must be specified"
  )
})
test_that("'Interpolation' without points or grid", {
  exp <- mock_s2dv_grid()
  obs <- mock_s2dv_grid()
  expect_error(
    c4h_downscale("Interpolation", exp, obs, method_remap = "nn"),
    "'target_grid' must be specified"
  )
})
test_that("'Downscaling to a grid requires 'obs'", {
  exp <- mock_s2dv_grid()
  expect_error(
    c4h_downscale("Analogs", exp, method_remap = "nn"),
    "If using 'Analogs', 'obs' must be specified."
  )
})
test_that("Incorrect method_remap + obs message", {
  exp <- mock_s2dv_grid(nd = 10, nv = 10, nt = 10, ns = 10, nm = 10)
  obs <- mock_s2dv_grid(nd = 10, nv = 10, nt = 10, ns = 10, nm = 10)
  expect_message(
    expect_error(
      c4h_downscale(
        "Intbc",
        exp,
        obs,
        bc_method = "bias",
        method_remap = c("con", "bil")
      ),
      "'method_remap' must be a single character string"
    ),
    "Downscaling will be done to the grid of 'obs'"
  )
  expect_error(
    c4h_downscale(
      "Intbc",
      exp,
      obs,
      bc_method = "bias",
      method_remap = "wrong"
    ),
    "Accepted values for 'method_remap' are"
  )
})
test_that("'Intbc' with missing 'bc_method'", {
  exp <- mock_s2dv_grid()
  obs <- mock_s2dv_grid()
  expect_error(
    c4h_downscale("Intbc", exp, obs, method_remap = "nn"),
    "If using 'Intbc', 'bc_method' must be specified"
  )
})
test_that("'Intlr' with missing 'lr_method'", {
  exp <- mock_s2dv_grid()
  obs <- mock_s2dv_grid()
  expect_error(
    c4h_downscale("Intlr", exp, obs, method_remap = "nn"),
    "If using 'Intlr', 'lr_method' must be specified"
  )
})

# Bilinear grid interpolation (skip CDO) ----
test_that("Bilinear grid interpolation", {
  testthat::skip_on_cran()

  exp <- mock_s2dv_grid()
  obs <- mock_s2dv_grid()

  bctest <- c4h_downscale(
    "Interpolation",
    exp,
    obs,
    target_grid = system.file(
      "extdata",
      "reanalysis/t2m_201001.nc",
      package = "clim4health"
    ),
    method_remap = "bilinear"
  )
  expect_true(all(names(bctest) %in% c("obs", "exp")))
  expect_true(inherits(bctest$exp, "s2dv_cube"))
})

# 9nn regression downscaling (skip CDO) ----
test_that("9nn regression downscaling", {
  testthat::skip_on_cran()

  exp <- mock_s2dv_grid()
  obs <- mock_s2dv_grid()
  down <- c4h_downscale(
    "Intlr",
    exp,
    obs,
    method_remap = "con",
    target_grid = system.file(
      "extdata",
      "reanalysis/t2m_201001.nc",
      package = "clim4health"
    ),
    lr_method = "9nn"
  )
  expect_true(all(names(down) %in% c("obs", "exp")))
  expect_true(inherits(down$obs, "s2dv_cube"))
})


# Calibration of areal data (skip CDO) ----
test_that("Calibrating areal data", {
  testthat::skip_on_cran()

  exp <- mock_s2dv_area(nd = 10, nv = 10, nt = 10, ns = 10, nm = 1, narea = 5)
  obs <- mock_s2dv_area(nd = 10, nv = 10, nt = 10, ns = 10, nm = 1, narea = 5)
  bctest <- c4h_downscale("Intbc", exp, obs, bc_method = "bias")
  expect_equal(names(bctest), c("obs", "exp"))
  expect_true(inherits(bctest$obs, "s2dv_cube"))
})

# Interpolation with points (skip CDO) ----
test_that("Interpolation with points", {
  testthat::skip_on_cran()

  hcst <- c4h_load(
    system.file("extdata", "hindcast", package = "clim4health"),
    variable = "t2m",
    year = 2010:2012,
    month = 1,
    leadtime_month = 1:3,
    ext = "nc"
  )
  lats <- hcst$coords$latitude
  lons <- hcst$coords$longitude
  obs <- c4h_load(
    system.file("extdata", "reanalysis", package = "clim4health"),
    variable = "t2m",
    year = 2010:2012,
    month = 1,
    leadtime_month = 1:3,
    bbox = c(max(lats), min(lons), min(lats), max(lons)),
    ext = "nc"
  )
  points <- list(longitude = -76, latitude = 4)
  dwn <- c4h_downscale(
    "Interpolation",
    exp = hcst,
    obs = obs,
    points = points,
    method_point_interp = "bilinear"
  )
  expect_true(all(names(dwn) %in% c("obs", "exp")))
  expect_true(inherits(dwn$exp, "s2dv_cube"))
})


# Intlr with bbox (skip CDO) ----
test_that("Intlr with bbox", {
  testthat::skip_on_cran()

  hcst <- c4h_load(
    system.file("extdata", "hindcast", package = "clim4health"),
    variable = "t2m",
    year = 2010:2012,
    month = 1,
    leadtime_month = 1:3,
    ext = "nc"
  )
  lats <- hcst$coords$latitude
  lons <- hcst$coords$longitude
  obs <- c4h_load(
    system.file("extdata", "reanalysis", package = "clim4health"),
    variable = "t2m",
    year = 2010:2012,
    month = 1,
    leadtime_month = 1:3,
    bbox = c(max(lats), min(lons), min(lats), max(lons)),
    ext = "nc"
  )
  dwn <- c4h_downscale(
    "Intlr",
    exp = hcst,
    obs = obs,
    method_remap = "bic",
    bc_method = "bias",
    lr_method = "basic",
    bbox = c(4, -77, 3, -76)
  )

  expect_true(all(names(dwn) %in% c("obs", "exp")))
  expect_true(inherits(dwn$exp, "s2dv_cube"))
})

# Analogs with no grid (CDO not needed) ----
test_that("Analogs with no grid", {
  hcst <- c4h_load(
    system.file("extdata", "hindcast", package = "clim4health"),
    variable = "t2m",
    year = 2010:2012,
    month = 1,
    leadtime_month = 1:3,
    ext = "nc"
  )
  lats <- hcst$coords$latitude
  lons <- hcst$coords$longitude
  obs <- c4h_load(
    system.file("extdata", "reanalysis", package = "clim4health"),
    variable = "t2m",
    year = 2010:2012,
    month = 1,
    leadtime_month = 1:3,
    bbox = c(max(lats), min(lons), min(lats), max(lons)),
    ext = "nc"
  )
  expect_message(
    {
      suppressWarnings({
        dwn <- c4h_downscale(
          "Analogs",
          exp = hcst,
          obs = obs
        )
      })
    },
    "c4h_downscale will use the grid of 'exp' as the coarse grid"
  )

  expect_true(all(names(dwn) %in% c("obs", "exp")))
  expect_true(inherits(dwn$obs, "s2dv_cube"))
})

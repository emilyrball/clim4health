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

make_mock_location_cube <- function(
  nd = 1, # datasets
  nv = 1, # variables
  ns = 1, # start dates
  nt = 1, # lead times
  nm = 1, # ensemble members
  nloc = 5,
  start_date = as.Date("2025-01-01")
) {
  # --- spatial coords (match real fcst quirks)
  lon <- seq(-7, 7, length.out = nloc)
  lat <- seq(2, -4, length.out = nloc) # descending latitude

  dims <- c(
    dataset = nd,
    var = nv,
    sdate = ns,
    time = nt,
    ensemble = nm,
    location = nloc
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
    location = seq_len(nloc) # <-- index, not lat/lon
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
      location = list(
        latitude = lat,
        longitude = lon
      ),
      Variable = list(varName = coords$var[1])
    )
  )

  class(cube) <- "s2dv_cube"
  cube
}

make_mock_area_cube <- function(
  nd = 1, # datasets
  nv = 1, # variables
  ns = 1, # start dates
  nt = 1, # lead times
  nm = 1, # ensemble members
  nloc = 2,
  start_date = as.Date("2025-01-01"),
  area_id = NULL
) {
  dims <- c(
    dataset = nd,
    var = nv,
    sdate = ns,
    time = nt,
    ensemble = nm,
    area = nloc
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
    area = seq_len(nloc) # <-- index, not lat/lon
  )

  if (!is.null(area_id)) {
    coords$area <- paste0("Area_", seq_len(nloc))
  }

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
      area = list(
        geom = NULL
      ),
      Variable = list(varName = coords$var[1])
    )
  )

  class(cube) <- "s2dv_cube"
  cube
}


make_test_areas_sf <- function(crs = 4326) {
  library(sf)
  library(terra)

  # Define extent within the cube's range (-80 to -70, -10 to 0)
  poly1 <- st_as_sfc(st_bbox(
    c(xmin = -78, xmax = -75, ymin = -8, ymax = -6),
    crs = st_crs(crs)
  ))
  poly2 <- st_as_sfc(st_bbox(
    c(xmin = -74, xmax = -72, ymin = -4, ymax = -2),
    crs = st_crs(crs)
  ))

  areas_sf <- st_sf(
    munip_code = c("Area_A", "Area_B"),
    geometry = c(poly1, poly2)
  )
  return(areas_sf)
}

test_that("1. Error if output_format is invalid", {
  cube <- make_mock_fcst_cube()
  expect_error(c4h_convert(cube, "invalid_format"),
               "Invalid output format. Choose 'terra', 'csv' or 'polygons'.")
})

test_that("2. Error if data is not an s2dv_cube", {
  not_a_cube <- list()
  expect_error(c4h_convert(not_a_cube, "terra"),
               "'data' must be an s2dv_cube.")
})

test_that("3. Error if gridded data is required but not present", {
  location_cube <- make_mock_location_cube()
  expect_error(c4h_convert(location_cube, "terra"),
               "Gridded data is required for 'terra' output format.")
})

test_that("4. Error if gridded or polygon data is required but not present", {
  location_cube <- make_mock_location_cube()
  expect_error(c4h_convert(location_cube, "polygons"),
               paste0("Gridded or polygon data is required for 'polygons'",
                      " output format."))
})

test_that("5. c4h_convert dispatches to .s2dv_to_terra", {
  cube <- make_mock_fcst_cube()
  mock_terra <- function(x, crs, drop) "terra_result"

  with_mocked_bindings(
    .s2dv_to_terra = mock_terra,
    {
      out <- c4h_convert(cube, "terra", crs = "EPSG:4326", drop = FALSE)
      expect_equal(out, "terra_result")
    }
  )
})

test_that("6. c4h_convert dispatches to .s2dv_to_csv", {
  cube <- make_mock_fcst_cube()
  mock_csv <- function(x, drop) "csv_result"


  with_mocked_bindings(
    .s2dv_to_csv = mock_csv,
    {
      out <- c4h_convert(cube, "csv", drop = FALSE)
      expect_equal(out, "csv_result")
    }
  )
})

test_that("7. c4h_convert dispatches to .s2dv_to_polygons", {
  cube <- make_mock_fcst_cube()
  mock_polygons <- function(x, crs, drop) "polygon_result"

  with_mocked_bindings(
    .s2dv_to_polygons = mock_polygons,
    {
      out <- c4h_convert(cube, "polygons", crs = "EPSG:4326", drop = FALSE)
      expect_equal(out, "polygon_result")
    }
  )
})

test_that("8. Message if 'crs' is NULL", {
  cube <- make_mock_fcst_cube()
  expect_message(c4h_convert(cube, "terra", crs = NULL),
                 paste0("'crs' is not provided. Assuming WGS 84",
                        " \\(EPSG:4326\\) for gridded data."))

  area_cube <- make_mock_area_cube()
  area_cube$attrs$area$geom <- make_test_areas_sf()$geometry
  expect_message(c4h_convert(area_cube, "polygons", crs = NULL),
                 paste0("'crs' is not provided. Taking CRS from",
                        "the input data if available."))
})

test_that("9. failure if drop is invalid", {
  cube <- make_mock_fcst_cube()
  expect_error(c4h_convert(cube, "terra", drop = "not_logical"),
               "'drop' must be a single logical value.")
  expect_error(c4h_convert(cube, "terra", drop = c(TRUE, FALSE)),
               "'drop' must be a single logical value.")
})
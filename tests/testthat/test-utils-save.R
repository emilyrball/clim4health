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

test_that("1. .save_s2dv writes NetCDF", {
  cube <- make_mock_fcst_cube()
  out_path <- tempfile(fileext = ".nc")

  expect_message(c4h_save(cube, "s2dv", out_path),
                 "Saved NetCDF: ")

  # Check that file was created
  expect_true(file.exists(out_path))

  # Clean up
  file.remove(out_path)
})

test_that("2. .save_terra writes terra", {
  cube <- terra::rast(nrow = 3, ncol = 3, vals = 1:9)
  out_path <- tempfile(fileext = ".tif")

  c4h_save(cube, "terra", out_path)

  # Check that file was created
  expect_true(file.exists(out_path))

  # Clean up
  file.remove(out_path)
})

test_that("3. .save_csv writes CSV", {
  df <- data.frame(x = 1:5, y = 6:10)
  out_path <- tempfile(fileext = ".csv")

  c4h_save(df, "csv", out_path)

  # Check that file was created
  expect_true(file.exists(out_path))

  # Clean up
  file.remove(out_path)
})

test_that("4. .save_polygons writes GeoPackage", {
  areas_sf <- make_test_areas_sf()
  out_path <- tempfile(fileext = ".gpkg")

  c4h_save(areas_sf, "polygons", out_path)

  # Check that file was created
  expect_true(file.exists(out_path))

  # Clean up
  file.remove(out_path)
})
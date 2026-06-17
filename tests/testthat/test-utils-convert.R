make_mock_fcst_cube <- function(
  nd = 1, # datasets
  nv = 1, # variables
  ns = 1, # start dates
  nt = 3, # lead times
  nm = 51, # ensemble members
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
  nt = 3, # lead times
  nm = 51, # ensemble members
  nloc = 5,
  start_date = as.Date("2025-01-01")
) {
  # --- spatial coords (match real fcst quirks)
  lon <- seq(-78, -74, length.out = nloc)
  lat <- seq(6, 2, length.out = nloc) # descending latitude

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
  nt = 3, # lead times
  nm = 51, # ensemble members
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

# --- Happy path tests ----
test_that("1. Happy path .s2dv_to_terra", {
  cube <- make_mock_fcst_cube()

  r <- .s2dv_to_terra(cube)

  # Returns SpatRaster
  expect_s4_class(r, "SpatRaster")
  # Produces correct spatial extent
  expect_equal(terra::xmin(r), min(cube$coords$longitude), tolerance = 0.5)
  expect_equal(terra::xmax(r), max(cube$coords$longitude), tolerance = 0.5)
  expect_equal(terra::ymin(r), min(cube$coords$latitude), tolerance = 0.5)
  expect_equal(terra::ymax(r), max(cube$coords$latitude), tolerance = 0.5)
  # Creates expected number of layers
  expected_layers <- 1 * 1 * 1 * 3 * 51
  expect_equal(terra::nlyr(r), expected_layers)
  # Names layers correctly
  expect_true(
    all(
      c(
        "dataset_1_var_t2m_sdate_20250101_time_20250101_ensemble_0",
        "dataset_1_var_t2m_sdate_20250101_time_20250102_ensemble_0",
        "dataset_1_var_t2m_sdate_20250101_time_20250103_ensemble_0",
        "dataset_1_var_t2m_sdate_20250101_time_20250101_ensemble_50",
        "dataset_1_var_t2m_sdate_20250101_time_20250102_ensemble_50",
        "dataset_1_var_t2m_sdate_20250101_time_20250103_ensemble_50"
      ) %in%
        names(r)
    )
  )
  # Preserves data value
  expect_equal(
    as.vector(terra::values(r[[1]])),
    as.vector(cube$data[1, 1, 1, 1, 1, , ]),
    tolerance = 1e-2
  )
})

test_that("2 Happy path .s2dv_to_df", {
  cube <- make_mock_fcst_cube()

  df <- .s2dv_to_df(cube)

  # Returns data.frame
  expect_s3_class(df, "data.frame")
  # Correct number of rows
  expected_rows <- 1 * 1 * 1 * 3 * 51 * 5 * 5
  expect_equal(nrow(df), expected_rows)
  # Contains expected columns
  expect_true(all(
    c(
      "dataset",
      "var",
      "sdate",
      "time",
      "ensemble",
      "latitude",
      "longitude",
      "value",
      "date",
      "var_name",
      "ensemble_value"
    ) %in%
      names(df)
  ))
  # Data preserved
  expect_equal(
    df$value[1],
    as.vector(cube$data)[1],
    tolerance = 1e-6
  )
})

test_that("3 Happy path .s2dv_to_sf", {
  cube <- make_mock_fcst_cube(ns = 1, nt = 2, nm = 2)

  sf_obj <- .s2dv_to_sf(cube)

  # Returns sf object
  expect_s3_class(sf_obj, "sf")
  # Columns include at least the first layer value
  expect_true(
    "dataset_1_var_t2m_sdate_20250101_time_20250101_ensemble_0" %in%
      names(sf_obj)
  )
  # Geometry exists
  expect_true("geometry" %in% names(sf_obj))
})

# --- Failures (validation) ----
test_that("4. Validation fails when cube is invalid", {
  # Not a cube
  expect_error(.validate_s2dv_cube(42))

  # Missing data
  cube <- make_mock_fcst_cube()
  cube$data <- NULL
  error_message <- "`data\\$data` must be a multidimensional array"
  expect_error(.validate_s2dv_cube(cube), error_message)

  # Missing dims
  cube <- make_mock_fcst_cube()
  cube$dims <- cube$dims[!names(cube$dims) %in% c("dataset", "var", "sdate")]
  cube$coords <- cube$coords[
    !names(cube$coords) %in%
      c("dataset", "var", "sdate")
  ]
  error_message <- "Missing required core dimension"
  expect_error(.validate_s2dv_cube(cube), error_message)
})

test_that("5. Validation fails with missing coords or wrong spatial dims", {
  cube <- make_mock_fcst_cube()
  cube$coords <- NULL
  error_message <- "`data\\$coords` is missing"
  expect_error(.validate_s2dv_cube(cube), error_message)

  cube <- make_mock_fcst_cube()
  cube$dims <- cube$dims[!names(cube$dims) %in% c("latitude")]
  cube$coords <- cube$coords[!names(cube$coords) %in% c("latitude")]
  error_message <- paste0(
    "data must contain either \\(latitude & longitude\\)",
    ", location, or area dimension."
  )
  expect_error(.validate_s2dv_cube(cube), error_message)

  cube <- make_mock_fcst_cube()
  cube$coords$latitude <- NULL
  expect_error(.validate_s2dv_cube(cube))

  cube <- make_mock_fcst_cube()
  cube$coords$latitude <- cube$coords$latitude[-1]
  error_message <- "Latitude length does not match data dimension"
  expect_error(.validate_s2dv_cube(cube), error_message)

  cube <- make_mock_fcst_cube()
  cube$coords$longitude <- cube$coords$longitude[-1]
  error_message <- "Longitude length does not match data dimension"
  expect_error(.validate_s2dv_cube(cube), error_message)

  cube <- make_mock_location_cube()
  cube$coords$location <- cube$coords$location[-1]
  error_message <- "Location length does not match data dimension."
  expect_error(.validate_s2dv_cube(cube), error_message)

  cube <- make_mock_area_cube()
  cube$coords$area <- cube$coords$area[-1]
  error_message <- "Area length does not match data dimension."
  expect_error(.validate_s2dv_cube(cube), error_message)

  cube <- make_mock_location_cube()
  cube$attrs$location <- cube$attrs$location[
    !names(cube$attrs$location) %in%
      c("latitude")
  ]
  error_message <- "must contain latitude and longitude."
  expect_error(.validate_s2dv_cube(cube), error_message)
})

test_that("6 Validation fails if Dates is invalid", {
  cube <- make_mock_fcst_cube()
  cube$attrs$Dates <- as.POSIXct("2010-01-01")
  error_message <- "`attrs\\$Dates` must be a matrix"
  expect_error(.validate_s2dv_cube(cube), error_message)

  cube <- make_mock_fcst_cube(ns = 2, nt = 2)
  cube$attrs$Dates <- matrix(as.POSIXct("2010-01-01"), nrow = 1, ncol = 1)
  error_message <- "`attrs\\$Dates` must have \\[sdate, time\\] dimensions."
  expect_error(.validate_s2dv_cube(cube), error_message)
})


# --- Dates handling ----
test_that("7. .s2dv_to_terra handles NULL Dates", {
  cube <- make_mock_fcst_cube(
    nd = 1,
    nv = 1,
    ns = 1,
    nt = 1,
    nm = 1
  )

  cube$attrs$Dates <- NULL
  r <- .s2dv_to_terra(cube)
  expect_equal(
    names(r),
    "dataset_1_var_t2m_sdate_01_time_01_ensemble_0"
  )
})

test_that("8. .s2dv_to_df handles different Dates shapes", {
  # 2D Dates (ns > 1 && nt > 1)
  cube_2d <- make_mock_fcst_cube(ns = 2, nt = 2)
  df_2d <- .s2dv_to_df(cube_2d)
  expect_equal(df_2d$date[1], as.character(as.Date(cube_2d$attrs$Dates[1, 1])))
  idx_22 <- which(df_2d$sdate == 2 & df_2d$time == 2)[1]
  expect_equal(
    df_2d$date[idx_22],
    as.character(as.Date(cube_2d$attrs$Dates[2, 2]))
  )

  # 1D time (length == nt)
  cube_time <- make_mock_fcst_cube(ns = 1, nt = 3)
  df_time <- .s2dv_to_df(cube_time)
  expect_equal(
    unique(df_time$date),
    c("2025-01-01", "2025-01-02", "2025-01-03")
  )

  # 1D sdate (length == ns)
  cube_sdate <- make_mock_fcst_cube(ns = 3, nt = 1)
  df_sdate <- .s2dv_to_df(cube_sdate)
  idx_s2 <- which(df_sdate$sdate == 2)[1]
  expect_equal(df_sdate$date[idx_s2], "2025-01-02")

  # NULL Dates
  cube_null <- make_mock_fcst_cube()
  cube_null$attrs$Dates <- NULL
  df_null <- .s2dv_to_df(cube_null)
  expect_false("date" %in% names(df_null))
})

# --- Location handling ----
test_that("9. .s2dv_to_df handles cubes with location", {
  cube_loc <- make_mock_location_cube()
  df_loc <- .s2dv_to_df(cube_loc)

  expect_true(all(c("latitude", "longitude") %in% names(df_loc)))
  expect_equal(unique(df_loc$latitude), cube_loc$attrs$location$latitude)
  expect_equal(unique(df_loc$longitude), cube_loc$attrs$location$longitude)
})


# --- Area handling ----
test_that("10. .s2dv_to_df handles cubes with area", {
  cube_area <- make_mock_area_cube()
  df_area <- .s2dv_to_df(cube_area)

  expect_true("area" %in% names(df_area))
  expect_equal(unique(df_area$area), as.character(cube_area$coords$area))

  cube_area$attrs$area_id <- "Area_ID"
  df_area <- .s2dv_to_df(cube_area)
  expect_true("Area_ID" %in% names(df_area))
  expect_equal(unique(df_area$Area_ID), as.character(cube_area$coords$area))
})

# --- terra label fallback ----
test_that("11. get_label falls back when coord labels too short", {
  cube <- make_mock_fcst_cube(nd = 2)
  cube$coords$dataset <- "only_one"
  r <- .s2dv_to_terra(cube)

  expect_true(any(grepl("dataset_only_one", names(r))))
  expect_true(any(grepl("dataset_02", names(r))))
})

# --- .s2dv_to_sf area case ----
test_that("12. Happy .s2dv_to_sf with area cube", {
  cube <- make_mock_area_cube()
  cube$attrs$area$geom <- make_test_areas_sf()$geometry

  sf_obj <- .s2dv_to_sf(cube)

  # Returns sf object
  expect_s3_class(sf_obj, "sf")
  # Columns include at least the first layer value
  expect_true(
    "layer" %in%
      names(sf_obj)
  )
  # Geometry exists
  expect_true("geometry" %in% names(sf_obj))
})


# --- Happy path tests ----
test_that("13. Happy drop .s2dv_to_terra", {
  cube <- make_mock_fcst_cube()

  r <- .s2dv_to_terra(cube, drop = TRUE)

  # Returns SpatRaster
  expect_s4_class(r, "SpatRaster")
  # Produces correct spatial extent
  expect_equal(terra::xmin(r), min(cube$coords$longitude), tolerance = 0.5)
  expect_equal(terra::xmax(r), max(cube$coords$longitude), tolerance = 0.5)
  expect_equal(terra::ymin(r), min(cube$coords$latitude), tolerance = 0.5)
  expect_equal(terra::ymax(r), max(cube$coords$latitude), tolerance = 0.5)
  # Creates expected number of layers
  expected_layers <- 1 * 3 * 51
  expect_equal(terra::nlyr(r), expected_layers)
  # Names layers correctly
  expect_true(
    all(
      c(
        "sdate_20250101_time_20250101_ensemble_0",
        "sdate_20250101_time_20250102_ensemble_0",
        "sdate_20250101_time_20250103_ensemble_0",
        "sdate_20250101_time_20250101_ensemble_50",
        "sdate_20250101_time_20250102_ensemble_50",
        "sdate_20250101_time_20250103_ensemble_50"
      ) %in%
        names(r)
    )
  )
  # Preserves data value
  expect_equal(
    as.vector(terra::values(r[[1]])),
    as.vector(cube$data[1, 1, 1, 1, 1, , ]),
    tolerance = 1e-2
  )
})

test_that("14. Happy drop .s2dv_to_sf", {
  cube <- make_mock_fcst_cube(ns = 1, nt = 2, nm = 1)

  sf_obj <- .s2dv_to_sf(cube, drop = TRUE)

  # Returns sf object
  expect_s3_class(sf_obj, "sf")
  # Columns include at least the first layer value
  expect_true(
    "sdate_20250101_time_20250101" %in%
      names(sf_obj)
  )
  # Geometry exists
  expect_true("geometry" %in% names(sf_obj))

  cube <- make_mock_area_cube(nm = 1)
  cube$attrs$area$geom <- make_test_areas_sf()$geometry

  sf_obj <- .s2dv_to_sf(cube, drop = TRUE)

  # Returns sf object
  expect_s3_class(sf_obj, "sf")
  # Columns include at least the first layer value
  expect_true(
    "layer" %in%
      names(sf_obj)
  )
  # Geometry exists
  expect_true("geometry" %in% names(sf_obj))
})


test_that("15. Happy drop .s2dv_to_df", {
  cube <- make_mock_fcst_cube()

  df <- .s2dv_to_df(cube, drop = TRUE)

  # Returns data.frame
  expect_s3_class(df, "data.frame")
  # Correct number of rows
  expected_rows <- 1 * 1 * 1 * 3 * 51 * 5 * 5
  expect_equal(nrow(df), expected_rows)
  # Contains expected columns
  expect_true(
    all(
      c(
        "sdate",
        "time",
        "ensemble",
        "latitude",
        "longitude",
        "value",
        "date",
        "ensemble_value"
      ) %in%
        names(df)
    ) &&
      !any(c("dataset", "var", "var_names") %in% names(df))
  )
  # Data preserved
  expect_equal(
    df$value[1],
    as.vector(cube$data)[1],
    tolerance = 1e-6
  )
})


test_that("16. Happy drop .s2dv_to_stars", {
  cube <- make_mock_fcst_cube()
  stars <- .s2dv_to_stars(cube, drop = TRUE)

  # Returns data.frame
  expect_s3_class(stars, "stars")
  # Contains expected columns
  expect_true(!any(c("dataset", "var", "var_names") %in% dimnames(stars)))
})

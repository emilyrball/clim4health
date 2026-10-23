# ---- Mock objects ----

.mock_dates <- function(ns, nt, start_date) {
  # Always a matrix: rows = sdate, cols = time
  Dates <- outer(
    start_date + seq_len(ns) - 1,
    seq_len(nt) - 1,
    `+`
  )
  as.POSIXct(Dates, origin = "1970-01-01")
}

.mock_cube <- function(dims, coords, attrs) {
  set.seed(1)
  cube <- list(
    data = array(runif(prod(dims), min = 298, max = 300), dim = dims),
    dims = dims,
    coords = coords,
    attrs = attrs
  )
  class(cube) <- "s2dv_cube"
  cube
}

.mock_core_coords <- function(nd, nv, ns, nt, nm) {
  list(
    dataset = if (nd == 1) 1 else seq_len(nd),
    var = if (nv == 1) "t2m" else paste0("var", seq_len(nv)),
    sdate = if (ns == 1) 1 else seq_len(ns),
    time = seq_len(nt),
    ensemble = 0:(nm - 1) # real fcst starts at 0
  )
}

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
  dims <- c(
    dataset = nd,
    var = nv,
    sdate = ns,
    time = nt,
    ensemble = nm,
    latitude = nlat,
    longitude = nlon
  )
  coords <- c(
    .mock_core_coords(nd, nv, ns, nt, nm),
    list(
      latitude = seq(6, 2, length.out = nlat), # descending, as in real fcst
      longitude = seq(-78, -74, length.out = nlon)
    )
  )
  .mock_cube(
    dims,
    coords,
    list(
      Dates = .mock_dates(ns, nt, start_date),
      Variable = list(varName = coords$var[1])
    )
  )
}

make_mock_location_cube <- function(
  nd = 1,
  nv = 1,
  ns = 1,
  nt = 1,
  nm = 1,
  nloc = 5,
  start_date = as.Date("2025-01-01")
) {
  dims <- c(
    dataset = nd,
    var = nv,
    sdate = ns,
    time = nt,
    ensemble = nm,
    location = nloc
  )
  coords <- c(
    .mock_core_coords(nd, nv, ns, nt, nm),
    list(location = seq_len(nloc)) # index, not lat/lon
  )
  .mock_cube(
    dims,
    coords,
    list(
      Dates = .mock_dates(ns, nt, start_date),
      location = list(
        latitude = seq(2, -4, length.out = nloc),
        longitude = seq(-7, 7, length.out = nloc)
      ),
      Variable = list(varName = coords$var[1])
    )
  )
}

make_mock_area_cube <- function(
  nd = 1,
  nv = 1,
  ns = 1,
  nt = 1,
  nm = 1,
  nloc = 2,
  start_date = as.Date("2025-01-01"),
  area_id = NULL,
  geom = NULL
) {
  dims <- c(
    dataset = nd,
    var = nv,
    sdate = ns,
    time = nt,
    ensemble = nm,
    area = nloc
  )
  coords <- c(
    .mock_core_coords(nd, nv, ns, nt, nm),
    list(area = seq_len(nloc)) # index, not lat/lon
  )
  if (!is.null(area_id)) {
    coords$area <- paste0("Area_", seq_len(nloc))
  }
  .mock_cube(
    dims,
    coords,
    list(
      Dates = .mock_dates(ns, nt, start_date),
      area = list(geom = geom),
      Variable = list(varName = coords$var[1])
    )
  )
}

make_test_areas_sf <- function(crs = 4326) {
  skip_if_not_installed("sf")

  poly1 <- sf::st_as_sfc(sf::st_bbox(
    c(xmin = -78, xmax = -75, ymin = -8, ymax = -6),
    crs = sf::st_crs(crs)
  ))
  poly2 <- sf::st_as_sfc(sf::st_bbox(
    c(xmin = -74, xmax = -72, ymin = -4, ymax = -2),
    crs = sf::st_crs(crs)
  ))

  sf::st_sf(
    munip_code = c("Area_A", "Area_B"),
    geometry = c(poly1, poly2)
  )
}

# Small real forecast cube (first `n` lat/lon cells) for integration tests
make_small_fcst <- function(n = 3) {
  fcst_path <- system.file("extdata", "forecast", package = "clim4health")
  fcst <- c4h_load(fcst_path, variable = "t2m", ext = "nc")

  fcst_small <- fcst
  fcst_small$data <- fcst$data[1, 1, 1, 1, 1, 1:n, 1:n, drop = FALSE]
  fcst_small$dims[c("dataset", "var", "sdate", "time", "ensemble")] <- 1
  fcst_small$dims[c("latitude", "longitude")] <- n
  fcst_small$coords$latitude <- fcst$coords$latitude[1:n]
  fcst_small$coords$longitude <- fcst$coords$longitude[1:n]
  fcst_small
}

# Mock that records the arguments the dispatcher forwards
capture_args <- function(x, crs = "<not passed>", drop = "<not passed>") {
  list(crs = crs, drop = drop)
}

# ---- Input validation ----

test_that("c4h_convert errors on an unknown output_format", {
  cube <- make_mock_fcst_cube()
  expect_error(
    c4h_convert(cube, "invalid_format"),
    "Invalid output format. Choose "
  )
})

test_that("c4h_convert errors on malformed output_format", {
  cube <- make_mock_fcst_cube()
  msg <- "Invalid output format. Choose "
  expect_error(c4h_convert(cube, NULL), msg)
  expect_error(c4h_convert(cube, c("sf", "terra")), msg)
  expect_error(c4h_convert(cube, 1), msg)
})

test_that("c4h_convert errors if data is not an s2dv_cube", {
  msg <- "'data' must be an s2dv_cube."
  expect_error(c4h_convert(list(), "terra"), msg)
  expect_error(c4h_convert(NULL, "terra"), msg)
  expect_error(c4h_convert(data.frame(a = 1), "data.frame"), msg)
})

test_that("c4h_convert validates the structure of the s2dv_cube", {
  cube <- make_mock_fcst_cube()
  cube$coords$latitude <- cube$coords$latitude[-1]
  expect_error(
    c4h_convert(cube, "terra"),
    "Latitude length does not match data dimension."
  )
})

test_that("c4h_convert errors if drop is invalid", {
  cube <- make_mock_fcst_cube()
  msg <- "'drop' must be a single logical value."
  expect_error(c4h_convert(cube, "terra", drop = "not_logical"), msg)
  expect_error(c4h_convert(cube, "terra", drop = c(TRUE, FALSE)), msg)
  expect_error(c4h_convert(cube, "terra", drop = NULL), msg)
  expect_error(c4h_convert(cube, "terra", drop = NA), msg)
})

test_that("c4h_convert requires gridded data for terra and stars", {
  msg <- "Gridded data is required for 'terra' and 'stars' formats."
  expect_error(c4h_convert(make_mock_location_cube(), "terra"), msg)
  expect_error(c4h_convert(make_mock_location_cube(), "stars"), msg)
  expect_error(c4h_convert(make_mock_area_cube(), "terra"), msg)
  expect_error(c4h_convert(make_mock_area_cube(), "stars"), msg)
})

test_that("c4h_convert requires gridded or polygon data for sf", {
  expect_error(
    c4h_convert(make_mock_location_cube(), "sf"),
    "Gridded or polygon data is required for 'sf' output format."
  )
})

# ---- Dispatch ----

test_that("c4h_convert dispatches to .s2dv_to_terra with crs and drop", {
  cube <- make_mock_fcst_cube()
  with_mocked_bindings(
    .s2dv_to_terra = capture_args,
    {
      out <- c4h_convert(cube, "terra", crs = "EPSG:3857", drop = TRUE)
      expect_equal(out, list(crs = "EPSG:3857", drop = TRUE))
    }
  )
})

test_that("c4h_convert dispatches to .s2dv_to_stars with crs and drop", {
  cube <- make_mock_fcst_cube()
  with_mocked_bindings(
    .s2dv_to_stars = capture_args,
    {
      out <- c4h_convert(cube, "stars", crs = "EPSG:3857", drop = TRUE)
      expect_equal(out, list(crs = "EPSG:3857", drop = TRUE))
    }
  )
})

test_that("c4h_convert dispatches to .s2dv_to_df without crs", {
  cube <- make_mock_fcst_cube()
  with_mocked_bindings(
    .s2dv_to_df = capture_args,
    {
      out <- c4h_convert(cube, "data.frame", crs = "EPSG:3857", drop = TRUE)
      expect_equal(out, list(crs = "<not passed>", drop = TRUE))
    }
  )
})

test_that("c4h_convert dispatches gridded data to .s2dv_to_sf", {
  cube <- make_mock_fcst_cube()
  with_mocked_bindings(
    .s2dv_to_sf = capture_args,
    {
      out <- c4h_convert(cube, "sf", crs = "EPSG:3857", drop = FALSE)
      expect_equal(out, list(crs = "EPSG:3857", drop = FALSE))
    }
  )
})

test_that("c4h_convert dispatches area data to .s2dv_to_sf", {
  area_cube <- make_mock_area_cube(geom = make_test_areas_sf()$geometry)
  with_mocked_bindings(
    .s2dv_to_sf = capture_args,
    {
      out <- c4h_convert(area_cube, "sf", crs = "EPSG:4326")
      expect_equal(out, list(crs = "EPSG:4326", drop = FALSE))
    }
  )
})

test_that("c4h_convert allows data.frame output for every data type", {
  cubes <- list(
    make_mock_fcst_cube(),
    make_mock_location_cube(),
    make_mock_area_cube()
  )
  with_mocked_bindings(
    .s2dv_to_df = function(x, drop) "df_result",
    {
      for (cube in cubes) {
        expect_equal(c4h_convert(cube, "data.frame"), "df_result")
      }
    }
  )
})

# ---- crs resolution ----

test_that("c4h_convert assumes WGS 84 for gridded data when crs is NULL", {
  cube <- make_mock_fcst_cube()
  with_mocked_bindings(
    .s2dv_to_terra = capture_args,
    .s2dv_to_stars = capture_args,
    {
      expect_message(
        out <- c4h_convert(cube, "terra"),
        "'crs' is not provided. Assuming WGS 84"
      )
      expect_equal(out$crs, "EPSG:4326")

      expect_message(
        out <- c4h_convert(cube, "stars"),
        "'crs' is not provided. Assuming WGS 84"
      )
      expect_equal(out$crs, "EPSG:4326")
    }
  )
})

test_that("c4h_convert takes crs from area geometries when crs is NULL", {
  geom <- make_test_areas_sf(crs = 3857)$geometry
  area_cube <- make_mock_area_cube(geom = geom)
  with_mocked_bindings(
    .s2dv_to_sf = capture_args,
    {
      expect_message(
        out <- c4h_convert(area_cube, "sf"),
        "'crs' is not provided. Taking CRS from"
      )
      expect_equal(out$crs, sf::st_crs(geom))
    }
  )
})

test_that("c4h_convert falls back to WGS 84 if area data has no usable CRS", {
  msg <- "input data has no CRS. Assuming WGS 84"

  with_mocked_bindings(
    .s2dv_to_sf = capture_args,
    {
      # geometry is missing (NULL): used to fail in sf::st_crs(NULL)
      area_cube <- make_mock_area_cube()
      expect_message(out <- c4h_convert(area_cube, "sf"), msg)
      expect_equal(out$crs, "EPSG:4326")

      # geometry exists but has no CRS (NA)
      area_cube$attrs$area$geom <- sf::st_set_crs(
        make_test_areas_sf()$geometry,
        NA
      )
      expect_message(out <- c4h_convert(area_cube, "sf"), msg)
      expect_equal(out$crs, "EPSG:4326")
    }
  )
})

test_that("c4h_convert does not message when crs is supplied", {
  cube <- make_mock_fcst_cube()
  with_mocked_bindings(
    .s2dv_to_terra = capture_args,
    .s2dv_to_stars = capture_args,
    .s2dv_to_sf = capture_args,
    {
      expect_no_message(c4h_convert(cube, "terra", crs = "EPSG:4326"))
      expect_no_message(c4h_convert(cube, "stars", crs = "EPSG:4326"))
      expect_no_message(c4h_convert(cube, "sf", crs = "EPSG:4326"))
    }
  )
})

test_that("c4h_convert does not message about crs for data.frame", {
  cube <- make_mock_fcst_cube()
  with_mocked_bindings(
    .s2dv_to_df = capture_args,
    {
      expect_no_message(c4h_convert(cube, "data.frame"))
    }
  )
})

# ---- .resolve_crs ----

test_that(".resolve_crs returns a user-provided crs unchanged", {
  cube <- make_mock_fcst_cube()
  expect_no_message(
    out <- .resolve_crs(cube, "EPSG:3857", "terra", "gridded")
  )
  expect_equal(out, "EPSG:3857")
})

test_that(".resolve_crs leaves crs NULL for data.frame", {
  cube <- make_mock_fcst_cube()
  expect_no_message(out <- .resolve_crs(cube, NULL, "data.frame", "gridded"))
  expect_null(out)
})

# ---- .s2dv_spatial_type ----

test_that(".s2dv_spatial_type detects each type of spatial data", {
  expect_equal(.s2dv_spatial_type(make_mock_fcst_cube()), "gridded")
  expect_equal(.s2dv_spatial_type(make_mock_area_cube()), "area")
  expect_equal(.s2dv_spatial_type(make_mock_location_cube()), "point")

  cube <- make_mock_fcst_cube()
  cube$dims <- cube$dims[c("dataset", "var", "sdate", "time", "ensemble")]
  expect_equal(.s2dv_spatial_type(cube), "none")
})

test_that(".s2dv_spatial_type gives priority to gridded, then area", {
  cube <- make_mock_fcst_cube()
  cube$dims <- c(cube$dims, area = 2, location = 2)
  expect_equal(.s2dv_spatial_type(cube), "gridded")

  cube$dims <- cube$dims[setdiff(names(cube$dims), c("latitude", "longitude"))]
  expect_equal(.s2dv_spatial_type(cube), "area")
})

# ---- Integration ----

test_that("c4h_convert: terra integration works", {
  r <- c4h_convert(make_small_fcst(5), "terra")

  expect_s4_class(r, "SpatRaster")
  expect_equal(terra::nlyr(r), 1)
  expect_equal(dim(r)[1:2], c(5, 5))
  expect_equal(terra::crs(r, describe = TRUE)$code, "4326")
})

test_that("c4h_convert: stars integration works", {
  s <- c4h_convert(make_small_fcst(3), "stars")

  expect_s3_class(s, "stars")
  expect_equal(
    unname(names(stars::st_dimensions(s))[1:2]),
    c("longitude", "latitude")
  )
  expect_equal(sf::st_crs(s)$epsg, 4326L)
})

test_that("c4h_convert: data.frame integration works", {
  df <- c4h_convert(make_small_fcst(3), "data.frame")

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 9)
  expect_true("value" %in% names(df))
})

test_that("c4h_convert: sf integration works", {
  sf_obj <- c4h_convert(make_small_fcst(3), "sf")

  expect_s3_class(sf_obj, "sf")
  expect_true("geometry" %in% names(sf_obj))
  expect_equal(nrow(sf_obj), 9)
})

test_that("c4h_convert: drop = TRUE removes singleton dimensions", {
  fcst <- make_small_fcst(3)

  r_keep <- c4h_convert(fcst, "terra", drop = FALSE)
  r_drop <- c4h_convert(fcst, "terra", drop = TRUE)
  expect_true(nchar(names(r_keep)) > nchar(names(r_drop)))

  df_keep <- c4h_convert(fcst, "data.frame", drop = FALSE)
  df_drop <- c4h_convert(fcst, "data.frame", drop = TRUE)
  expect_true(all(c("dataset", "var", "ensemble") %in% names(df_keep)))
  expect_false(any(c("dataset", "var", "ensemble") %in% names(df_drop)))
})

test_that("c4h_convert: area data converts to sf and data.frame", {
  areas <- make_test_areas_sf()
  cube <- make_mock_area_cube(area_id = TRUE, geom = areas$geometry)

  sf_obj <- c4h_convert(cube, "sf")
  expect_s3_class(sf_obj, "sf")
  expect_equal(nrow(sf_obj), 2)
  expect_true(sf::st_crs(sf_obj) == sf::st_crs(areas))

  df <- c4h_convert(cube, "data.frame")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2)
})

test_that("c4h_convert: area geometries are transformed to a new crs", {
  areas <- make_test_areas_sf(crs = 3857)
  cube <- make_mock_area_cube(geom = areas$geometry)

  sf_obj <- c4h_convert(cube, "sf", crs = "EPSG:4326")

  expect_true(sf::st_crs(sf_obj) == sf::st_crs(4326))
  # coordinates were reprojected, not just relabelled
  expect_false(isTRUE(all.equal(
    as.numeric(sf::st_bbox(sf_obj)),
    as.numeric(sf::st_bbox(areas))
  )))
})

test_that("c4h_convert: area geometries without crs are assigned one", {
  areas <- make_test_areas_sf()
  geom <- sf::st_set_crs(areas$geometry, NA)
  cube <- make_mock_area_cube(geom = geom)

  sf_obj <- c4h_convert(cube, "sf", crs = "EPSG:4326")

  expect_true(sf::st_crs(sf_obj) == sf::st_crs(4326))
  # coordinates are unchanged
  expect_equal(
    as.numeric(sf::st_bbox(sf_obj)),
    as.numeric(sf::st_bbox(areas))
  )
})

test_that("c4h_convert: location data converts to data.frame", {
  cube <- make_mock_location_cube(nloc = 4)

  df <- c4h_convert(cube, "data.frame")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 4)
  expect_true(all(c("latitude", "longitude") %in% names(df)))
})

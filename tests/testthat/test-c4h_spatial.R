make_test_s2dv_cube <- function(reverse_latitude = FALSE) {
  # Dimensions
  lon <- seq(-80, -70, length.out = 10)
  lat <- seq(-10, 0, length.out = 10)
  if (reverse_latitude) {
    lat <- seq(0, -10, length.out = 10)
  }
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

# This is your "source of truth" for the geometries
make_test_areas_sf <- function() {
  library(sf)
  library(terra)

  # Define extent within the cube's range (-80 to -70, -10 to 0)
  poly1 <- st_as_sfc(st_bbox(
    c(xmin = -78, xmax = -75, ymin = -8, ymax = -6),
    crs = st_crs(4326)
  ))
  poly2 <- st_as_sfc(st_bbox(
    c(xmin = -74, xmax = -72, ymin = -4, ymax = -2),
    crs = st_crs(4326)
  ))

  areas_sf <- st_sf(
    munip_code = c("Area_A", "Area_B"),
    geometry = c(poly1, poly2)
  )
  return(areas_sf)
}

# This creates the exact same 2 polygons but as a SpatVector
make_test_areas_terra <- function() {
  areas_sf <- make_test_areas_sf()
  return(terra::vect(areas_sf))
}

test_that("1. Failure if areas_id not found", {
  # 1. Setup mock objects
  cube <- make_test_s2dv_cube()
  areas_sf <- make_test_areas_sf()

  # 2. Run c4h_space
  # We test that it runs with expected error
  expect_error(
    {
      obs_agg <- c4h_space(
        data = cube,
        areas = areas_sf,
        areas_id = "dummy_code"
      )
    },
    "'areas_id' not found in the 'areas' object."
  )
})

test_that("2. Happy path with correct spatial aggregation", {
  # 1. Setup mock objects
  cube <- make_test_s2dv_cube()
  areas_sf <- make_test_areas_sf()

  # 2. Run c4h_space
  # We test that it runs without error
  expect_error(
    {
      obs_agg <- c4h_space(
        data = cube,
        areas = areas_sf,
        areas_id = "munip_code"
      )
    },
    NA
  )

  # 3. Verify the structure of the aggregated object
  expect_s3_class(obs_agg, "s2dv_cube")
  expect_true("area" %in% names(obs_agg$dims))
  expect_equal(as.numeric(obs_agg$dims["area"]), 2)
  expect_equal(drop(obs_agg$data), c(287, 294), tolerance = 1e-2)
  expect_equal(obs_agg$coords$area, c("Area_A", "Area_B"))
})

test_that("3. Handling latitude stored north to south", {
  # 1. Setup mock objects
  cube <- make_test_s2dv_cube(reverse_latitude = TRUE)
  areas_sf <- make_test_areas_sf()

  # 2. Run c4h_space
  # We test that it runs without error
  expect_error(
    {
      obs_agg <- c4h_space(
        data = cube,
        areas = areas_sf,
        areas_id = "munip_code"
      )
    },
    NA
  )

  # 3. Verify the structure of the aggregated object
  expect_s3_class(obs_agg, "s2dv_cube")
  expect_true("area" %in% names(obs_agg$dims))
  expect_equal(as.numeric(obs_agg$dims["area"]), 2)
  expect_equal(drop(obs_agg$data), c(287, 294), tolerance = 1e-2)
  expect_equal(obs_agg$coords$area, c("Area_A", "Area_B"))
})

test_that("4. Identical results for sf, terra and path areas", {
  cube <- make_test_s2dv_cube()

  # 1. Test with sf
  areas_sf <- make_test_areas_sf()
  res_sf <- c4h_space(cube, areas = areas_sf, areas_id = "munip_code")

  # 2. Test with terra (SpatVector)
  areas_terra <- make_test_areas_terra()
  res_terra <- c4h_space(cube, areas = areas_terra, areas_id = "munip_code")

  # 3. Test with file path
  tmp_file <- tempfile(fileext = ".gpkg")
  st_write(areas_sf, tmp_file, quiet = TRUE)
  res_file <- c4h_space(cube, areas = tmp_file, areas_id = "munip_code")

  # 3. Verify they are identical
  # Note: We compare the data array and the area coordinates
  expect_equal(res_sf$data, res_terra$data)
  expect_equal(res_sf$data, res_file$data)
  expect_equal(res_sf$coords$area, res_terra$coords$area)
  expect_equal(res_sf$coords$area, res_file$coords$area)

  if (file.exists(tmp_file)) file.remove(tmp_file)
})

test_that("5. Different results for weighting options", {
  cube <- make_test_s2dv_cube()

  # The grid lon is -80 to -70 (10 cells, so edges are -80, -79, -78...)
  # The grid lat is -10 to 0 (10 cells, so edges are -10, -9, -8...)
  # We place a polygon across the longitude line -75.0
  # It occupies 0.4 units of the cell to the left and 0.1 units to the right
  poly_weighted <- st_as_sf(st_as_sfc(st_bbox(
    c(xmin = -75.4, xmax = -74.9, ymin = -5.5, ymax = -4.5),
    crs = st_crs(4326)
  )))
  poly_weighted$munip_code <- "W_Test"

  # 1. Simple mean
  res_none <- c4h_space(cube, poly_weighted, weighting = "none", fun = "mean")
  # 2. Approx weighting
  res_approx <- c4h_space(
    cube,
    poly_weighted,
    weighting = "approx",
    fun = "mean"
  )
  # 3. Exact weighting
  res_exact <- c4h_space(cube, poly_weighted, weighting = "exact", fun = "mean")

  # Convert to vectors to ensure we are comparing values, not array attributes
  val_none <- as.vector(res_none$data)
  val_approx <- as.vector(res_approx$data)
  val_exact <- as.vector(res_exact$data)

  # 'None' should be significantly different from 'Exact' and 'Approx'
  # (Difference should be large because 'none' ignores partial cell areas)
  expect_gt(abs(val_none - val_exact), 1e-2)
  expect_gt(abs(val_none - val_approx), 1e-2)

  # 'Approx' should be different from 'Exact'
  # but the difference will be much smaller.
  expect_gt(abs(val_approx - val_exact), 1e-7)
  # Double check they aren't EXACTLY the same (bit-for-bit)
  expect_false(identical(val_approx, val_exact))
})

test_that("6. Aggregation function 'fun' works", {
  cube <- make_test_s2dv_cube()
  areas <- make_test_areas_sf()

  # Get mean vs max
  res_mean <- c4h_space(cube, areas, fun = "mean")
  res_max <- c4h_space(cube, areas, fun = "max")

  # In a gradient (like your make_test_s2dv_cube), max should be > mean
  expect_true(all(res_max$data > res_mean$data))
})

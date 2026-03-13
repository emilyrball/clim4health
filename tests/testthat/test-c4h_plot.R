make_mock_fcst_cube <- function(
  nd   = 1,   # datasets
  nv   = 1,   # variables
  ns   = 1,   # start dates
  nt   = 1,   # lead times
  nm   = 1,  # ensemble members
  nlat = 5,
  nlon = 5,
  start_date = as.Date("2025-01-01")
) {

  # --- spatial coords (match real fcst quirks)
  lon <- seq(-78, -74, length.out = nlon)
  lat <- seq(6, 2, length.out = nlat)  # descending latitude

  dims <- c(
    dataset   = nd,
    var       = nv,
    sdate     = ns,
    time      = nt,
    ensemble  = nm,
    latitude  = nlat,
    longitude = nlon
  )

  # --- data
  data <- array(
    runif(prod(dims), min = 298, max = 300),
    dim = dims
  )

  # --- coords
  coords <- list(
    dataset   = if (nd == 1) 1 else seq_len(nd),
    var       = if (nv == 1) "t2m" else paste0("var", seq_len(nv)),
    sdate     = if (ns == 1) 1 else seq_len(ns),
    time      = seq_len(nt),
    ensemble  = 0:(nm - 1),        # real fcst starts at 0
    latitude  = lat,
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
  nd   = 1,   # datasets
  nv   = 1,   # variables
  ns   = 1,   # start dates
  nt   = 1,   # lead times
  nm   = 1,  # ensemble members
  nloc = 5,
  start_date = as.Date("2025-01-01")
) {

  # --- spatial coords (match real fcst quirks)
  lon <- seq(-7, 7, length.out = nloc)
  lat <- seq(2, -4, length.out = nloc)  # descending latitude

  dims <- c(
    dataset   = nd,
    var       = nv,
    sdate     = ns,
    time      = nt,
    ensemble  = nm,
    location = nloc
  )

  # --- data
  data <- array(
    runif(prod(dims), min = 298, max = 300),
    dim = dims
  )

  # --- coords
  coords <- list(
    dataset   = if (nd == 1) 1 else seq_len(nd),
    var       = if (nv == 1) "t2m" else paste0("var", seq_len(nv)),
    sdate     = if (ns == 1) 1 else seq_len(ns),
    time      = seq_len(nt),
    ensemble  = 0:(nm - 1),        # real fcst starts at 0
    location = seq_len(nloc)   # <-- index, not lat/lon
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
        latitude  = lat,
        longitude = lon
      ),
      Variable = list(varName = coords$var[1])
    )
  )

  class(cube) <- "s2dv_cube"
  cube
}

make_mock_area_cube <- function(
  nd   = 1,   # datasets
  nv   = 1,   # variables
  ns   = 1,   # start dates
  nt   = 1,   # lead times
  nm   = 1,  # ensemble members
  nloc = 2,
  start_date = as.Date("2025-01-01"),
  area_id = NULL
) {

  dims <- c(
    dataset   = nd,
    var       = nv,
    sdate     = ns,
    time      = nt,
    ensemble  = nm,
    area      = nloc
  )

  # --- data
  data <- array(
    runif(prod(dims), min = 298, max = 300),
    dim = dims
  )

  # --- coords
  coords <- list(
    dataset   = if (nd == 1) 1 else seq_len(nd),
    var       = if (nv == 1) "t2m" else paste0("var", seq_len(nv)),
    sdate     = if (ns == 1) 1 else seq_len(ns),
    time      = seq_len(nt),
    ensemble  = 0:(nm - 1),        # real fcst starts at 0
    area      = seq_len(nloc)   # <-- index, not lat/lon
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
  poly1 <- st_as_sfc(st_bbox(c(xmin = -78, xmax = -75, ymin = -8, ymax = -6),
                             crs = st_crs(crs)))
  poly2 <- st_as_sfc(st_bbox(c(xmin = -74, xmax = -72, ymin = -4, ymax = -2),
                             crs = st_crs(crs)))

  areas_sf <- st_sf(
    munip_code = c("Area_A", "Area_B"),
    geometry = c(poly1, poly2)
  )
  return(areas_sf)
}

cube_1 <- make_mock_fcst_cube()
area_1 <- make_mock_area_cube()
location_1 <- make_mock_location_cube()

test_that("1. failure if data is not an s2dv_cube", {
  expect_error(c4h_plot(1:10), "'data' must be an s2dv_cube")
})

test_that("2. failure if var is not valid", {
  cube <- make_mock_fcst_cube(nv = 2)
  expect_error(c4h_plot(cube), paste0("More than one 'var' is available,",
                                      " please select one."))
  expect_error(c4h_plot(cube, var = 1:2), "Only one 'var' is possible.")
  expect_error(c4h_plot(cube, var = "nonexistent"),
               paste0("'var' not found in the data: ", "nonexistent"))
})

test_that("3. failure if sdate is not valid", {
  expect_error(c4h_plot(cube_1, sdate = "not_numeric"),
               "'sdate' must be of type numeric.")
  expect_error(c4h_plot(cube_1, sdate = 2),
               paste0("A wrong 'sdate' has been selected \\(possible",
                      " range: 1 - 1\\)."))
})

test_that("4. failure if time is not valid", {
  expect_error(c4h_plot(cube_1, time = "not_numeric"),
               "'time' must be of type numeric.")
  expect_error(c4h_plot(cube_1, time = 2),
               paste0("A wrong 'time' has been selected \\(possible",
                      " range: 1 - 1\\)."))
})

test_that("5. failure if ensemble is not valid", {
  expect_error(c4h_plot(cube_1, ensemble = "not_numeric"),
               "'ensemble' must be either TRUE, NULL, or of type numeric.")
  expect_error(c4h_plot(cube_1, ensemble = FALSE),
               "'ensemble' must be either TRUE, NULL, or of type numeric.")
  expect_error(c4h_plot(cube_1, ensemble = 2),
               paste0("A wrong 'ensemble' has been selected \\(possible",
                      " range: 1 - 1\\)."))
})

test_that("6. failure if all sdate, time, ensemble length greater than 1", {
  cube <- make_mock_fcst_cube(ns = 2, nt = 2, nm = 2)
  expect_error(c4h_plot(cube, sdate = 1:2, time = 1:2, ensemble = 1:2),
               paste0("Only two out of 'sdate', 'time' and 'ensemble' can have",
                      " a length larger than one."))
})

test_that("7. failure if boundaries is not valid", {
  expect_error(c4h_plot(cube_1, boundaries = "invalid"),
               paste0("'boundaries' must be set to either 'countries' ",
                      "or be a polygon 'sf' object."))
})
test_that("8. failure if centering is not valid", {
  expect_error(c4h_plot(cube_1, centering = "not_numeric"),
               "'centering' must be numeric.")
  expect_error(c4h_plot(cube_1, centering = max(cube_1$data) + 1),
               paste0("'centering' must be between the data",
                      " minimum and maximum values."))
})

test_that("9. failure if bins, bins_method, bins_label is not valid", {
  expect_error(c4h_plot(cube_1, bins = 1.5),
               "bins must be an integer.")
  expect_error(c4h_plot(cube_1, bins = 1, bins_method = "invalid"),
               "bins_method must be 'quantile' or 'equal'.")
  expect_error(c4h_plot(cube_1, bins = 1, bins_method = "quantile",
                        bins_label = 1:2),
               "The length of bins_label must be equal to the number of bins.")
})

test_that("10. warning if number of panels is large", {
  cube_large <- make_mock_fcst_cube(ns = 6, nt = 6, nm = 6)
  expect_warning(c4h_plot(cube_large, sdate = 1),
                 paste0("The number of panels is very large, consider ",
                        "selecting a smaller set of times\\/ensemble ",
                        "members\\/start dates."))
  expect_warning(c4h_plot(cube_large, time = 1),
                 paste0("The number of panels is very large, consider ",
                        "selecting a smaller set of times\\/ensemble ",
                        "members\\/start dates."))
  expect_warning(c4h_plot(cube_large, ensemble = TRUE),
                 paste0("The number of panels is very large, consider ",
                        "selecting a smaller set of times\\/ensemble ",
                        "members\\/start dates."))
})

test_that("11. expect message when aggregating ensemble", {
  cube_10 <- make_mock_fcst_cube(nm = 10)
  expect_message(c4h_plot(cube_10, ensemble = TRUE),
                 "Aggregating ensemble members with the function: mean")
})

test_that("12. expect message when boundaries are not EPSG 4326", {
  areas_sf <- make_test_areas_sf(crs = 3857)
  expect_message(c4h_plot(cube_1, boundaries = areas_sf),
                 "Transforming 'boundaries' to CRS EPSG 4326")

  # expect silent when boundaries are already in EPSG 4326
  areas_sf <- make_test_areas_sf(crs = 4326)
  res <- c4h_plot(cube_1, boundaries = areas_sf)
  expect_s3_class(res, "ggplot")
  # expect silent when boundaries is "countries"
  res <- c4h_plot(cube_1, boundaries = "countries")
  expect_s3_class(res, "ggplot")
  res <- c4h_plot(cube_1, boundaries = "countries", mask_boundaries = TRUE)
  expect_s3_class(res, "ggplot")
})

test_that("13. happy when var is character", {
  res <- c4h_plot(cube_1, var = "t2m")
  expect_s3_class(res, "ggplot")

  res <- c4h_plot(cube_1, var = 1)
  expect_s3_class(res, "ggplot")
})

test_that("14. happy s2dv_cube is location or area", {
  res <- c4h_plot(location_1, var = "t2m")
  expect_s3_class(res, "ggplot")

  area_1$attrs$area$geom <- make_test_areas_sf()$geometry
  res <- c4h_plot(area_1, var = "t2m")
  expect_s3_class(res, "ggplot")
})

test_that("15. happy bins", {
  res <- c4h_plot(cube_1, var = "t2m", bins = 5,
                  bins_method = "quantile")
  expect_s3_class(res, "ggplot")
  res <- c4h_plot(cube_1, var = "t2m", bins = 5,
                  bins_method = "quantile",
                  bins_label = as.character(1:5))
  expect_s3_class(res, "ggplot")
  res <- c4h_plot(cube_1, var = "t2m", bins = 5,
                  bins_method = "equal")
  expect_s3_class(res, "ggplot")
  res <- c4h_plot(cube_1, var = "t2m", bins = 5,
                  bins_method = "equal",
                  bins_label = as.character(1:5))
  expect_s3_class(res, "ggplot")
})


test_that("15. happy location geometries and labels", {
  # this is not testing anything currently, aiming to hit
  # L429, L432
  res <- c4h_plot(location_1, var = "t2m",
                  boundaries = "countries",
                  bins = 5)
  expect_s3_class(res, "ggplot")
  res <- c4h_plot(location_1, var = "t2m",
                  centering = mean(location_1$data))
  expect_s3_class(res, "ggplot")

})

test_that("16. happy area geometries and labels", {
  # this is not testing anything currently, aiming to hit
  # L471, L474
  area_1$attrs$area$geom <- make_test_areas_sf()$geometry

  res <- c4h_plot(area_1, var = "t2m",
                  boundaries = "countries", bins = 5)
  expect_s3_class(res, "ggplot")
  res <- c4h_plot(area_1, var = "t2m", centering = mean(area_1$data))
  expect_s3_class(res, "ggplot")
})

test_that("17. happy faceting", {
  cube <- make_mock_fcst_cube(nt = 3)
  res <- c4h_plot(cube, var = "t2m", coordgrid = TRUE,
                  centering = mean(cube$data))
  expect_s3_class(res, "ggplot")

  cube <- make_mock_fcst_cube(nt = 3, nm = 3)
  res <- c4h_plot(cube, var = "t2m", coordgrid = TRUE,
                  centering = mean(cube$data))
  expect_s3_class(res, "ggplot")
})

test_that("18. happy title", {
  res <- c4h_plot(cube_1, title = "Test Title")
  expect_s3_class(res, "ggplot")
})

test_that("19. happy legends", {
  res <- c4h_plot(cube_1, legend = "Test Legend")
  expect_s3_class(res, "ggplot")
  res <- c4h_plot(location_1, legend = "Test Legend")
  expect_s3_class(res, "ggplot")
})

test_that("20. happy time labels when not gridded and sdate/time present", {
  cube <- make_mock_location_cube(nt = 3, ns = 3, nm = 2)
  res <- c4h_plot(cube, sdate = 1)
  expect_s3_class(res, "ggplot")
})
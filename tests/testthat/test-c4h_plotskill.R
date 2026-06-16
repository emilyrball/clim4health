make_mock_gridded_data <- function(
  nd = 1,
  nv = 1,
  nt = 1,
  nm = 1,
  nlat = 5,
  nlon = 5,
  metric = "rpss"
) {
  # --- spatial coords (match real fcst quirks)
  lon <- seq(-78, -74, length.out = nlon)
  lat <- seq(6, 2, length.out = nlat) # descending latitude

  dims <- c(
    dataset = nd,
    var = nv,
    sdate = 1,
    time = nt,
    ensemble = nm,
    latitude = nlat,
    longitude = nlon
  )

  # --- Dates
  # Always a matrix: rows = sdate, cols = time
  Dates <- outer(
    as.Date("2025-01-01") + seq_len(1) - 1,
    seq_len(nt) - 1,
    `+`
  )
  Dates <- as.POSIXct(Dates, origin = "1970-01-01")
  dim(Dates) <- c(sdate = 1, time = nt)

  # --- data
  data <- array(
    runif(prod(dims), min = -1, max = 1),
    dim = dims
  )

  # --- significance
  sign <- data
  sign[sign <= 0] <- FALSE
  sign[sign > 0] <- TRUE
  class(sign) <- "logical"

  # --- coords
  coords <- list(
    dataset = if (nd == 1) 1 else seq_len(nd),
    var = if (nv == 1) "t2m" else paste0("var", seq_len(nv)),
    sdate = 1,
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
  sign_cube <- list(
    data = sign,
    dims = dims,
    coords = coords,
    attrs = list(
      Dates = Dates,
      Variable = list(varName = coords$var[1])
    )
  )
  class(data_cube) <- "s2dv_cube"
  class(sign_cube) <- "s2dv_cube"

  skill <- list(data_cube, sign_cube)
  names(skill) <- c(metric, paste0("sign"))
  return(skill)
}

make_mock_location_data <- function(
  nd = 1, # datasets
  nv = 1, # variables
  nt = 1, # lead times
  nm = 1, # ensemble members
  nloc = 5,
  metric = "rpss"
) {
  # --- spatial coords (match real fcst quirks)
  lon <- seq(-78, -74, length.out = nloc)
  lat <- seq(6, 2, length.out = nloc) # descending latitude

  dims <- c(
    dataset = nd,
    var = nv,
    sdate = 1,
    time = nt,
    ensemble = nm,
    location = nloc
  )

  # --- coords
  coords <- list(
    dataset = if (nd == 1) 1 else seq_len(nd),
    var = if (nv == 1) "t2m" else paste0("var", seq_len(nv)),
    sdate = 1,
    time = seq_len(nt),
    ensemble = 0:(nm - 1), # real fcst starts at 0
    location = seq_len(nloc) # <-- index, not lat/lon
  )

  # --- Dates
  # Always a matrix: rows = sdate, cols = time
  Dates <- outer(
    as.Date("2025-01-01") + seq_len(1) - 1,
    seq_len(nt) - 1,
    `+`
  )
  Dates <- as.POSIXct(Dates, origin = "1970-01-01")
  dim(Dates) <- c(sdate = 1, time = nt)

  # --- data
  data <- array(
    runif(prod(dims), min = -2, max = 1),
    dim = dims
  )
  # --- significance
  sign <- data
  sign[sign <= 0] <- FALSE
  sign[sign > 0] <- TRUE
  class(sign) <- "logical"

  data_cube <- list(
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
  sign_cube <- list(
    data = sign,
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

  class(data_cube) <- "s2dv_cube"
  class(sign_cube) <- "s2dv_cube"
  skill <- list(data_cube, sign_cube)
  names(skill) <- c(metric, paste0("sign"))
  return(skill)
}

make_mock_area_data <- function(
  nd = 1, # datasets
  nv = 1, # variables
  nt = 1, # lead times
  nm = 1, # ensemble members
  nloc = 2,
  area_id = NULL,
  metric = "rpss"
) {
  dims <- c(
    dataset = nd,
    var = nv,
    sdate = 1,
    time = nt,
    ensemble = nm,
    area = nloc
  )

  # --- data
  data <- array(
    runif(prod(dims), min = -2, max = 1),
    dim = dims
  )
  # --- significance
  sign <- data
  sign[sign <= 0] <- FALSE
  sign[sign > 0] <- TRUE
  class(sign) <- "logical"

  # --- coords
  coords <- list(
    dataset = if (nd == 1) 1 else seq_len(nd),
    var = if (nv == 1) "t2m" else paste0("var", seq_len(nv)),
    sdate = 1,
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
    as.Date("2025-01-01") + seq_len(1) - 1,
    seq_len(nt) - 1,
    `+`
  )
  Dates <- as.POSIXct(Dates, origin = "1970-01-01")
  dim(Dates) <- c(sdate = 1, time = nt)

  data_cube <- list(
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
  sign_cube <- list(
    data = sign,
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

  class(data_cube) <- "s2dv_cube"
  class(sign_cube) <- "s2dv_cube"
  skill <- list(data_cube, sign_cube)
  names(skill) <- c(metric, paste0("sign"))
  return(skill)
}


make_test_areas_sf <- function(crs = 4326) {
  library(sf)
  library(terra)

  # Define extent within the cube's range (-80 to -70, -10 to 0)
  poly1 <- st_as_sfc(st_bbox(
    c(xmin = -78, xmax = -75, ymin = 4, ymax = 6),
    crs = st_crs(crs)
  ))
  poly2 <- st_as_sfc(st_bbox(
    c(xmin = -76, xmax = -74, ymin = 2, ymax = 4),
    crs = st_crs(crs)
  ))

  areas_sf <- st_sf(
    munip_code = c("Area_A", "Area_B"),
    geometry = c(poly1, poly2)
  )
  return(areas_sf)
}

test_that("1. failure if data is not an s2dv_cube", {
  expect_error(c4h_plotskill(1:10), "'skill' must be an s2dv_cube")
})

test_that("2. failure if sdate or ensemble dims are greater than 1", {
  skill <- make_mock_gridded_data()
  skill[[1]]$dims["sdate"] <- 2
  expect_error(
    c4h_plotskill(skill[[1]]),
    paste0(
      "'sdate' and 'ensemble' dimensions must have a length ",
      "of one. Please verify you are plotting the output of ",
      "c4h_verify()"
    )
  )

  skill[[1]]$dims["sdate"] <- 1
  skill[[1]]$dims["ensemble"] <- 2
  expect_error(
    c4h_plotskill(skill[[1]]),
    paste0(
      "'sdate' and 'ensemble' dimensions must have a length ",
      "of one. Please verify you are plotting the output of ",
      "c4h_verify()"
    )
  )
})

test_that("3. failure if any skill value greater than 1", {
  skill <- make_mock_gridded_data()
  skill[[1]]$data[1] <- 1.5
  expect_error(
    c4h_plotskill(skill[[1]]),
    paste0(
      "'skill' data must be lower than 1. Please use ",
      "'c4h_plot' for other metrics such as RMSE and MSE."
    )
  )
})

test_that("4. failure if sign is not valid", {
  skill <- make_mock_gridded_data()
  expect_error(
    c4h_plotskill(skill[[1]], sign = 1:10),
    "'sign' must be an s2dv_cube"
  )
  skill[[2]]$data[1] <- "not_logical"
  expect_error(
    c4h_plotskill(skill[[1]], skill[[2]]),
    "'sign' data must be of type logical \\(TRUE\\/FALSE\\)."
  )
  skill[[2]]$data[1] <- TRUE
  class(skill[[2]]$data) <- "logical"
  skill[[2]]$dims["sdate"] <- 3
  expect_error(
    c4h_plotskill(skill[[1]], skill[[2]]),
    "The dimensions of 'skill' and 'sign' must be identical."
  )
})

test_that("5. failure if time is invalid", {
  skill <- make_mock_gridded_data()
  expect_error(
    c4h_plotskill(skill[[1]], time = "not_numeric"),
    "'time' must be of type numeric."
  )

  expect_error(
    c4h_plotskill(skill[[1]], time = 3),
    paste0("A wrong 'time' has been selected \\(possible", " range: 1 - 1\\).")
  )
})

test_that("6. failure if boundaries is invalid", {
  skill <- make_mock_gridded_data()
  expect_error(
    c4h_plotskill(skill[[1]], boundaries = "not_valid"),
    paste0(
      "'boundaries' must be set to either 'countries' or be a ",
      "polygon 'sf' object."
    )
  )
})

test_that("7. warning if number of times is large", {
  skill <- make_mock_gridded_data(nt = 31)
  expect_warning(
    c4h_plotskill(skill[[1]]),
    paste0(
      "The number of panels is very large, consider ",
      "selecting a smaller set of times."
    )
  )
})

test_that("8. message if CRS is not 4326", {
  skill <- make_mock_gridded_data()
  expect_message(
    c4h_plotskill(skill[[1]], boundaries = make_test_areas_sf(crs = 3857)),
    paste0("Transforming 'boundaries' to CRS EPSG 4326")
  )
})

test_that("9. message if no skill values are significant", {
  skill <- make_mock_gridded_data()
  skill[[2]]$data[] <- FALSE
  expect_message(
    c4h_plotskill(skill[[1]], skill[[2]]),
    "No areas have a significant skill score."
  )
})

test_that("10. happy gridded legend, happy coordgrid, happy boundaries", {
  skill_gridded <- make_mock_gridded_data(nt = 3)
  res <- c4h_plotskill(
    skill_gridded[[1]],
    skill_gridded[[2]],
    legend = "Test Legend",
    coordgrid = TRUE
  )
  expect_s3_class(res, "ggplot")
  res <- c4h_plotskill(
    skill_gridded[[1]],
    boundaries = make_test_areas_sf(),
    mask_boundaries = TRUE
  )
  expect_s3_class(res, "ggplot")
})

test_that("11. happy areas, happy title", {
  skill_area <- make_mock_area_data(nt = 3)
  skill_area$rpss$attrs$area$geom <- make_test_areas_sf()$geometry
  skill_area$sign$attrs$area$geom <- make_test_areas_sf()$geometry
  res_area <- c4h_plotskill(skill_area[[1]], title = "Test Title")
  expect_s3_class(res_area, "ggplot")
  skill_area[[2]]$data[1] <- TRUE
  res_area <- c4h_plotskill(
    skill_area[[1]],
    skill_area[[2]],
    boundaries = "countries"
  )
  expect_s3_class(res_area, "ggplot")
})

test_that("12. happy location, happy legend", {
  skill_loc <- make_mock_location_data(nt = 3)
  res_loc <- c4h_plotskill(
    skill_loc[[1]],
    skill_loc[[2]],
    legend = "Test Legend"
  )
  expect_s3_class(res_loc, "ggplot")
  res_loc <- c4h_plotskill(skill_loc[[1]], boundaries = "countries")
  expect_s3_class(res_loc, "ggplot")
})

#' .s2dv_to_terra
#'
#' Transform s2dv into terra
#'
#' @param cube The s2dv_cube object to be transformed.
#' @param crs Coordinate Reference System of the s2dv_cube. The default is
#' "EPSG:4326".
#' @param drop Logical. If TRUE, dimensions with only one value will be dropped
#' when converting to the desired format. Default is FALSE.
#'
#' @returns A terra::SpatRaster object.
#' @noRd
.s2dv_to_terra <- function(cube, crs = "EPSG:4326", drop = FALSE) {
  .validate_s2dv_cube(cube)

  lon <- as.vector(cube$coords$longitude)
  lat <- as.vector(cube$coords$latitude)

  # REVIEW - logic copied from c4h_space
  if (lat[1] > lat[length(lat)]) {
    # if latitude is stored north to south, reverse it
    lat <- rev(lat)
    lat_dim <- which(names(dim(cube$data)) == "latitude")
    idx <- lapply(dim(cube$data), seq_len)
    idx[[lat_dim]] <- rev(idx[[lat_dim]])
    cube$data <- do.call(`[`, c(list(cube$data), idx, drop = FALSE))
  }

  d <- dim(cube$data)

  nd <- .get_dim(d, "dataset")
  nv <- .get_dim(d, "var")
  ns <- .get_dim(d, "sdate")
  nt <- .get_dim(d, "time")
  nm <- .get_dim(d, "ensemble")

  # terra is composed of rows(x/latitude), columns(y/longitude) and layers(z)
  # create one layer to each combination of dataset, var, sdate, time, and
  # ensemble
  layers <- list()
  k <- 1

  # REVIEW - nested loops
  for (d in seq_len(nd)) {
    d_lab <- .get_label(cube$coords$dataset, d, "dataset")
    if (nd == 1 && isTRUE(drop)) {
      d_lab <- NULL
    }
    for (v in seq_len(nv)) {
      v_lab <- .get_label(cube$coords$var, v, "var")
      if (nv == 1 && isTRUE(drop)) {
        v_lab <- NULL
      }
      for (s in seq_len(ns)) {
        s_lab <- if (
          !is.null(cube$attrs$Dates) &&
            length(cube$attrs$Dates[, 1]) >= s
        ) {
          paste0("sdate_", format(cube$attrs$Dates[s, 1], "%Y%m%d"))
        } else {
          sprintf("sdate_%02d", s)
        }

        for (t in seq_len(nt)) {
          t_lab <- if (
            !is.null(cube$attrs$Dates) &&
              length(cube$attrs$Dates[s, ]) >= t
          ) {
            paste0("time_", format(cube$attrs$Dates[s, t], "%Y%m%d"))
          } else {
            sprintf("time_%02d", t)
          }

          for (m in seq_len(nm)) {
            vals <- as.vector(cube$data[d, v, s, t, m, , ])
            xyz <- data.frame(
              x = rep(lon, each = length(lat)),
              y = rep(lat, times = length(lon)),
              z = vals
            )
            r <- terra::rast(xyz, type = "xyz", crs = crs)

            m_lab <- .get_label(cube$coords$ensemble, m, "ensemble")
            if (nm == 1 && isTRUE(drop)) {
              m_lab <- NULL
            }

            parts <- list(d_lab, v_lab, s_lab, t_lab, m_lab)
            parts <- parts[
              !vapply(parts, function(x) is.null(x) || x == "", logical(1))
            ]

            names(r) <- paste(parts, collapse = "_")

            layers[[k]] <- r
            k <- k + 1
          }
        }
      }
    }
  }

  # stack layers
  rstack <- do.call(c, layers)
  return(rstack)
}

#' .s2dv_to_df
#'
#' Transform s2dv into data.frame
#'
#' @param cube The s2dv_cube object to be transformed.
#' @param drop Logical. If TRUE, dimensions with only one value will be dropped
#' when converting to the desired format. Default is FALSE.
#' @returns A data.frame
#' @noRd
.s2dv_to_df <- function(cube, drop = FALSE) {
  .validate_s2dv_cube(cube)

  d <- dim(cube$data)

  nd <- .get_dim(d, "dataset")
  nv <- .get_dim(d, "var")
  ns <- .get_dim(d, "sdate")
  nt <- .get_dim(d, "time")
  nm <- .get_dim(d, "ensemble")

  # ---- identify spatial dimensions ----
  core_dims <- c("dataset", "var", "sdate", "time", "ensemble")
  spatial_dims <- setdiff(names(d), core_dims)

  # ---- extract spatial coordinates ----
  spatial_coords <- lapply(spatial_dims, function(dimn) {
    as.vector(cube$coords[[dimn]])
  })
  names(spatial_coords) <- spatial_dims

  # ---- dates and ensemble values ----
  dates <- tryCatch(as.Date(cube$attrs$Dates), error = function(e) NULL)
  ens <- tryCatch(as.vector(cube$coords$ensemble), error = function(e) NULL)

  # ---- build expand.grid arguments ----
  grid_args <- c(
    list(
      dataset = seq_len(nd),
      var = seq_len(nv),
      sdate = seq_len(ns),
      time = seq_len(nt),
      ensemble = seq_len(nm)
    ),
    spatial_coords
  )

  df <- do.call(expand.grid, c(grid_args, list(KEEP.OUT.ATTRS = FALSE)))

  df$value <- as.vector(cube$data)

  if (isTRUE(drop)) {
    for (dimn in c("dataset", "var", "ensemble")) {
      if (length(unique(df[[dimn]])) == 1) {
        df[[dimn]] <- NULL
      }
    }
  }

  # ---- assign dates ----
  if (!is.null(dates)) {
    if (ns > 1 && nt > 1) {
      # Dates 2-D
      df$date <- as.character(dates[
        cbind(df$sdate, df$time)
      ])
    } else if (length(dates) == nt) {
      # Dates 1-D and time dimension
      df$date <- as.character(dates[df$time])
    } else if (length(dates) == ns) {
      # Dates 1-D and sdate dimension
      df$date <- as.character(dates[df$sdate])
    }
  }

  # ---- assign variable names ----
  var_names <- tryCatch(as.vector(cube$coords$var), error = function(e) NULL)
  if (!is.null(var_names) && nv == length(var_names)) {
    if (nv > 1 || !isTRUE(drop)) {
      df$var_name <- var_names[df$var]
    }
  }

  # ---- assign ensemble values ----
  if (!is.null(ens) && length(ens) == nm) {
    if (nm > 1 || !isTRUE(drop)) {
      df$ensemble_value <- ens[df$ensemble]
    }
  }

  # ---- if s2dv_cube contains location ----
  if ("location" %in% names(cube$coords)) {
    df$latitude <- cube$attrs$location$latitude[df$location]
    df$longitude <- cube$attrs$location$longitude[df$location]
  }

  # ---- if s2dv_cube contains area ----
  if ("area" %in% names(cube$coords)) {
    df$area <- as.character(cube$coords$area[df$area])
    if (!is.null(cube$attrs$area_id)) {
      names(df)[names(df) == "area"] <- cube$attrs$area_id
    }
  }

  return(df)
}

#' .s2dv_to_sf
#'
#' Transform s2dv into sf
#'
#' @param cube The s2dv_cube object to be transformed.
#' @param crs Coordinate Reference System (default: "EPSG:4326").
#' @param drop Logical. If TRUE, dimensions with only one value will be dropped
#' when converting to the desired format. Default is FALSE.
#' @returns An sf object with polygons.
#' @noRd
.s2dv_to_sf <- function(cube, crs = "EPSG:4326", drop = FALSE) {
  # Validate cube first
  .validate_s2dv_cube(cube)

  # Spatial coords
  dim_names <- names(cube$dims)
  has_latlon <- all(c("latitude", "longitude") %in% dim_names)
  has_area <- "area" %in% dim_names

  # handle gridded case
  if (has_latlon) {
    rstack <- .s2dv_to_terra(cube, crs = crs, drop = drop)
    pv <- terra::as.polygons(
      rstack,
      dissolve = FALSE,
      values = TRUE,
      na.rm = FALSE
    )
    sf_obj <- sf::st_as_sf(pv)
  } else if (has_area) {
    # Extract area geometries (assumed to be an sf or WKT vector)
    areas <- cube$attrs$area$geom

    d <- dim(cube$data)
    nd <- .get_dim(d, "dataset")
    nv <- .get_dim(d, "var")
    ns <- .get_dim(d, "sdate")
    nt <- .get_dim(d, "time")
    nm <- .get_dim(d, "ensemble")

    layers <- list()
    k <- 1

    for (d in seq_len(nd)) {
      d_lab <- .get_label(cube$coords$dataset, d, "dataset")
      if (nd == 1 && isTRUE(drop)) {
        d_lab <- NULL
      }
      for (v in seq_len(nv)) {
        v_lab <- .get_label(cube$coords$var, v, "var")
        if (nv == 1 && isTRUE(drop)) {
          v_lab <- NULL
        }
        for (s in seq_len(ns)) {
          s_lab <- if (
            !is.null(cube$attrs$Dates) &&
              length(cube$attrs$Dates[, 1]) >= s
          ) {
            paste0("sdate_", format(cube$attrs$Dates[s, 1], "%Y%m%d"))
          } else {
            sprintf("sdate_%02d", s)
          }
          for (t in seq_len(nt)) {
            t_lab <- if (
              !is.null(cube$attrs$Dates) &&
                length(cube$attrs$Dates[s, ]) >= t
            ) {
              paste0("time_", format(cube$attrs$Dates[s, t], "%Y%m%d"))
            } else {
              sprintf("time_%02d", t)
            }
            for (m in seq_len(nm)) {
              m_lab <- .get_label(cube$coords$ensemble, m, "ensemble")
              if (nm == 1 && isTRUE(drop)) {
                m_lab <- NULL
              }

              parts <- list(d_lab, v_lab, s_lab, t_lab, m_lab)
              parts <- parts[
                !vapply(parts, function(x) is.null(x) || x == "", logical(1))
              ]

              layer_name <- paste(parts, collapse = "_")

              # Extract values for all areas at this combination
              vals <- as.vector(cube$data[d, v, s, t, m, ])

              # Build sf data frame from polygon geometries + values
              geom_sf <- sf::st_as_sf(data.frame(
                value = vals,
                geometry = areas
              ))
              sf::st_crs(geom_sf) <- crs

              # Convert to SpatVector for terra
              vect_layer <- terra::vect(geom_sf)
              vect_layer$layer <- layer_name

              layers[[k]] <- vect_layer
              k <- k + 1
            }
          }
        }
      }
    }

    # Stack all SpatVector layers into one
    vstack <- do.call(rbind, layers)
    pv <- terra::as.polygons(vstack)
    sf_obj <- sf::st_as_sf(pv)
  }
  return(sf_obj)
}

#' .get_dim
#' Get dimension size or 1 if not present
#' @param d Named integer vector with dimensions.
#' @param x Name of the dimension to get.
#' @returns Integer size of the dimension or 1 if not present.
#' @noRd
.get_dim <- function(d, x) if (x %in% names(d)) d[[x]] else 1L

#' .get_label
#' Generate a label for a cube dimension value
#' @param x A vector of labels (e.g., variable names, ensemble labels).
#' @param i Index of the value to label.
#' @param prefix Prefix for the label if x is NULL or index is missing.
#' @param width Width for numeric labels (default: 2).
#' @returns A character string label.
#' @noRd
.get_label <- function(x, i, prefix, width = 2) {
  if (!is.null(x) && length(x) >= i) {
    paste0(prefix, "_", x[i])
  } else {
    paste0(prefix, "_", sprintf(paste0("%02d"), i))
  }
}

#' .validate_s2dv_cube
#' Validate an s2dv_cube object.
#' @param cube The s2dv_cube object to validate.
#' @returns Invisibly returns TRUE if the cube is valid; otherwise, stops
#'   with an error.
#' @noRd
.validate_s2dv_cube <- function(cube) {
  stopifnot(inherits(cube, "s2dv_cube"))

  if (is.null(cube$data) || !is.array(cube$data)) {
    stop("`data$data` must be a multidimensional array.")
  }

  if (is.null(cube$coords)) {
    stop("`data$coords` is missing.")
  }

  dim_names <- names(cube$dims)
  core_dims <- c("dataset", "var", "sdate", "time", "ensemble")
  missing_core <- setdiff(core_dims, dim_names)
  if (length(missing_core) > 0) {
    stop(
      "Missing required core dimension(s): ",
      paste(missing_core, collapse = ", ")
    )
  }

  # Spatial validation
  has_latlon <- all(c("latitude", "longitude") %in% dim_names)
  has_location <- "location" %in% dim_names
  has_area <- "area" %in% dim_names

  if (!has_latlon && !has_location && !has_area) {
    stop(paste0(
      "data must contain either (latitude & longitude),",
      " location, or area dimension."
    ))
  }

  # If lat/lon grid
  if (has_latlon) {
    if (length(cube$coords$latitude) != dim(cube$data)[6]) {
      stop("Latitude length does not match data dimension.")
    }
    if (length(cube$coords$longitude) != dim(cube$data)[7]) {
      stop("Longitude length does not match data dimension.")
    }
  }

  # If location index
  if (has_location) {
    if (length(cube$coords$location) != dim(cube$data)[6]) {
      stop("Location length does not match data dimension.")
    }
    if (!is.null(cube$attrs$location)) {
      if (!all(c("latitude", "longitude") %in% names(cube$attrs$location))) {
        stop("`attrs$location` must contain latitude and longitude.")
      }
    }
  }

  # If area index
  if (has_area) {
    if (length(cube$coords$area) != dim(cube$data)[6]) {
      stop("Area length does not match data dimension.")
    }
  }

  # Dates validation
  if (!is.null(cube$attrs$Dates)) {
    if (!is.matrix(cube$attrs$Dates)) {
      stop("`attrs$Dates` must be a matrix.")
    }

    if (
      nrow(cube$attrs$Dates) < dim(cube$data)[3] ||
        ncol(cube$attrs$Dates) < dim(cube$data)[4]
    ) {
      stop(
        "`attrs$Dates` must have [sdate, time] dimensions."
      )
    }
  }

  invisible(TRUE)
}

.s2dv_to_stars <- function(cube, crs = "EPSG:4326", drop = FALSE) {
  # Reorder array
  cubedata <- cube$data
  spatial_dims <- c(
    which(names(dim(cubedata)) == "longitude"),
    which(names(dim(cubedata)) == "latitude")
  )
  cubedata <- aperm(
    cubedata,
    c(spatial_dims, setdiff(seq_along(names(dim(cubedata))), spatial_dims))
  )

  # Drop singletons if needed
  if (isTRUE(drop)) {
    if (dim(cubedata)["longitude"] <= 1 || dim(cubedata)["latitude"] <= 1) {
      message(
        "Longitude and/or latitude dimensions are singletons. ",
        "Please set drop = FALSE if you want to keep spatial information."
      )
    }
    cubedata <- drop(cubedata)
  }

  # Convert to stars and insert coordinate values
  starscube <- stars::st_as_stars(cubedata)
  names(starscube) <- cube$attrs$Variable$varName
  starscube <- stars::st_set_dimensions(
    starscube,
    "longitude",
    values = c(cube$coords$longitude)
  )
  starscube <- stars::st_set_dimensions(
    starscube,
    "latitude",
    values = c(cube$coords$latitude)
  )

  # Apply CRS if the first two dimensions are lon/lat
  if (
    names(dim(starscube))[1] == "longitude" &&
      names(dim(starscube))[2] == "latitude"
  ) {
    sf::st_crs(starscube) <- crs
  }

  return(starscube)
}

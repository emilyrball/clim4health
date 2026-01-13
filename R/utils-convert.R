#' .s2dv_to_terra
#'
#' Transform s2dv into terra
#'
#' @param cube The s2dv_cube object to be transformed.
#' @param crs Coordinate Reference System of the s2dv_cube. The default is
#' "EPSG:4326".
#'
#' @returns A terra::SpatRaster object.
#' @noRd
.s2dv_to_terra <- function(cube, crs = "EPSG:4326") {
  ### REVIEW - requires lat/lon coordinates, OK for now
  ### REVIEW - if lat/lon required, add check for them?
  lon <- as.vector(cube$coords$longitude)
  lat <- as.vector(cube$coords$latitude)

  d  <- dim(cube$data)
  nd <- d[1]
  nv <- d[2]
  ns <- d[3]
  nt <- d[4]
  nm <- d[5]

  layers <- list()
  k <- 1

  # REVIEW - nested loops
  for (d in seq_len(nd)) {
    d_lab <- if (!is.null(cube$coords$dataset) &&
                   length(cube$coords$dataset) >= d) {
      paste0("dataset_", cube$coords$dataset[d])
    } else {
      sprintf("dataset_%02d", d)
    }
    for (v in seq_len(nv)) {
      v_lab <- if (!is.null(cube$coords$var) &&
                     length(cube$coords$var) >= v) {
        paste0("var_", cube$coords$var[v])
      } else {
        sprintf("var_%02d", v)
      }
      for (s in seq_len(ns)) {
        s_lab <- if (!is.null(cube$attrs$Dates) &&
                       length(cube$attrs$Dates[, 1]) >= s) {
          paste0("sdate_", format(cube$attrs$Dates[s, 1], "%Y%m%d"))
        } else {
          sprintf("sdate_%02d", s)
        }
        for (t in seq_len(nt)) {
          t_lab <- if (!is.null(cube$attrs$Dates) &&
                         length(cube$attrs$Dates[s, ]) >= t) {
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

            m_lab <- if (!is.null(cube$coords$ensemble) &&
                           length(cube$coords$ensemble) >= m) {
              paste0("ensemble_", cube$coords$ensemble[m])
            } else {
              sprintf("ensemble_%02d", m)
            }
            names(r) <- paste(d_lab, v_lab, s_lab, t_lab, m_lab, sep = "_")

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

#' .s2dv_to_csv
#'
#' Transform s2dv into csv
#'
#' @param cube The s2dv_cube object to be transformed.
#' @param file Optional file path to save the CSV. If NULL, the data.frame
#' is not saved.
#' @returns A data.frame or writes a CSV file.
#' @noRd
.s2dv_to_csv <- function(cube, file = NULL) {
  ### REVIEW - argument file is redundant as can be used in c4h_save?
  stopifnot(inherits(cube, "s2dv_cube"))

  d  <- dim(cube$data)

  nds  <- .get_dim(d, "dataset")
  nvar <- .get_dim(d, "var")
  nsd  <- .get_dim(d, "sdate")
  nt   <- .get_dim(d, "time")
  nm   <- .get_dim(d, "ensemble")

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
  ens   <- tryCatch(as.vector(cube$coords$ensemble), error = function(e) NULL)

  # ---- build expand.grid arguments ----
  grid_args <- c(
    list(
      dataset  = seq_len(nds),
      var      = seq_len(nvar),
      sdate    = seq_len(nsd),
      time     = seq_len(nt),
      ensemble = seq_len(nm)
    ),
    spatial_coords
  )

  df <- do.call(expand.grid, c(grid_args, list(KEEP.OUT.ATTRS = FALSE)))

  df$value <- as.vector(cube$data)

  # ---- assign dates ----
  if (!is.null(dates)) {
    if (nsd > 1 && nt > 1) {
      # Dates 2-D
      df$date <- as.character(dates[
        cbind(df$sdate, df$time)
      ])
    } else if (length(dates) == nt) {
      # Dates 1-D and time dimension
      df$date <- as.character(dates[df$time])
    } else if (length(dates) == nsd) {
      # Dates 1-D and sdate dimension
      df$date <- as.character(dates[df$sdate])
    }
  }

  # ---- assign variable names ----
  var_names <- tryCatch(as.vector(cube$coords$var), error = function(e) NULL)
  if (!is.null(var_names) && nvar == length(var_names)) {
    df$var_name <- var_names[df$var]
  }

  # ---- assign ensemble values ----
  if (!is.null(ens) && length(ens) == nm) {
    df$ensemble_value <- ens[df$ensemble]
  }

  # ---- if s2dv_cube contains location ----
  if ("location" %in% names(cube$coords)) {
    df$latitude  <- cube$attrs$location$latitude[df$location]
    df$longitude <- cube$attrs$location$longitude[df$location]
  }

  if (!is.null(file)) utils::write.csv(df, file = file, row.names = FALSE)
  return(df)
}

#' .s2dv_to_polygons
#'
#' Transform s2dv into polygons
#'
#' @param cube The s2dv_cube object to be transformed.
#' @param crs Coordinate Reference System (default: "EPSG:4326").
#' @returns An sf object with polygons.
#' @noRd
.s2dv_to_polygons <- function(cube, crs = "EPSG:4326") {

  rstack <- .s2dv_to_terra(cube, crs = crs)
  pv <- terra::as.polygons(rstack, dissolve = FALSE,
                           values = TRUE, na.rm = FALSE)
  sf_obj <- sf::st_as_sf(pv)
  return(sf_obj)
}

#' .terra_to_s2dv
#'
#' Transform terra into s2dv
#'
#' @param rstack The terra::SpatRaster object to be transformed.
#' @param dates  List of the dates you want to assign to the objects
#' format: yyyy-mm-dd. The default is NULL.
#' @param ensemble_vals List of auxiliary values used for ensembles
#' @param variable Name of the variable. The default is "t2m".
#' @param dataset Dataset index. The default is 1.
#' @param sdate Start date index. The default is 1.
#' @returns An s2dv_cube object.
#' @noRd
.terra_to_s2dv <- function(rstack, dates = NULL, ensemble_vals = NULL,
                           variable = "t2m", dataset = 1, sdate = 1) {
  # REVIEW variable = "t2m" as default - shouldn't break but will just name
  # output variable as t2m

  # REVIEW - nicer error message
  stopifnot(inherits(rstack, "SpatRaster"))

  # Data.frame xy + layers
  df <- terra::as.data.frame(rstack, xy = TRUE, cells = FALSE, na.rm = FALSE)
  stopifnot(all(c("x", "y") %in% names(df)))

  layer_names <- names(rstack)
  # build matrix by layer
  layers <- as.list(df[layer_names])
  xy_arr <- .build_array_from_xy_layers(df$x, df$y, layers, lat_desc = TRUE)

  # parsing time/ensemble from names
  pe <- .parse_time_ens(layer_names, dates = dates,
                        ensemble_vals = ensemble_vals)
  nt <- max(pe$time_idx)
  nm <- max(pe$ens_idx)

  # assemble array [dataset, var, sdate, time, ensemble, lat, lon]
  nlat <- length(xy_arr$lat)
  nlon <- length(xy_arr$lon)
  arr <- array(NA_real_, dim = c(dataset, 1, sdate, nt, nm, nlat, nlon))

  for (k in seq_along(xy_arr$mats)) {
    t <- pe$time_idx[k]
    m <- pe$ens_idx[k]
    arr[1, 1, 1, t, m, , ] <- xy_arr$mats[[k]]
  }

  cube <- list(
    data  = arr,
    dims  = c(dataset = dataset, var = 1, sdate = sdate,
              time = nt, ensemble = nm, latitude = nlat, longitude = nlon),
    coords = list(
      dataset   = seq_len(dataset),
      var       = variable,
      sdate     = seq_len(sdate),
      time      = if (!is.null(dates) && length(dates) == nt) {
        seq_len(nt)
      } else {
        seq_len(nt)
      },
      ensemble  = if (!is.null(ensemble_vals)) ensemble_vals else seq_len(nm),
      latitude  = xy_arr$lat,
      longitude = xy_arr$lon
    ),
    attrs = list(Dates = NULL, ### REVIEW
                 Variable = list(varName = variable,
                                 metadata = NULL,
                                 units = NULL,
                                 source_files = NULL),
                 source_files = NULL)
  )
  class(cube) <- "s2dv_cube"
  cube
}

#' .csv_to_s2dv
#'
#' Transform csv into s2dv
#' #' @param df The data.frame object to be transformed.
#' @param value_cols Character vector with the names of the columns
#' containing the values to be transformed. Default is "value".
#' @param var_names Character vector with the names of the variables.
#' If NULL, the names in value_cols are used.
#' @returns A s2dv_cube object.
#' @noRd
.csv_to_s2dv <- function(df, value_cols = "value", var_names = NULL) {
  # Validate basic columns
  needed <- c("latitude", "longitude")
  if (!all(needed %in% names(df))) {
    stop("The CSV should have columns: ", paste(needed, collapse = ", "))
  }
  if (!all(value_cols %in% names(df))) {
    stop("No value columns were found: ", paste(value_cols, collapse = ", "))
  }

  # Create available dimensions
  lat <- sort(unique(df$latitude), decreasing = TRUE)
  lon <- sort(unique(df$longitude))

  # optional dimensions (if none exist, use 1)
  time <- if ("time" %in% names(df)) sort(unique(df$time)) else 1
  ensemble <- if ("ensemble" %in% names(df)) sort(unique(df$ensemble)) else 1

  if (is.null(var_names)) var_names <- value_cols
  nvar <- length(value_cols)

  dims <- c(
    dataset = 1,
    var = nvar,
    sdate = 1,
    time = length(time),
    ensemble = length(ensemble),
    latitude = length(lat),
    longitude = length(lon)
  )

  arr <- array(NA_real_, dim = dims)

  # Fullfill the array
  for (v in seq_along(value_cols)) {
    for (t in seq_along(time)) {
      for (e in seq_along(ensemble)) {
        sub <- df
        if ("time" %in% names(df)) sub <- sub[sub$time == time[t], ]
        if ("ensemble" %in% names(df)) sub <- sub[sub$ensemble == ensemble[e], ]

        mat <- matrix(NA, nrow = length(lat), ncol = length(lon))
        for (i in seq_along(lat)) {
          for (j in seq_along(lon)) {
            val <- sub[sub$latitude == lat[i] & sub$longitude == lon[j],
                       value_cols[v]]
            if (length(val) == 1) mat[i, j] <- val
          }
        }
        arr[1, v, 1, t, e, , ] <- mat
      }
    }
  }

  # Create s2dv object
  cube <- list(
    data = arr,
    dims = dims,
    coords = list(
      dataset = 1,
      var = var_names,
      sdate = 1,
      time = time,
      ensemble = ensemble,
      latitude = lat,
      longitude = lon
    ),
    attrs = list(Dates = NULL, ### REVIEW
                 Variable = list(varName = var_names,
                                 metadata = NULL,
                                 units = NULL,
                                 source_files = NULL),
                 source_files = NULL)
  )
  class(cube) <- "s2dv_cube"
  return(cube)
}

#' .polygons_to_s2dv
#'
#' Transform polygons into s2dv
#' @param sf_polys The sf object with polygons to be transformed.
#' @param dates  List of the dates you want to assign to the objects
#' format: yyyy-mm-dd. The default is NULL.
#' @param ensemble_vals List of auxiliary values used for ensembles
#' @param variable Name of the variable. The default is "t2m".
#' @param round_digits Number of digits to round the coordinates. Default is 6.
#' @returns A s2dv_cube object.
#' @noRd
.polygons_to_s2dv <- function(sf_polys,
                              dates = NULL,
                              ensemble_vals = NULL,
                              variable = "t2m",
                              round_digits = 6) {

  # REVIEW - nice error message
  stopifnot(inherits(sf_polys, "sf"))
  ### REVIEW - can be removed?
  #if (!requireNamespace("sf", quietly = TRUE)) stop("Instala 'sf'.")

  # 1) Centers of each cell based on the bbox (robust and simple)
  b <- do.call(
    rbind,
    lapply(sf::st_geometry(sf_polys), sf::st_bbox)
  )
  xmid <- round((b[, "xmin"] + b[, "xmax"]) / 2, round_digits)
  ymid <- round((b[, "ymin"] + b[, "ymax"]) / 2, round_digits)

  # 2) regular mesh (long ascending, short descending)
  lon_u <- sort(unique(xmid))
  lat_u <- sort(unique(ymid), decreasing = TRUE)
  nlon <- length(lon_u)
  nlat <- length(lat_u)

  lon_idx <- match(xmid, lon_u)
  lat_idx <- match(ymid, lat_u)

  # 3) Layers = numerical columns (in the order they appear)
  num_cols <- names(sf_polys)[sapply(sf_polys, is.numeric)]
  num_cols <- setdiff(num_cols, attr(sf_polys, "sf_column"))
  # REVIEW - clearer error message
  if (!length(num_cols)) stop("There are no numeric columns in 'sf_polys'.")

  len_col  <- length(num_cols)
  nt <- if (!is.null(dates)) {
    length(dates)
  } else {
    # if there are no dates, everything is a single time
    # (or several if you give nm)
    len_col
  }
  nm <- if (!is.null(ensemble_vals)) {
    length(ensemble_vals)
  } else {
    ceiling(len_col / nt)
  }


  # 4) Build the array [dataset, var, sdate, time, ensemble, lat, lon]
  arr <- array(NA_real_, dim = c(1, 1, 1, nt, nm, nlat, nlon))

  k <- 1L
  for (t in seq_len(nt)) {
    for (m in seq_len(nm)) {
      vals <- sf_polys[[num_cols[k]]]
      mat <- matrix(NA_real_, nrow = nlat, ncol = nlon)
      mat[cbind(lat_idx, lon_idx)] <- vals
      arr[1, 1, 1, t, m, , ] <- mat
      k <- k + 1L
    }
  }

  # 5) make the s2dv_cube
  ### REVIEW - forcing sdate = 1
  cube <- list(
    data  = arr,
    dims  = c(dataset = 1, var = 1, sdate = 1,
              time = nt, ensemble = nm, latitude = nlat, longitude = nlon),
    coords = list(
      dataset   = 1,
      var       = variable,
      sdate     = 1,
      time      = seq_len(nt),
      ensemble  = if (!is.null(ensemble_vals)) ensemble_vals else seq_len(nm),
      latitude  = lat_u,
      longitude = lon_u
    ),
    attrs = list(Dates    = if (!is.null(dates)) as.Date(dates) else NULL,
                 Variable = list(varName = variable,
                                 metadata = NULL,
                                 units = NULL,
                                 source_files = NULL),
                 source_files = NULL) ### REVIEW
  )
  class(cube) <- "s2dv_cube"
  cube
}

#' .parse_time_ens
#'
#' Parse time and ensemble from layer names
#' @param layer_names Character vector with the names of the layers.
#' @param dates  List of the dates you want to assign to the objects
#' format: yyyy-mm-dd. The default is NULL.
#' @param ensemble_vals List of auxiliary values used for ensembles
#' @returns A list with time_idx, ens_idx, time_labels and ens_labels.
#' @noRd
.parse_time_ens <- function(layer_names, dates = NULL, ensemble_vals = NULL) {
  # Return list(time_idx, ens_idx, time_labels, ens_labels)
  time_idx <- integer(length(layer_names))
  ens_idx  <- integer(length(layer_names))
  time_labels <- character(0)
  ens_labels  <- character(0)

  rx <- "^time_([0-9]{8}|[0-9]+)_ens_(.+)$"

  parsed <- vapply(layer_names, function(nm) grepl(rx, nm), logical(1))
  if (all(parsed)) {
    # extract tokens
    t_tok <- sub(rx, "\\1", layer_names)
    e_tok <- sub(rx, "\\2", layer_names)

    if (is.null(dates)) {
      # generate unique levels in order of appearance
      time_labels <- unique(t_tok)
      time_idx <- match(t_tok, time_labels)
    } else {
      # map provided dates
      ymd <- tryCatch(format(as.Date(dates), "%Y%m%d"), # REVIEW date format
                      error = function(e) NULL)
      if (!is.null(ymd)) {
        time_idx <- match(t_tok, ymd)
        time_labels <- ymd
      } else {
        # numeric fallback
        time_idx <- as.integer(t_tok)
        time_labels <- as.character(seq_along(dates))
      }
    }

    if (is.null(ensemble_vals)) {
      ens_labels <- unique(e_tok)
      ens_idx <- match(e_tok, ens_labels)
    } else {
      ens_idx <- match(e_tok, as.character(ensemble_vals))
      ens_labels <- as.character(ensemble_vals)
    }
  } else {
    # No pattern: assume order layers by (time, ensemble)
    if (is.null(ensemble_vals)) {
      ens_labels <- as.character(seq_len(length(layer_names)))
    }
    # If you pass us dates / ensemble, we use their lengths
    nt <- if (!is.null(dates)) length(dates) else NA_integer_
    # Attempt to factorize by a plausible nm:
    nm_guess <- if (!is.null(ensemble_vals)) length(ensemble_vals) else 1L
    if (is.na(nt)) {
      # deduct nt by integer division
      nt <- ceiling(length(layer_names) / nm_guess)
    }
    nm <- ceiling(length(layer_names) / nt)

    k <- seq_along(layer_names)
    time_idx <- ((k - 1) %/% nm) + 1
    ens_idx  <- ((k - 1) %%  nm) + 1

    if (is.null(dates)) {
      time_labels <- as.character(seq_len(nt))
    } else {
      time_labels <- format(as.Date(dates), "%Y%m%d") # REVIEW - use as.POSIXct?
    }
    if (is.null(ensemble_vals)) {
      ens_labels <- as.character(seq_len(nm))
    } else {
      ens_labels <- as.character(ensemble_vals)
    }
  }

  list(time_idx = time_idx,
       ens_idx  = ens_idx,
       time_labels = time_labels,
       ens_labels  = ens_labels)
}

#' .build_array_from_xy_layers
#'
#' Build array from x,y and list of layers
#' @param x Numeric vector with the x (longitude) coordinates.
#' @param y Numeric vector with the y (latitude) coordinates.
#' @param layer_list Named list of numeric vectors (each length ncell).
#' @param lat_desc Logical indicating if latitude is in descending order.
#' @returns A list with arr, lon_vec and lat_vec.
#' @noRd
.build_array_from_xy_layers <- function(x, y, layer_list, lat_desc = TRUE) {
  # x: lon vector (repeated by cells), y: lat vector, length ncell
  # layer_list: named list of numeric vectors (each length ncell)
  # Returns list(arr, lon_vec, lat_vec) with arr dims [time, ensemble, lat, lon]
  stopifnot(length(unique(x)) * length(unique(y)) == length(x))

  lon_vec <- sort(unique(x))
  # lat in many rasters is north->south (desc). We leave it selective:
  lat_vec_inc <- sort(unique(y))
  lat_vec <- if (lat_desc) rev(lat_vec_inc) else lat_vec_inc

  # sort cells by increasing length, then increasing width (rapid width)
  ord <- order(x, y)
  nx <- length(lon_vec)
  ny <- length(lat_vec_inc)

  # function to convert a flat vector to a [lat, lon] matrix with increasing lat
  to_mat_inc <- function(v) matrix(v[ord], nrow = ny, ncol = nx, byrow = FALSE)
  to_mat <- function(v) {
    m <- to_mat_inc(v)
    if (lat_desc) m <- m[ny:1, , drop = FALSE]
    m
  }

  mats <- lapply(layer_list, to_mat)
  # join array [time, ensemble, lat, lon] later based on indexes
  list(mats = mats, lon = lon_vec, lat = lat_vec)
}

#' .get_dim
#' Get dimension size or 1 if not present
#' @param d Named integer vector with dimensions.
#' @param x Name of the dimension to get.
#' @returns Integer size of the dimension or 1 if not present.
#' @noRd
.get_dim <- function(d, x) if (x %in% names(d)) d[[x]] else 1L

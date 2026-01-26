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

  nd <- .get_dim(d, "dataset")
  nv <- .get_dim(d, "var")
  ns <- .get_dim(d, "sdate")
  nt <- .get_dim(d, "time")
  nm <- .get_dim(d, "ensemble")

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
  ens   <- tryCatch(as.vector(cube$coords$ensemble), error = function(e) NULL)

  # ---- build expand.grid arguments ----
  grid_args <- c(
    list(
      dataset  = seq_len(nd),
      var      = seq_len(nv),
      sdate    = seq_len(ns),
      time     = seq_len(nt),
      ensemble = seq_len(nm)
    ),
    spatial_coords
  )

  df <- do.call(expand.grid, c(grid_args, list(KEEP.OUT.ATTRS = FALSE)))

  df$value <- as.vector(cube$data)

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
  ### REVIEW - requires lat/lon coordinates, should be able to input
  ### REVIEW - other s2dv_cube types? in particular area.
  rstack <- .s2dv_to_terra(cube, crs = crs)
  pv <- terra::as.polygons(rstack, dissolve = FALSE,
                           values = TRUE, na.rm = FALSE)
  sf_obj <- sf::st_as_sf(pv)
  return(sf_obj)
}

#' .get_dim
#' Get dimension size or 1 if not present
#' @param d Named integer vector with dimensions.
#' @param x Name of the dimension to get.
#' @returns Integer size of the dimension or 1 if not present.
#' @noRd
.get_dim <- function(d, x) if (x %in% names(d)) d[[x]] else 1L

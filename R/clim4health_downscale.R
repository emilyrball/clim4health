# currently CSDownscale is not available in CRAN
source(paste0("https://earth.bsc.es/gitlab/es/csdownscale/-/raw/",
              "master/R/Analogs.R"))
source(paste0("https://earth.bsc.es/gitlab/es/csdownscale/-/raw/",
              "master/R/Interpolation.R"))
source(paste0("https://earth.bsc.es/gitlab/es/csdownscale/-/raw/",
              "master/R/Intbc.R"))
source(paste0("https://earth.bsc.es/gitlab/es/csdownscale/-/raw/",
              "master/R/Intlr.R"))
source(paste0("https://earth.bsc.es/gitlab/es/csdownscale/-/raw/",
              "master/R/LogisticReg.R"))
source(paste0("https://earth.bsc.es/gitlab/es/csdownscale/-/raw/",
              "master/R/Utils.R"))

#' Downscale and calibrate climate data...
#'
#' @param downscale_function The downscaling function to use. Options are:
#' "Interpolation", "Intbc", "Intlr", "Analogs".
#' @param exp The experimental data to downscale. It must be an s2dv_cube.
#' @param obs The observational data to downscale. It must be an s2dv_cube.
#' @param exp_cor Optional data to downscale. It must be an s2dv_cube. If the
#' provided, it will be downscaled using exp and obs; if not provided, the exp
#' will be downscaled instead. The default value is NULL.
#' @param region a numeric vector indicating the borders of the region defined
#' in obs. It consists of four elements in this order: lonmin, lonmax, latmin,
#' latmax. lonmin refers to the left border, while lonmax refers to the right
#' border. latmin indicates the lower border, whereas latmax indicates the
#' upper border. If set to NULL (default), the function clim4health_downscale
#' will take the minimum and maximum values of the latitudes and longitudes.
#' @param ncores an integer indicating the number of cores to use in parallel
#' computation. The default value is NULL.
#' @param target_grid a character vector indicating the target grid (in the case
#' of Interpolation techniques) or the coarse grid (in the case of Analogs) to
#' be passed to CDO. It must be a grid recognised by CDO or a NetCDF file. The
#' default value is NULL.
#' @param bc_method a character vector indicating the bias adjustment method to
#' be applied after an interpolation. Accepted methods are 'quantile_mapping',
#' 'dynamical_bias', 'bias', 'evmos', 'mse_min', 'crps_min', 'rpc-based'. The
#' abbreviations 'dbc','qm' can also be used. The default value is NULL.
#' @param method_remap a character vector indicating the regridding method to
#' be passed to CDORemap. Accepted methods are "con", "bil", "bic", "nn",
#' "con2". If "nn" method is to be used, CDO_1.9.8 or newer version is required.
#' The default value is NULL.
#' @param points a list of two elements containing the point latitudes and
#' longitudes of the locations to downscale the model data. The list must
#' contain the two elements named as 'latitude' and 'longitude'. If the
#' downscaling is to a point location, only regular grids are allowed for exp
#' and obs. Only needed if the downscaling is to a point location.
#' @param method_point_interp a character vector indicating the interpolation
#' method to interpolate model gridded data into the point locations. Accepted
#' methods are "nearest", "bilinear", "9point", "invdist4nn". Only needed if the
#' downscaling is to a point location.
#' @param loocv a logical indicating whether to apply leave-one-out
#' cross-validation when applying the bias correction. Default to FALSE.
#' @param lr_method a character vector indicating the linear regression method
#' to be applied. Accepted methods are 'basic', 'large-scale' and '9nn'. The
#' 'basic' method fits a linear regression using high resolution observations as
#' predictands and the interpolated model data as predictor. Then, the
#' regression equation is applied to the interpolated model data to correct the
#' interpolated values. The 'large-scale' method fits a linear regression with
#' large-scale predictors (e.g. teleconnection indices) as predictors and
#' high-resolution observations as predictands. This equation is then applied
#' to the interpolated model values. Finally, the '9nn' method uses a linear
#' regression with the nine nearest neighbours as predictors and high-resolution
#' observations as predictands. Instead of constructing a regression model using
#' all nine predictors, principal component analysis is applied to the data of
#' neighboring grids to reduce the dimension of the predictors. The linear
#' regression model is then built using the principal components that explain
#' 95% of the variance. The '9nn' method does not require a pre-interpolation
#' process. The default value is NULL.
#' @param predictors an array with large-scale data to be used in the
#' 'large-scale' method with 'Intlr'. Only needed if the linear regression
#' method is set to 'large-scale'. It must have, at least the dimension start
#' date and another dimension whose name has to be specified in the parameter
#' 'large_scale_predictor_dimname'. It should contain as many elements as the
#' number of large-scale predictors. The default value is NULL.
#' @param large_scale_predictor_dimname a character vector indicating the name
#' of the dimension in 'predictors' that contain the predictor variables. See
#' parameter 'predictors'.
#' @param obsL an 's2dv_cube' object with named dimensions containing the
#' observational field of the large-scale variable. The object must have, at
#' least, the dimensions latitude, longitude and sdate. The object is expected
#' to be already subset for the desired region.
#' @param nanalogs an integer indicating the number of analogs to be searched.
#' The default value is 3. It is only used if the downscaling function is set
#' to "Analogs".
#' @param fun_analog a function to be applied over the found analogs. Valid
#' options: "mean", "wmean", "max", "min", "median" or NULL. If set to NULL
#' (default), the function returns the found analogs.
#' @param metric a character vector to select the analog specification method.
#' Valid options: "dist" (i.e., Euclidian distance), "dcor" (i.e., distance
#' correlation) or "cor" (i.e., Spearman's .correlation). The default metric
#' is "dist". Only used if the downscaling function is set to "Analogs".
#' @param return_indices a logical vector indicating whether to return the
#' indices of the analogs together with the downscaled fields. Default to
#' FALSE. Only used if the downscaling function is set to "Analogs".
#' @param loocv_window a logical vector only to be used if 'obs' does not have
#' the dimension 'window'. It indicates whether to apply leave-one-out
#' cross-validation in the creation of the window. It is recommended to be set
#' to TRUE. Default to TRUE. Only used if the downscaling function is set to
#' "Analogs".
#'
#' @return TBD
#' @export
#'
#' @examples
#' TBD
clim4health_downscale <- function(downscale_function,
  # overlapping inputs for all CSDownscale functions
  exp, obs = NULL, exp_cor = NULL, region = NULL, ncores = NULL,
  target_grid = NULL,
  # additional inputs for Intbc
  bc_method = NULL, method_remap = NULL, points = NULL,
  method_point_interp = NULL, loocv = FALSE,
  # additional inputs for Intlr
  lr_method = NULL, predictors = NULL,
  large_scale_predictor_dimname = "vars",
  # additional inputs for Analogs
  obsL = NULL, nanalogs = 3, fun_analog = NULL,
  metric = "dist", return_indices = FALSE, loocv_window = TRUE, ...
) {
  # clim4health s2dv_cube objects will have the following dimensions
  # to input into CSDownscale functions
  sdate_dim <- "sdate"
  time_dim <- "time"
  member_dim <- "ensemble"
  lat_dim <- "latitude"
  lon_dim <- "longitude"

  if (is.null(exp)) {
    stop("Parameter 'exp' must be specified. It must be an s2dv_cube",
         " object loaded and transformed using clim4health functions.")
  } else if (!downscale_function %in% c("Interpolation", "Intbc",
                                        "Intlr", "Analogs")) {
    stop("Please specify a downscaling function with 'downscale_function'.",
         " Options are: 'Interpolation', 'Intbc', 'Analogs', 'Intlr'.")
  }

  if (downscale_function %in% c("Interpolation", "Intbc", "Intlr")) {
    if (is.null(target_grid) && is.null(points)) {
      stop(paste0("If using '", downscale_function, "', either 'points' or ",
                  "'target_grid' must be specified. 'target_grid' can be ",
                  "either a path to another NetCDF file which to read the ",
                  "target grid from (a single grid must be defined in such ",
                  "file) or a character vector indicating the coarse grid to ",
                  "be passed to CDO, and it must be a grid recognised by CDO ",
                  "or a NetCDF file. 'points' must be a list of two elements ",
                  "containing the point latitudes and longitudes of the ",
                  "locations to downscale the model data. The list must ",
                  "contain the two elements named as indicated in the ",
                  "parameters 'lat_dim' and 'lon_dim'. If the downscaling is ",
                  "to a point location, only regular grids are allowed for ",
                  "exp and obs. ",
                  "Only needed if the downscaling is to a point location."))
    }
    if (is.null(method_point_interp) && !is.null(points)) {
      stop(paste0("If using '", downscale_function, "' and 'points', ",
                  "'method_point_interp' must be specified.",
                  " 'method_point_interp' must be a character vector ",
                  "indicating the interpolation method to interpolate model ",
                  "gridded data into the point locations. Accepted methods are",
                  " 'nearest', 'bilinear', '9point', 'invdist4nn'."))
    } else if (is.null(method_remap) && !is.null(target_grid)) {
      stop(paste0("If using '", downscale_function, "' and 'target_grid', ",
                  "'method_remap' must be specified. 'method_remap'",
                  " must be a character vector indicating the regridding ",
                  "method. Accepted methods are",
                  " 'con', 'bil', 'bic', 'nn', 'con2'."))
    }
  }

  # catch missing arguments for each downscaling function
  if (downscale_function == "Intbc") {
    if (is.null(bc_method)) {
      stop("If using 'Intbc', 'bc_method' must be specified. Options ",
           "are: 'quantile_mapping', 'dynamical_bias', 'bias', 'evmos',",
           "'mse_min', 'crps_min', 'rpc-based'. Abbreviations 'dbc', ",
           "'qm' are also accepted.")
    }
  }
  if (downscale_function == "Intlr") {
    if (is.null(lr_method)) {
      stop("If using 'Intlr', 'lr_method' must be specified. Options ",
           "are: 'basic', 'large-scale' and '9nn'.")
    }
  }

  if (downscale_function %in% c("Intbc", "Intlr", "Analogs")) {
    if (is.null(obs)) {
      stop(paste0("If using '", downscale_function, "', 'obs' must be ",
                  "specified. It must be an s2dv_cube object loaded and ",
                  "transformed using clim4health functions."))
    }
  }

  if (downscale_function == "Analogs") {
    if (is.null(target_grid)) {
      stop("If using 'Analogs', 'target_grid' must be specified. 'target_grid'",
           "can either be a character vector with a path to an example file of",
           " the exp data. It can be either a path to another NetCDF file ",
           "which to read the target grid from (a single grid must be defined ",
           "in such file) or a character vector indicating the coarse grid to ",
           "be passed to CDO, and it must be a grid recognised by CDO.")
    }
  }

  # cases of bugs in CSDownscale
  if (!is.null(points)) {
    if (is.null(region)) {
      region <- c(min(exp$coords$longitude),
                  max(exp$coords$longitude),
                  min(exp$coords$latitude),
                  max(exp$coords$latitude))
    }

    if (downscale_function == "Intlr" && is.null(method_remap)) {
      # assign default method_remap which is not used
      method_remap <- "bil"
    }
    # create temporary netCDF file with reduced dimensions if needed
    source_file_exp <- exp$attrs$source_files[1]

    tmp_file_exp <- .reduce_source_file(source_file_exp)
    exp$attrs$source_files[1] <- tmp_file_exp

    if (!is.null(obs)) {
      source_file_obs <- NULL
      if (is.na(match("location", names(dim(obs$data))))) {
        # case where obs is not a point location
        source_file_obs <- obs$attrs$source_files[1]
        tmp_file_obs <- .reduce_source_file(source_file_obs)
        obs$attrs$source_files[1] <- tmp_file_obs
      }
    }
    if (!is.null(exp_cor)) {
      source_file_exp_cor <- exp_cor$attrs$source_files[1]
      exp_cor$attrs$source_files[1] <- tmp_file_exp
    }
  }


  spatial_type <- .determine_spatial_type(exp$data)
  if (spatial_type == "region") {
    stop("Cannot downscale if data has been aggregated to regions.")
  }

  if (is.null(ncores)) {
    # if ncores is not specified, use third of the available cores
    ncores <- max(floor(parallel::detectCores() / 3), 1)
  } else if (ncores > (parallel::detectCores() - 1)) {
    # if ncores is greater than the number of available cores, stop
    stop("ncores cannot be equal to or greater than the available cores.")
  }

  exp_dims <- names(dim(exp$data))
  obs_dims <- names(dim(obs$data))
  if (!is.null(exp_cor)) {
    exp_dims <- dim(exp_cor$data)
  }
  if (!is.null(points)) {
    exp_dims <- c(exp_dims[!exp_dims %in% c("longitude", "latitude")],
                  "location")
    if (is.na(match("location", names(dim(obs$data))))) {
      # case of gridded obs data
      obs_dims <- c(obs_dims[!obs_dims %in% c("longitude", "latitude")],
                    "location")
    }
  }
  if (downscale_function == "Interpolation") {
    out <- CST_Interpolation(exp = exp, points = points,
                             method_remap = method_remap,
                             target_grid = target_grid,
                             lat_dim = lat_dim,
                             lon_dim = lon_dim,
                             region = region,
                             method_point_interp = method_point_interp,
                             ncores = ncores)

    # CST_Interpolation returns list with reordered s2dv_cube objects

    if ("aux" %in% names(dim(out$exp$data))) {
      dim <- which(names(dim(out$exp$data)) == "aux")
      out$exp$data <- drop_dim(out$exp$data, dim)
    }
    out$exp$data <- s2dv::Reorder(out$exp$data, order = exp_dims)

    out$exp$dims <- dim(out$exp$data)
  } else if (downscale_function == "Intbc") {
    out <- CST_Intbc(exp = exp, obs = obs, exp_cor = exp_cor,
                     target_grid = target_grid, bc_method = bc_method,
                     int_method = method_remap, points = points,
                     method_point_interp = method_point_interp,
                     lat_dim = lat_dim, lon_dim = lon_dim,
                     sdate_dim = sdate_dim, member_dim = member_dim,
                     time_dim = time_dim, region = region,
                     ncores = ncores, loocv = loocv, ...)

    # CST_Intbc returns list with reordered s2dv_cube objects
    if ("aux" %in% names(dim(out$exp$data))) {
      dim <- which(names(dim(out$exp$data)) == "aux")
      out$exp$data <- drop_dim(out$exp$data, dim)
    }
    if ("aux" %in% names(dim(out$obs$data))) {
      dim <- which(names(dim(out$obs$data)) == "aux")
      out$obs$data <- drop_dim(out$obs$data, dim)
    }

    out$exp$data <- s2dv::Reorder(out$exp$data, order = exp_dims)
    out$obs$data <- s2dv::Reorder(out$obs$data, order = obs_dims)

    out$exp$dims <- dim(out$exp$data)
    out$obs$dims <- dim(out$obs$data)
  } else if (downscale_function == "Intlr") {
    out <- CST_Intlr(exp = exp, obs = obs, exp_cor = exp_cor,
                     lr_method = lr_method, target_grid = target_grid,
                     points = points, int_method = method_remap,
                     method_point_interp = method_point_interp,
                     predictors = predictors,
                     lat_dim = lat_dim, lon_dim = lon_dim,
                     sdate_dim = sdate_dim, time_dim = time_dim,
                     member_dim = member_dim, large_scale_predictor_dimname =
                       large_scale_predictor_dimname,
                     loocv = loocv, region = region,
                     ncores = ncores)
    if ("aux" %in% names(dim(out$exp$data))) {
      dim <- which(names(dim(out$exp$data)) == "aux")
      out$exp$data <- drop_dim(out$exp$data, dim)
    }
    if ("aux" %in% names(dim(out$obs$data))) {
      dim <- which(names(dim(out$obs$data)) == "aux")
      out$obs$data <- drop_dim(out$obs$data, dim)
    }

    # CST_Intlr returns list with reordered s2dv_cube objects
    out$exp$data <- s2dv::Reorder(out$exp$data, order = exp_dims)
    out$obs$data <- s2dv::Reorder(out$obs$data, order = obs_dims)

    out$exp$dims <- dim(out$exp$data)
    out$obs$dims <- dim(out$obs$data)
  } else if (downscale_function == "Analogs") {
    out <- CST_Analogs(exp = exp, obs = obs, obsL = obsL,
                       grid_exp = target_grid, nanalogs = nanalogs,
                       fun_analog = fun_analog,
                       lat_dim = lat_dim, lon_dim = lon_dim,
                       sdate_dim = sdate_dim, time_dim = time_dim,
                       member_dim = member_dim, metric = metric,
                       region = region, return_indices = return_indices,
                       loocv_window = loocv_window, ncores = ncores)

    # CST_Analogs returns list with reordered s2dv_cube objects
    if ("aux" %in% names(dim(out$exp$data))) {
      dim <- which(names(dim(out$exp$data)) == "aux")
      out$exp$data <- drop_dim(out$exp$data, dim)
    }
    if ("aux" %in% names(dim(out$obs$data))) {
      dim <- which(names(dim(out$obs$data)) == "aux")
      out$obs$data <- drop_dim(out$obs$data, dim)
    }
    out$exp$data <- s2dv::Reorder(out$exp$data, order = c(exp_dims, "analogs"))
    out$obs$data <- s2dv::Reorder(out$obs$data, order = obs_dims)

    out$exp$dims <- dim(out$exp$data)
    out$obs$dims <- dim(out$obs$data)
  }

  if (!is.null(points)) {
    if (is.null(exp_cor)) {
      if (out$exp$attrs$source_files[1] != source_file_exp) {
        out$exp$attrs$source_files[1] <- source_file_exp
        system(paste0("rm ", tmp_file_exp))
      }
    } else {
      if (out$exp$attrs$source_files[1] != source_file_exp_cor) {
        out$exp$attrs$source_files[1] <- source_file_exp_cor
        system(paste0("rm ", tmp_file_exp))
      }
    }
    if (!is.null(obs) && !is.null(source_file_obs)) {
      out$obs$attrs$source_files[1] <- source_file_obs
      system(paste0("rm ", tmp_file_obs))
    }
  }
  return(out)
}


drop_dim <- function(data, dim) {
  dims <- dim(data)
  if (dims[dim] != 1) stop("Cannot drop a dimension that has length > 1")

  index_list <- lapply(dims, function(d) quote(expr = ))
  index_list[[dim]] <- 1

  data <- do.call(`[`, c(list(data), index_list, list(drop = FALSE)))

  dim(data) <- dims[-dim]
  return(data)
}

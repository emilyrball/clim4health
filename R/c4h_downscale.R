#' c4h_downscale
#'
#' Downscale and calibrate climate data.
#'
#' @param downscale_function The downscaling function to use. Options are:
#' "Interpolation", "Intbc", "Intlr", "Analogs".
#' @param exp The experimental data to downscale. It must be an s2dv_cube.
#' @param obs The observational data to downscale. It must be an s2dv_cube.
#' @param exp_cor Optional data to downscale. It must be an s2dv_cube. If
#' provided, it will be downscaled using exp and obs; if not provided, the exp
#' will be downscaled instead. The default value is NULL.
#' @param bbox a numeric vector indicating the borders of the region defined
#' in obs. It consists of four elements in this order: North, West, South,
#' East. If set to NULL (default), the function c4h_downscale
#' will take the minimum and maximum values of the latitudes and longitudes.
#' @param ncores an integer indicating the number of cores to use in parallel
#' computation. The default value is NULL.
#' @param target_grid a character vector indicating the target grid (in the case
#' of Interpolation techniques) or the coarse grid (in the case of Analogs) to
#' be passed to CDO. It must be a grid recognised by CDO or a NetCDF file. The
#' default value is NULL. If NULL, the grid of obs will be used as the target
#' grid for Interpolation techniques, and the grid of exp will be used as the
#' coarse grid for Analogs.
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
#' @param loocv a logical indicating whether to apply leave-one-out
#' cross-validation when applying the bias correction. Default to TRUE.
#' @param ... additional arguments to be passed to the CSDownscale function.
#' @returns A list of statistically downscaled data as s2dv_cube objects.
#' @export
#'
#' @examples
#' \donttest{
#' hcst <- c4h_load(system.file("extdata", "hindcast", package = "clim4health"),
#'                  variable = "t2m",
#'                  year = 2010:2012,
#'                  month = 1,
#'                  leadtime_month = 1:3,
#'                  ext = "nc")
#' lats <- hcst$coords$latitude
#' lons <- hcst$coords$longitude
#' obs <- c4h_load(system.file("extdata", "reanalysis",
#'                 package = "clim4health"),
#'                 variable = "t2m",
#'                 year = 2010:2012,
#'                 month = 1,
#'                 leadtime_month = 1:3,
#'                 bbox = c(max(lats), min(lons),
#'                          min(lats), max(lons)),
#'                 ext = "nc")
#' ### Example with gridded data
#' dwn <- c4h_downscale("Intbc", exp = hcst, obs = obs,
#'                      method_remap = "bilinear",
#'                      bc_method = "qm",
#'                      target_grid = system.file("extdata",
#'                                                "reanalysis/t2m_201001.nc",
#'                                                package = "clim4health"))
#' ## Example with polygon data
#' shp_path <- system.file("extdata", "areas", "munip_vallecauca.gpkg",
#'                         package = "clim4health")
#' obs_agg <- c4h_space(obs,
#'                      areas = shp_path,
#'                      fun = "mean",
#'                      areas_id = "munip_code")
#' hcst_agg <- c4h_space(hcst,
#'                       areas = shp_path,
#'                       fun = "mean",
#'                       areas_id = "munip_code")
#' hcst_agg_cal <- c4h_downscale(downscale_function = "Intbc",
#'                               exp = hcst_agg,
#'                               obs = obs_agg,
#'                               bc_method = "evmos")
#' ## Example with station data
#' point_locs <- clim4health::c4h_load(system.file("extdata",
#'                                                "stations",
#'                                                package = "clim4health"),
#'                                     ext = "csv",
#'                                     year = 2010:2012,
#'                                     month = 1,
#'                                     leadtime_month = 1:3,
#'                                     variable = "temp_mean")
#' points <- list(latitude = point_locs$attrs$location$latitude,
#'                longitude = point_locs$attrs$location$longitude)
#' point_locs <- c4h_time(point_locs,
#'                        time_aggregation = "monthly",
#'                        dim_aggregation = "sdate",
#'                        fun = "mean")
#'
#' dwn <- c4h_downscale("Interpolation", exp = hcst,
#'                      points = points, method_point_interp = "bilinear")
#' }
c4h_downscale <- function(
  downscale_function,
  exp,
  obs = NULL,
  exp_cor = NULL,
  bbox = NULL,
  ncores = NULL,
  target_grid = NULL,
  bc_method = NULL,
  method_remap = NULL,
  points = NULL,
  method_point_interp = NULL,
  lr_method = NULL,
  loocv = FALSE,
  ...
) {
  # clim4health s2dv_cube objects will have the following dimensions
  # to input into CSDownscale functions
  sdate_dim <- "sdate"
  time_dim <- "time"
  member_dim <- "ensemble"
  lat_dim <- "latitude"
  lon_dim <- "longitude"

  # get required dimensions for exp and obs and exp_cor
  exp_dims <- names(dim(exp$data))
  obs_dims <- names(dim(obs$data))
  if (!is.null(exp_cor)) {
    exp_dims <- names(dim(exp_cor$data))
  }

  if (!inherits(exp, "s2dv_cube")) {
    stop(
      "Parameter 'exp' must be an s2dv_cube object loaded and",
      " transformed using clim4health functions."
    )
  }

  # check spatial format
  if (!all(c("latitude", "longitude") %in% names(exp$dims))) {
    if (!("area" %in% names(exp$dims))) {
      stop(
        "The 'exp' s2dv_cube object must have 'latitude' and 'longitude'",
        "or 'area' dimensions."
      )
    } else {
      # case where data has been aggregated to regions
      message(
        "The 'exp' s2dv_cube object has been aggregated to regions ",
        "and has an 'area' dimension. Only bias adjustment or ",
        "calibration will be applied, no spatial downscaling."
      )
      if (downscale_function != "Intbc") {
        stop(
          "The 'exp' s2dv_cube object has been aggregated to regions ",
          "and has an 'area' dimension. Method '",
          downscale_function,
          "' is not valid for this type of data."
        )
      } else {
        if (is.null(bc_method)) {
          stop(
            "If using 'Intbc', 'bc_method' must be specified. Options ",
            "are: 'quantile_mapping', 'dynamical_bias', 'bias', 'evmos',",
            "'mse_min', 'crps_min', 'rpc-based'. Abbreviations 'dbc', ",
            "'qm' are also accepted."
          )
        }
        if (bc_method %in% c("quantile_mapping", "qm")) {
          out <- list()
          out$obs <- obs
          out$exp <-
            CSTools::CST_QuantileMapping(
              exp = exp,
              obs = obs,
              exp_cor = exp_cor,
              memb_dim = member_dim,
              sdate_dim = sdate_dim,
              method = "QUANT",
              na.rm = TRUE,
              ncores = ncores,
              ...
            )
        } else {
          out <- list()
          out$obs <- obs
          out$exp <-
            CSTools::CST_Calibration(
              exp = exp,
              obs = obs,
              exp_cor = exp_cor,
              memb_dim = member_dim,
              sdate_dim = sdate_dim,
              cal.method = bc_method,
              ncores = ncores,
              ...
            )
        }
      }

      out$exp <- .manage_dims(out$exp, c(exp_dims))
      return(out)
    }
  }

  .cdo_check()

  if (
    !downscale_function %in% c("Interpolation", "Intbc", "Intlr", "Analogs")
  ) {
    stop(
      "Please specify a downscaling function with 'downscale_function'.",
      " Options are: 'Interpolation', 'Intbc', 'Analogs', 'Intlr'."
    )
  }

  if (downscale_function %in% c("Interpolation", "Intbc", "Intlr")) {
    if (!is.null(points)) {
      # case: downscaling to point locations
      if (
        !inherits(points, "list") ||
          !all(c("latitude", "longitude") %in% names(points)) ||
          !is.numeric(points$latitude) ||
          !is.numeric(points$longitude) ||
          length(points$latitude) != length(points$longitude)
      ) {
        stop(paste0(
          "'points' must be a list with two elements named ",
          "'latitude' and 'longitude', both numeric vectors of the",
          " same length indicating the locations to downscale the",
          " model data."
        ))
      }
      if (is.null(method_point_interp)) {
        # case: points specified but method_point_interp not specified
        stop(paste0(
          "If using '",
          downscale_function,
          "' and 'points', ",
          "'method_point_interp' must be specified.",
          " 'method_point_interp' must be a character vector ",
          "indicating the interpolation method to interpolate model ",
          "gridded data into the point locations. Accepted methods",
          " are 'nearest', 'bilinear', '9point', 'invdist4nn'."
        ))
      }
      message(
        "Downscaling to point locations specified in 'points'. Note that",
        " 'method_remap' and 'target_grid' parameters will be ignored."
      )
    } else if (is.null(target_grid)) {
      # case: downscaling to a grid
      if (is.null(method_remap)) {
        # case: downscaling to grid but method_remap not specified
        stop(paste0(
          "If using '",
          downscale_function,
          "' and 'points' is not",
          " specified, downscaling is to a grid and ",
          "'method_remap' must be specified. 'method_remap'",
          " must be a character vector indicating the regridding ",
          "method. Accepted methods are",
          " 'con', 'bil', 'bic', 'nn', 'con2'."
        ))
      }
      if (downscale_function == "Interpolation") {
        # case: downscaling to a grid with Interpolation requires 'target_grid'
        stop(paste0(
          "If using 'Interpolation' and 'points' is not specified, ",
          "downscaling is to a grid. 'target_grid' must be specified",
          ". It can be either a path to another NetCDF file from ",
          "which to read the target ",
          "grid (a single grid must be defined in such file) or",
          " a character vector indicating the coarse grid to be",
          " passed to CDO, and it must be a grid recognised by CDO."
        ))
      } else {
        # case: 'Intbc' or 'Intlr' downscaling to a grid
        if (is.null(obs)) {
          # case: 'Intbc' or 'Intlr' downscaling to a grid requires 'obs'
          stop(paste0(
            "If using '",
            downscale_function,
            "', 'obs' must be ",
            "specified. It must be an s2dv_cube object loaded and ",
            "transformed using clim4health functions."
          ))
        }
        # case: 'Intbc' or 'Intlr' downscaling to a grid requires 'target_grid'
        message(paste0(
          "Note: If using '",
          downscale_function,
          "' and 'points'",
          " is not specified, downscaling is to a grid. ",
          "'target_grid' has not been specified. Downscaling will",
          " be done to the grid of 'obs'."
        ))
        target_grid <- obs$attrs$source_files[1]
      }
    }
  }

  if (length(method_remap) > 1) {
    # case: if more than one method_remap is provided, stop
    stop(
      "'method_remap' must be a single character string. Accepted values",
      " are 'con', 'bil', 'bic', 'nn', 'con2'."
    )
  }

  # catch missing arguments for each downscaling function
  if (downscale_function == "Intbc") {
    if (is.null(bc_method)) {
      stop(
        "If using 'Intbc', 'bc_method' must be specified. Options ",
        "are: 'quantile_mapping', 'dynamical_bias', 'bias', 'evmos',",
        "'mse_min', 'crps_min', 'rpc-based'. Abbreviations 'dbc', ",
        "'qm' are also accepted."
      )
    }
  }

  if (downscale_function == "Intlr") {
    if (is.null(lr_method)) {
      stop(
        "If using 'Intlr', 'lr_method' must be specified. Options ",
        "are: 'basic', 'large-scale' and '9nn'."
      )
    }
  }

  if (downscale_function %in% c("Intbc", "Intlr", "Analogs")) {
    if (is.null(obs)) {
      stop(paste0(
        "If using '",
        downscale_function,
        "', 'obs' must be ",
        "specified. It must be an s2dv_cube object loaded and ",
        "transformed using clim4health functions."
      ))
    }
  }

  if (downscale_function == "Analogs") {
    if (is.null(target_grid)) {
      # case: 'Analogs' downscaling to a grid requires 'target_grid'
      message(paste0(
        "Note: using method '",
        downscale_function,
        "' and ",
        "'target_grid' has not been specified. c4h_downscale ",
        "will use the grid of 'exp' as the coarse grid."
      ))

      # create temporary netCDF file with reduced dimensions if needed
      source_file_exp <- exp$attrs$source_files[1]

      tmp_file_exp <- .reduce_source_file(source_file_exp)
      exp$attrs$source_files[1] <- tmp_file_exp
      target_grid <- exp$attrs$source_files[1]
    }
  }

  if (is.null(bbox)) {
    region <- c(
      min(exp$coords$longitude),
      max(exp$coords$longitude),
      min(exp$coords$latitude),
      max(exp$coords$latitude)
    )
  } else {
    if (
      !(length(bbox) == 4 &&
          is.numeric(bbox) &&
          all(bbox[c(1, 3)] > -90) ||
          all(bbox[c(1, 3)] < 90) &&
            all(bbox[c(1, 3)] > -90) ||
          all(bbox[c(1, 3)] < 90) &&
            all(bbox[c(2, 4)] > -180) ||
          all(bbox[c(1, 3)] < 180) &&
            all(bbox[c(2, 4)] > -180) ||
          all(bbox[c(1, 3)] < 180))
    ) {
      stop(
        "'bbox' coordinates are out of bound. N,W,S,E coordinates,
             latitudes from 90 to -90, longitudes -180 to 180"
      )
    }
    # reorder bbox to work with CSDownscale functions
    # lonmin, lonmax, latmin, latmax
    region <- c(bbox[2], bbox[4], bbox[3], bbox[1])
  }

  # cases of bugs in CSDownscale
  if (!is.null(points)) {
    if (downscale_function == "Intlr" && is.null(method_remap)) {
      # assign default method_remap which is not used
      method_remap <- "bil"
    }
    # create temporary netCDF file with reduced dimensions if needed
    source_file_exp <- exp$attrs$source_files[1]

    tmp_file_exp <- .reduce_source_file(source_file_exp)
    if (grepl("esarchive", source_file_exp)) {
      # if source file is in esarchive, lon is 0-360 in source file
      tmp_file_exp2 <- gsub(".nc", "_lon360.nc", tmp_file_exp)
      system(paste0("cdo sellonlatbox,-180,180,-90,90 ",
                    tmp_file_exp, " ", tmp_file_exp2))
      system(paste0("rm ", tmp_file_exp))
      tmp_file_exp <- tmp_file_exp2
    }
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

  if (!is.null(ncores)) {
    if (ncores > (parallel::detectCores() - 1)) {
      # if ncores is greater than the number of available cores, stop
      stop("'ncores' cannot be equal to or greater than the available cores.")
    }
  }

  if (!is.null(points)) {
    exp_dims <- c(
      exp_dims[!exp_dims %in% c("longitude", "latitude")],
      "location"
    )
    if (is.na(match("location", names(dim(obs$data))))) {
      # case of gridded obs data
      obs_dims <- c(
        obs_dims[!obs_dims %in% c("longitude", "latitude")],
        "location"
      )
    }
  }
  if (downscale_function == "Interpolation") {
    out <- CSDownscale::CST_Interpolation(
      exp = exp,
      points = points,
      method_remap = method_remap,
      target_grid = target_grid,
      lat_dim = lat_dim,
      lon_dim = lon_dim,
      region = region,
      method_point_interp = method_point_interp,
      ncores = ncores
    )
    out$exp <- .manage_dims(out$exp, exp_dims)
  } else if (downscale_function == "Intbc") {
    out <- CSDownscale::CST_Intbc(
      exp = exp,
      obs = obs,
      exp_cor = exp_cor,
      target_grid = target_grid,
      bc_method = bc_method,
      int_method = method_remap,
      points = points,
      method_point_interp = method_point_interp,
      lat_dim = lat_dim,
      lon_dim = lon_dim,
      sdate_dim = sdate_dim,
      member_dim = member_dim,
      time_dim = time_dim,
      region = region,
      ncores = ncores,
      loocv = loocv,
      ...
    )
    out$exp <- .manage_dims(out$exp, exp_dims)
    out$obs <- .manage_dims(out$obs, obs_dims)
  } else if (downscale_function == "Intlr") {
    out <- CSDownscale::CST_Intlr(
      exp = exp,
      obs = obs,
      exp_cor = exp_cor,
      lr_method = lr_method,
      target_grid = target_grid,
      points = points,
      int_method = method_remap,
      method_point_interp = method_point_interp,
      lat_dim = lat_dim,
      lon_dim = lon_dim,
      sdate_dim = sdate_dim,
      time_dim = time_dim,
      member_dim = member_dim,
      region = region,
      ncores = ncores,
      loocv = loocv,
      ...
    )
    out$exp <- .manage_dims(out$exp, exp_dims)
    out$obs <- .manage_dims(out$obs, obs_dims)
  } else if (downscale_function == "Analogs") {
    out <- CSDownscale::CST_Analogs(
      exp = exp,
      obs = obs,
      grid_exp = target_grid,
      lat_dim = lat_dim,
      lon_dim = lon_dim,
      sdate_dim = sdate_dim,
      time_dim = time_dim,
      member_dim = member_dim,
      region = region,
      ncores = ncores,
      loocv = loocv,
      ...
    )
    out$exp <- .manage_dims(out$exp, c(exp_dims, "analogs"))
    out$obs <- .manage_dims(out$obs, obs_dims)
  }

  # remove temporary files if created
  if (!is.null(points)) {
    out$exp$attrs$location <- points
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
    if (!is.null(obs)) {
      out$obs$attrs$location <- points
      if (!is.null(source_file_obs)) {
        out$obs$attrs$source_files[1] <- source_file_obs
        if (tmp_file_obs != source_file_obs) {
          system(paste0("rm ", tmp_file_obs))
        }
      }
    }
  }
  if (downscale_function == "Analogs") {
    if (exists("tmp_file_exp")) {
      system(paste0("rm ", tmp_file_exp))
    }
  }
  return(out)
}

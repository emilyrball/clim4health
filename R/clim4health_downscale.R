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

library(s2dv)
library(CSTools)
library(multiApply)

#' Downscale and calibrate climate data...
#'
#' @param obs TBD
#' @param exp TBD
#' @param fcst TBD
#' @param ref_path TBD
#' @param var TBD
#' @param method TBD
#' @param ncores TBD
#'
#' @returns TBD
#' @export
#'
#' @examples
#' # TBD
clim4health_downscale <- function(downscale_function,
  # required/overlapping inputs for all CSDownscale functions
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
  lat_dim <- "latitude"
  lon_dim <- "longitude"
  sdate_dim <- "sdate"
  member_dim <- "ensemble"
  time_dim <- "time"
  # ref, hcst, (fcst) s2dv_cube objects with dimensions
  #
  if (is.null(exp)) {
    stop("parameter 'exp' must be specified")
  } else if (is.null(downscale_function)) {
    stop("Please specify a downscaling function. Options are: ",
         "'Interpolation', 'Intbc', 'Analogs', 'Intlr'")
  } else if (is.null(obs) && downscale_function != "Interpolation") {
    stop(paste0("parameter 'obs' must be specified for 'downscale_function' =",
                " '", downscale_function, "'"))
  }

  # catch missing arguments for each downscaling function
  if (downscale_function == "Intbc" && (is.null(target_grid) ||
                                          is.null(bc_method))) {
    stop(paste0("If using 'Intbc', both 'target_grid' and 'bc_method'",
                " must be specified"))
    # print valid options
  }
  if (downscale_function == "Intlr" && is.null(lr_method)) {
    stop(paste0("If using 'Intlr', 'lr_method' must be specified. Options ",
                "are: 'basic', 'large-scale', '9nn'."))
  }
  if (is.null(method_remap) && (downscale_function == "Intbc" ||
                                  downscale_function == "Intlr")) {
    stop(paste0("If using '", downscale_function, "', 'method_remap' must be ",
                "specified. Options are: 'con', 'bil', 'bic', 'nn', 'con2'."))
  }
  if (is.null(target_grid)) {
    stop(paste0("If using '", downscale_function, "', 'target_grid' must be ",
                "specified. 'target_grid' can be either a path to another ",
                "NetCDF file which to read the target grid from (a single",
                " grid must be defined in such file) or a character vector ",
                "indicating the coarse grid to be passed to CDO, and it must",
                " be a grid recognised by CDO or a NetCDF file."))
  }
  spatial_type <- .determine_spatial_type(exp$data)
  if (spatial_type == "region") {
    stop("Cannot downscale if data has been aggregated to regions")
  }

  # get var name
  if (is.null(var)) {
    var <- .get_var(obs)
  }

  if (is.null(ncores)) {
    # if ncores is not specified, use third of the available cores
    ncores <- max(floor(parallel::detectCores() / 3), 1)
  } else if (ncores > (parallel::detectCores() - 1)) {
    # if ncores is greater than the number of available cores, stop
    stop("ncores cannot be equal to or greater than the available cores")
  }

  exp_dims <- names(dim(exp$data))
  obs_dims <- names(dim(obs$data))
  if (!is.null(exp_cor)) {
    exp_dims <- dim(exp_cor$data)
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
    out$exp$data <- s2dv::Reorder(out$exp$data, order = c(exp_dims, "analogs"))
    out$obs$data <- s2dv::Reorder(out$obs$data, order = obs_dims)

    out$exp$dims <- dim(out$exp$data)
    out$obs$dims <- dim(out$obs$data)
  }

  return(out)
}
#' Downscale and calibrate climate data...
#'
#' @param ref TBD
#' @param hcst TBD
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
clim4health_downscale <- function(ref,
                                  hcst,
                                  fcst = NULL,
                                  ref_path = NULL,
                                  var = NULL,
                                  method = "Interpolation",
                                  method_point_interp = NULL,
                                  bc_method = NULL,
                                  lr_method = NULL,
                                  ncores = NULL) {
  # ref, hcst, (fcst) s2dv_cube objects with dimensions
  #
  if (is.null(ref) || is.null(hcst)) {
    stop("ref and hcst must be specified")
  }
  if (method == "Intbc" && is.null(bc_method)) {
    stop("Bias correction method 'bc_method' must be specified")
  }

  # get var name
  if (is.null(var)) {
    var <- .get_var(ref)
  }

  if (is.null(ncores)) {
    # if ncores is not specified, use third of the available cores
    ncores <- max(floor(parallel::detectCores() / 3), 1)
  } else if (ncores > (parallel::detectCores() - 1)) {
    # if ncores is greater than the number of available cores, stop
    stop("ncores cannot be equal to or greater than the available cores")
  }

  spatial_type <- .determine_spatial_type(ref$data)
  if (spatial_type == "region") {
    stop("Cannot downscale if data has been aggregated to regions")
  }

  if (method %in% c("Interpolation", "Intbc") && is.null(ref_path)) {
    stop("ref_path must be specified if regridding is needed")
  }

  # regrid (gridded data)
  if (method %in% c("Interpolation", "Intbc")) {
    out <- .clim4health_regrid(ref_path = ref_path, ref = ref, exp = hcst,
                               vars = var, ncores = ncores)
    hcst$data <- out$data_array
    hcst$coords$latitude <- out$lats
    hcst$coords$longitude <- out$lons
    if (!is.null(fcst)) {
      out <- .clim4health_interpolate(ref_path = ref_path,
                                      ref = ref,
                                      exp = fcst,
                                      vars = var,
                                      ncores = ncores)
      fcst$data <- out$data_array
      fcst$coords$latitude <- out$lats
      fcst$coords$longitude <- out$lons
    }
  }

  if (method == "Intbc") {
    out <- .clim4health_Intbc(ref_path = ref_path,
                              ref = ref,
                              exp = hcst,
                              exp_cor = fcst,
                              vars = var,
                              bc_method = bc_method,
                              ncores = ncores)
    hcstcal <- out$hcstcal
    if (!is.null(fcst)) {
      fcstcal <- out$fcstcal
    }
    hcst$data <- hcstcal$data
    fcst$data <- fcstcal$data
  }

  return(list(hcst = hcst, fcst = fcst))
}
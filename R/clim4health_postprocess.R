#' Postprocess climate data including calibration...
#'
#' @param ref TBD
#' @param hcst TBD
#' @param fcst TBD
#' @param ref_path TBD
#' @param var TBD
#' @param method TBD
#' @param skill TBD
#' @param cal_method TBD
#' @param eval_method TBD
#' @param wet_day TBD
#' @param memb_dim TBD
#' @param sdate_dim TBD
#' @param ncores TBD
#'
#' @returns TBD
#' @export
#'
#' @examples
#' # TBD
clim4health_postprocess <- function(ref, 
                                    hcst, 
                                    fcst = NULL,
                                    ref_path = NULL,
                                    var = NULL,
                                    method = "calibrate",
                                    skill = TRUE,
                                    cal_method = NULL,
                                    eval_method = NULL,
                                    wet_day = TRUE,
                                    memb_dim = "ensemble",
                                    sdate_dim = "sdate",
                                    ncores = NULL) {
  # ref, hcst, fcst s2dv_cube objects with dimensions:
  # dataset, var, time, ensemble, latitude, longitude (output of load)
  # or
  # dataset, var, time, ensemble, region (output of spatial)
  #
  # if method = "calibrate" calls clim4health_calibrate
  # if method = "qm" calls clim4health_qm
  # if skill = TRUE, additionally calls clim4health_skill
  if (is.null(ref) || is.null(hcst)) {
    stop("ref and hcst must be specified")
  }
  
  # get var name
  if (is.null(var)) {
    var <- .get_var(ref)
  }
  
  if (is.null(ncores)) {
    # if ncores is not specified, use half of the available cores
    ncores <- max(floor(parallel::detectCores() / 3), 1)
  } else if (ncores > (parallel::detectCores() - 1)) {
    # if ncores is greater than the number of available cores, stop
    stop("ncores cannot be equal to or greater than the available cores")
  }
  
  spatial_type <- .determine_spatial_type(ref$data)
  
  # check length of dimensions
  if (spatial_type == "grid") {
    # regrid if needed
    if (dim(ref$data)[["latitude"]] != dim(hcst$data)[["latitude"]] ||
        dim(ref$data)[["longitude"]] != dim(hcst$data)[["longitude"]]) {
      if (is.null(ref_path)) {
        stop("ref_path must be specified if regridding is needed")
      }
      
      out <- .clim4health_regrid(ref_path = ref_path, ref = ref, exp = hcst,
                                 vars = var, ncores = ncores)
      hcst$data <- out$data_array
      hcst$coords$latitude <- out$lats
      hcst$coords$longitude <- out$lons
      if (!is.null(fcst)) {
        out <- .clim4health_regrid(ref_path = ref_path, ref = ref, exp = fcst,
                                   vars = var, ncores = ncores)
        fcst$data <- out$data_array
        fcst$coords$latitude <- out$lats
        fcst$coords$longitude <- out$lons
      }
    }
  } else {
    if (dim(ref)[["region"]] != dim(hcst)[["region"]]) {
      stop("ref and hcst must have the same number of regions")
    }
  }
  
  ref <- .get_array(ref)
  hcst <- .get_array(hcst)
  if (!is.null(fcst)) {
    fcst <- .get_array(fcst)
  }
  
  if (method == "calibrate") {
    # if parameters not specified, use default values
    if (is.null(cal_method)) {
      cal_method <- "evmos"
    }
    if (is.null(eval_method)) {
      eval_method <- "leave-one-out"
    }
    # calibration
    data <- .clim4health_calibrate(ref = ref, hcst = hcst,
                                   fcst = fcst,
                                   cal_method = cal_method,
                                   eval_method = eval_method,
                                   memb_dim = memb_dim,
                                   sdate_dim = sdate_dim,
                                   ncores = ncores)
  } else if (method == "qm") {
    # if parameters not specified, use default values
    if (is.null(cal_method)) {
      cal_method <- "QUANT"
    }
    # quantile mapping
    data <- .clim4health_qm(ref = ref, hcst = hcst,
                            fcst = fcst,
                            cal_method = cal_method,
                            memb_dim = memb_dim,
                            sdate_dim = sdate_dim,
                            ncores = ncores)
  } else {
    stop("method must be either 'calibrate' or 'qm'")
  }
  
  # skill
  if (skill == TRUE) {
    # if thresholds not specified, use default value
    data$bss10 <- .clim4health_skill(ref = ref, hcst = data$hcstcal,
                                     threshold = c(0.1),
                                     sdate_dim = sdate_dim,
                                     memb_dim = memb_dim)
    data$bss90 <- .clim4health_skill(ref = ref, hcst = data$hcstcal,
                                     threshold = c(0.9),
                                     sdate_dim = sdate_dim,
                                     memb_dim = memb_dim)
    data$crpss <- .clim4health_skill(ref = ref, hcst = data$hcstcal,
                                     sdate_dim = sdate_dim,
                                     memb_dim = memb_dim)
  }
  return(data)
}
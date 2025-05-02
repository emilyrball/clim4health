#' Get var
#'
#' @param data TBD
#'
#' @returns TBD
#'
#' @examples
#' # TBD

.get_var <- function(data) {
  if (class(data) == "s2dv_cube") {
    return(data$coords$var)
  }
}

#' Title
#'
#' @param data TBD 
#'
#' @returns TBD
#'
#' @examples
#' # TBD

.get_array <- function(data) {
  if (class(data) == "s2dv_cube") {
    return(data$data)
  } else if (class(data) == "array") {
    return(data)
  } else {
    stop("data must be an s2dv_cube or an array")
  }
}

#' Determine spatial type
#' 
#' @param data TBD 
#'
#' @returns TBD
#'
#' @examples
#' # TBD

.determine_spatial_type <- function(data) {
  if (any(names(dim(data)) == "latitude") &&
      any(names(dim(data)) == "longitude")) {
    output <- "grid"
  } else if (any(names(dim(data)) == "region")) {
    output <- "region"
  } else {
    stop("ref must have either latitude and longitude or region dimensions")
  }
  return(output)
}

#' Calibrate
#'
#' @param ref TBD 
#' @param hcst TBD 
#' @param fcst  TBD
#' @param cal_method  TBD
#' @param eval_method  TBD
#' @param memb_dim  TBD
#' @param sdate_dim  TBD
#' @param ncores  TBD
#'
#' @returns TBD
#' 
#' @examples
#' # TBD

.clim4health_calibrate <- function(ref, hcst, fcst = NULL,
                                   cal_method = "evmos",
                                   eval_method = "leave-one-out",
                                   memb_dim = "ensemble",
                                   sdate_dim = "sdate",
                                   ncores = 1) {
  # calls CST_Calibrate
  hcstcal <- CSTools::Calibration(exp = hcst, obs = ref,
                                  cal.method = cal_method,
                                  eval.method = eval_method,
                                  memb_dim = memb_dim,
                                  sdate_dim = sdate_dim,
                                  ncores = ncores)
  if (!is.null(fcst)) {
    fcstcal <- CSTools::Calibration(exp = hcst, exp_cor = fcst, obs = ref,
                                    cal.method = cal_method,
                                    eval.method = eval_method,
                                    memb_dim = memb_dim,
                                    sdate_dim = sdate_dim,
                                    ncores = ncores)
  } else {
    fcstcal <- NULL
  }
  return(list(hcstcal = hcstcal, fcstcal = fcstcal))
}

#' Quantile mapping
#'
#' @param ref TBD 
#' @param hcst TBD 
#' @param fcst TBD 
#' @param cal_method TBD 
#' @param wet_day TBD 
#' @param memb_dim TBD 
#' @param sdate_dim TBD 
#' @param ncores TBD 
#'
#' @returns TBD
#'
#' @examples
#' # TBD

.clim4health_qm <- function(ref, hcst, fcst = NULL,
                            cal_method = "QUANT",
                            wet_day = TRUE,
                            memb_dim = "ensemble",
                            sdate_dim = "sdate",
                            ncores = 1) {
  hcstcal <- CSTools::QuantileMapping(exp = hcst, obs = ref,
                                      memb_dim = memb_dim,
                                      sdate_dim = sdate_dim,
                                      method = cal_method,
                                      wet.day = wet_day,
                                      na.rm = TRUE,
                                      ncores = ncores)
  # because QuantileMapping returns the array in a different order
  hcstcal <- aperm(hcstcal, match(names(dim(hcst)), names(dim(hcstcal))))
  
  if (!is.null(fcst)) {
    fcstcal <- CSTools::QuantileMapping(exp = hcst, exp_cor = fcst, obs = ref,
                                        memb_dim = memb_dim,
                                        sdate_dim = sdate_dim,
                                        method = cal_method,
                                        wet.day = wet_day,
                                        na.rm = TRUE,
                                        ncores = ncores)
    # because QuantileMapping returns the array in a different order
    fcstcal <- aperm(fcstcal, match(names(dim(fcst)), names(dim(fcstcal))))
  } else {
    fcstcal <- NULL
  }
  return(list(hcstcal = hcstcal, fcstcal = fcstcal))
}

#' Skill
#'
#' @param hcst TBD 
#' @param ref TBD 
#' @param threshold  TBD
#' @param sdate_dim TBD 
#' @param memb_dim  TBD
#' @param ncores TBD 
#'
#' @returns TBD
#'
#' @examples
#' # TBD

.clim4health_skill <- function(hcst, ref, threshold = NULL,
                               sdate_dim = "sdate", memb_dim = "ensemble",
                               ncores = 1) {
  # if threshold not specified, return CRPSS
  if (is.null(threshold)) {
    skill <- s2dv::CRPSS(exp = hcst, obs = ref,
                         clim.cross.val = TRUE,
                         time_dim = sdate_dim,
                         memb_dim = memb_dim,
                         ncores = ncores)
  } else {
    # calls s2dv::RPSS
    skill <- s2dv::RPSS(exp = hcst, obs = ref,
                        prob_thresholds = threshold,
                        cross.val = TRUE,
                        time_dim = sdate_dim,
                        memb_dim = memb_dim,
                        ncores = ncores)
  }
  return(skill)
}

#' Regrid
#'
#' @param ref_path TBD 
#' @param ref TBD 
#' @param exp TBD 
#' @param vars TBD 
#' @param ncores TBD 
#'
#' @returns TBD
#'
#' @examples
#' # TBD

.clim4health_regrid <- function(ref_path, ref, exp, vars, ncores) {
  # regrid based on s2dv::CDORemap()
  # for temperature related 'bilinear', for precipitation 'con2'
  lons <- exp$coords$longitude
  lats <- exp$coords$latitude
  
  crop <- c(min(ref$coords$longitude), max(ref$coords$longitude),
            min(ref$coords$latitude), max(ref$coords$latitude))
  
  for (var in vars) {
    if (var %in% c("tas", "t2m", "tmax", "tmin", "tasmax", "tasmin",
                   "temperature", "temp")) {
      method <- "bilinear"
    } else if (var %in% c("pr", "precip", "rain", "prec", "rr", "tp",
                          "precipitation", "prlr", "rainfall")) {
      method <- "con2"
    } else {
      method <- "bilinear"
      warning("var not recognized, using bilinear method")
    }
    
    output <- s2dv::CDORemap(data_array = exp$data,
                             lons = lons, lats = lats, grid = ref_path, method = method,
                             avoid_writes = TRUE, crop = crop, force_remap = FALSE,
                             print_sys_msg = FALSE, ncores = ncores
    )
  }
  return(output)
}
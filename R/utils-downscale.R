#' Interpolation
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

.clim4health_interpolate <- function(ref_path,
                                     ref,
                                     exp,
                                     vars,
                                     ncores) {
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

    output <- s2dv::CDORemap(
      data_array = exp$data,
      lons = lons,
      lats = lats,
      grid = ref_path,
      method = method,
      avoid_writes = TRUE,
      crop = crop,
      force_remap = FALSE,
      print_sys_msg = TRUE,
      ncores = ncores
    )
  }
  return(output)
}

#' IntBC
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

.clim4health_Intbc <- function(ref_path,
                               ref,
                               exp,
                               exp_cor = NULL,
                               var = NULL,
                               bc_method = NULL,
                               ncores = 1) {
  # calibrate
  # currently supports bc_method = "bias" or "qm"
  if (is.null(bc_method)) {
    bc_method <- "bias"
    warning(paste("bc_method not specified, using method 'bias'. Please",
                  "ensure this method is appropriate for your data."))
  }

  ref <- .get_array(ref)
  hcst <- .get_array(hcst)
  if (!is.null(fcst)) {
    fcst <- .get_array(fcst)
  }

  if (bc_method == "bias") {
    # call CSTools::Calibration
    if (is.null(cal_method)) {
      cal_method <- "evmos"
    }
    if (is.null(eval_method)) {
      eval_method <- "leave-one-out"
    }
    # calibration
    hcstcal <- CSTools::Calibration(exp = hcst, obs = ref,
                                    cal.method = cal_method,
                                    eval.method = eval_method,
                                    memb_dim = "member",
                                    sdate_dim = "sdate",
                                    ncores = ncores)
    if (!is.null(fcst)) {
      fcstcal <- CSTools::Calibration(exp = hcst, exp_cor = fcst, obs = ref,
                                      cal.method = cal_method,
                                      eval.method = eval_method,
                                      memb_dim = "member",
                                      sdate_dim = "sdate",
                                      ncores = ncores)
    } else {
      fcstcal <- NULL
    }
  }
  if (bc_method == "dbc" || bc_method == "dynamical_bias") {
    # call CSTools::DynBiasCorrection
  }
  if (bc_method == "qm" || bc_method == "quantile_mapping") {
    # call CSTools::QuantileMapping
    if (is.null(cal_method)) {
      cal_method <- "QUANT"
    }
    hcstcal <- CSTools::QuantileMapping(exp = hcst, obs = ref,
                                        memb_dim = "member",
                                        sdate_dim = "sdate",
                                        method = cal_method,
                                        wet.day = wet_day,
                                        na.rm = TRUE,
                                        ncores = ncores)
    # because QuantileMapping returns the array in a different order
    hcstcal <- aperm(hcstcal, match(names(dim(hcst)), names(dim(hcstcal))))

    if (!is.null(fcst)) {
      fcstcal <- CSTools::QuantileMapping(exp = hcst, exp_cor = fcst, obs = ref,
                                          memb_dim = "member",
                                          sdate_dim = "sdate",
                                          method = cal_method,
                                          wet.day = wet_day,
                                          na.rm = TRUE,
                                          ncores = ncores)
      # because QuantileMapping returns the array in a different order
      fcstcal <- aperm(fcstcal, match(names(dim(fcst)), names(dim(fcstcal))))
    } else {
      fcstcal <- NULL
    }
  }
  return(list(hcstcal = hcstcal, fcstcal = fcstcal))
}
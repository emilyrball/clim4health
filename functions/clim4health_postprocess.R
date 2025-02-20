# function to postprocess climate data, maintaining the same object dimensions
# and adding information to the metadata

# dependencies
library(CSTools)
library(s2dv)
library(parallel)

clim4health_calibrate <- function(ref, hcst, fcst = NULL,
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

clim4health_qm <- function(ref, hcst, fcst = NULL,
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

clim4health_skill <- function(hcst, ref, threshold = NULL,
                              sdate_dim = "sdate", memb_dim = "ensemble",
                              ncores = 1) {
  # if threshold not specified, return CRPSS
  if (is.null(threshold)) {
    skill <- s2dv::CRPSS(exp = hcst, obs = ref,
                         clim.cross.val = TRUE,
                         time_dim = "sdate",
                         memb_dim = memb_dim,
                         ncores = ncores)
  } else {
    # calls s2dv::RPSS
    skill <- s2dv::RPSS(exp = hcst, obs = ref,
                        prob_thresholds = threshold,
                        cross.val = TRUE,
                        time_dim = "sdate",
                        memb_dim = memb_dim,
                        ncores = ncores)
  }
  return(skill)
}

rename_region <- function() {
  # if using output of clim4health_spatial
}

clim4health_postprocess <- function(ref, hcst, fcst = NULL,
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
  # if method = "calibrate" calls clim4health_calibrate
  # if method = "qm" calls clim4health_qm
  # if skill = TRUE, additionally calls clim4health_skill
  if (is.null(ref) || is.null(hcst)) {
    stop("ref and hcst must be specified")
  }

  if (is.null(ncores)) {
    # if ncores is not specified, use half of the available cores
    ncores <- floor(detectCores() / 2)
  } else if (ncores > detectCores()) {
    # if ncores is greater than the number of available cores, stop
    stop("ncores cannot be greater than the number of available cores")
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
    data <- clim4health_calibrate(ref = ref, hcst = hcst,
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
    data <- clim4health_qm(ref = ref, hcst = hcst,
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
    data$bss10 <- clim4health_skill(ref = ref, hcst = data$hcstcal,
                                    thresholds = c(0.1),
                                    sdate_dim = sdate_dim,
                                    memb_dim = memb_dim)
    data$bss90 <- clim4health_skill(ref = ref, hcst = data$hcstcal,
                                    thresholds = c(0.9),
                                    sdate_dim = sdate_dim,
                                    memb_dim = memb_dim)
    data$crpss <- clim4health_skill(ref = ref, hcst = data$hcstcal,
                                    sdate_dim = sdate_dim,
                                    memb_dim = memb_dim)
  }

  return(data)
}
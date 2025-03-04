# function to postprocess climate data, maintaining the same object dimensions
# and adding information to the metadata

# dependencies
library(CSTools)
library(s2dv)
library(parallel)

get_var <- function(data) {
  if (class(data) == "s2dv_cube") {
    return(data$coords$var)
  }
}

get_array <- function(data) {
  if (class(data) == "s2dv_cube") {
    return(data$data)
  } else if (class(data) == "array") {
    return(data)
  } else {
    stop("data must be an s2dv_cube or an array")
  }
}

determine_spatial_type <- function(data) {
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

clim4health_regrid <- function(ref_path, ref, exp, vars, ncores) {
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

clim4health_postprocess <- function(ref, hcst, fcst = NULL,
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
    var <- get_var(ref)
  }

  if (is.null(ncores)) {
    # if ncores is not specified, use half of the available cores
    ncores <- max(floor(parallel::detectCores() / 3), 1)
  } else if (ncores > (parallel::detectCores() - 1)) {
    # if ncores is greater than the number of available cores, stop
    stop("ncores cannot be equal to or greater than the available cores")
  }

  spatial_type <- determine_spatial_type(ref$data)

  # check length of dimensions
  if (spatial_type == "grid") {
    # regrid if needed
    if (dim(ref$data)[["latitude"]] != dim(hcst$data)[["latitude"]] ||
          dim(ref$data)[["longitude"]] != dim(hcst$data)[["longitude"]]) {
      if (is.null(ref_path)) {
        stop("ref_path must be specified if regridding is needed")
      }

      out <- clim4health_regrid(ref_path = ref_path, ref = ref, exp = hcst,
                                vars = var, ncores = ncores)
      hcst$data <- out$data_array
      hcst$coords$latitude <- out$lats
      hcst$coords$longitude <- out$lons
      if (!is.null(fcst)) {
        out <- clim4health_regrid(ref_path = ref_path, ref = ref, exp = fcst,
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

  ref <- get_array(ref)
  hcst <- get_array(hcst)
  if (!is.null(fcst)) {
    fcst <- get_array(fcst)
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
                                    threshold = c(0.1),
                                    sdate_dim = sdate_dim,
                                    memb_dim = memb_dim)
    data$bss90 <- clim4health_skill(ref = ref, hcst = data$hcstcal,
                                    threshold = c(0.9),
                                    sdate_dim = sdate_dim,
                                    memb_dim = memb_dim)
    data$crpss <- clim4health_skill(ref = ref, hcst = data$hcstcal,
                                    sdate_dim = sdate_dim,
                                    memb_dim = memb_dim)
  }
  return(data)
}
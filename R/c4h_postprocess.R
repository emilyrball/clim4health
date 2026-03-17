#' c4h_postprocess
#'
#' Quality assessment of climate forecasts
#'
#' @param exp An s2dv_cube object of forecast data with at least the 'sdate'
#' and 'member' dimensions.
#' @param obs An s2dv_cube object of observation data with at least the 'sdate'
#' and 'member' dimensions.
#' @param metrics A character vector of the verification metrics to be
#' calculated. Valid options are: "BSS", "RPSS", "CRPSS", "AbsBiasSS", "MSE",
#' "MSSS", "RMSE", "RMSSS", "ROCSS".
#' @param ref An optional s2dv_cube object of reference data with at least the
#' 'sdate' and 'ensemble' dimensions. The dimensions must be the same as 'exp'
#' except 'ensemble'. If 'ref' is NULL, the climatological
#' forecast is used as reference forecast. The default value is NULL.
#' @param brier_thresholds Optional numeric vector of thresholds for the
#' Brier Skill Score (BSS) calculation. The default is c(0.1, 0.9). Only
#' applicable if "BSS" is in 'metrics'.
#' @param prob_thresholds Optional numeric vector of thresholds for the
#' Ranked Probability Skill Score (RPSS) calculation. The default is c(1/3,
#' 2/3). Only applicable if "RPSS" is in 'metrics'.
#' @param alpha A numeric of the significance level to be used in the
#' statistical significance test. The default value is 0.05. Relevant for all
#' metrics except "ROCSS".
#' @param ncores An integer indicating the number of cores to use for parallel
#' computation. The default value is NULL.
#' @param N.eff Effective sample size to be used in the statistical significance
#' test. It can be NA (and it will be computed with the s2dv:::.Eno), FALSE (and
#' it will use the length of "obs" along "time", so the autocorrelation is
#' not taken into account), a numeric (which is used for all cases), or an array
#' with the same dimensions as "obs" except "time" (for a particular N.eff
#' to be used for each case). The default value is NA. Only relevant if "BSS",
#' "RPSS", "CRPSS", or "RMSSS" is in 'metrics'.
#' @param Fair A logical indicating whether to compute the FairRPSS or FairCRPSS
#' (the potential RPSS or CRPSS that the forecast would have with an infinite
#' ensemble size). The default value is FALSE. Only relevant if "BSS",
#' "RPSS", or "CRPSS" is in 'metrics'.
#' @param indices_for_clim A vector of the indices to be taken along 'time'
#' for computing the thresholds between the probabilistic categories. If NULL,
#' the whole period is used. The default value is NULL. Only relevant if "BSS",
#' "RPSS", or "ROCSS" is in 'metrics'.
#' @param cross.val A logical indicating whether to compute the thresholds
#' between probabilistics categories in cross-validation. The default value is
#' FALSE. Only relevant if "BSS", "RPSS", or "ROCSS" is in 'metrics'.
#' @param cat_dim A character string indicating the name of the category
#' dimension that is needed when exp, obs, and ref are probabilities. The
#' default value is NULL, which means that the data are not probabilities.
#' Relevant if "BSS", "RPSS", or "ROCSS" is in 'metrics'.
#' @param clim.cross.val A logical indicating whether to build the
#' climatological forecast in cross-validation (i.e. excluding the observed
#' value of the time step when building the probabilistic distribution function
#' for that particular time step). The default value is TRUE. Only used if 'ref'
#' is NULL and "CRPSS" is in 'metrics'.
#' @param nmemb A numeric value indicating the number of members used to compute
#' the probabilities. This parameter is necessary when calculating FairRPS from
#' probabilities. Default is NULL. Only relevant if "BSS" or "RPSS" is in
#' 'metrics' and 'Fair' is TRUE.
#' @param nmemb_ref A numeric value indicating the number of members of the
#' reference forecast 'ref'. If 'ref' is NULL and uses the climatology,
#' 'nmemb_ref' should be the number of years used to compute the climatology.
#' Default is NULL. Only relevant if "BSS" or "RPSS" is in 'metrics'.
#' @param weights_exp  A named numerical array of the forecast ensemble weights
#' for probability calculation. The dimensions should include 'ensemble', 
#' 'time', and 'dataset' if there are multiple datasets. All dimension
#' lengths must be equal to 'exp' dimension lengths. The default value is NULL,
#' which means no weighting is applied. The ensemble should have at least 70
#' members or span at least 10 time steps and have more than 45 members if
#' consistency between the weighted and unweighted methodologies is desired.
#' Only relevant if "BSS" or "RPSS" is in 'metrics'.
#' @param weights_ref Same as 'weights_exp' but for the reference forecast.
#' @param comp_dim A character string indicating the name of dimension along
#' which obs is taken into account only if it is complete. The default value
#' is NULL. Only relevant if "MSE" or "RMSE" is in 'metrics'.
#' @param limits A vector of two integers indicating the range along comp_dim
#' to be completed. The default value is c(1, length(comp_dim dimension)). Only
#' relevant if "MSE" or "RMSE" is in 'metrics'.
#' @param conf A logical value indicating whether to retrieve the confidence
#' intervals or not. The default value is TRUE. Only relevant if "MSE" or "RMSE"
#' is in 'metrics'.
#' @param na.rm A logical. FALSE means that NA values will not be removed, and
#' the function will return 'NA' if they are present. TRUE means that NA values
#' will be removed before computation. The default value is FALSE. Only
#' relevant if "BSS", "RPSS", or "AbsBiasSS" is in 'metrics'.
#' @param sig_method.type A character string indicating the test type of the
#' significance method. For "MSSS" and "RMSSS", the default applied is NULL.
#' The default applied for "BSS", "RPSS", and "AbsBiasSS" is 'two.sided.approx'.
#' @param sign A logical value indicating whether to compute or not the
#' statistical significance of the test Ho: (R)MSSS = 0. The default value is
#' TRUE. Only relevant if "MSSS" or "RMSSS" is in 'metrics'.
#' @param pval A logical value indicating whether to compute or not the p-value
#' of the test Ho: (R)MSSS = 0. The default value is FALSE. Only relevant if
#' "MSSS" or "RMSSS" is in 'metrics'.
#' @param sig_method A character string indicating the significance method. The
#' options are "one-sided Fisher" (default) and "Random Walk". Only relevant if
#' "MSSS" or "RMSSS" is in 'metrics'.
#'
#' @returns a list of lists with the calculated metrics and their significances
#' as s2dv_cube objects.
#' @export
#'
#' @examples
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
#'
#' ## Example with polygon data
#' shp_path <- system.file("extdata", "areas", "munip_vallecauca.gpkg",
#'                         package = "clim4health")
#' obs_areas <- c4h_space(obs,
#'                        areas = shp_path,
#'                        fun = "mean",
#'                        areas_id = "munip_code")
#' hcst_areas <- c4h_space(hcst,
#'                         areas = shp_path,
#'                         fun = "mean",
#'                         areas_id = "munip_code")
#' skill <- c4h_postprocess(exp = hcst_areas,
#'                          obs = obs_areas,
#'                          metrics = c("BSS", "CRPSS", "RMSSS", "RMSE"),
#'                          brier_thresholds = c(0.1, 0.9))
c4h_postprocess <- function(exp, obs, metrics, ref = NULL,
                            brier_thresholds = c(0.1, 0.9),
                            prob_thresholds = c(1 / 3, 2 / 3),
                            alpha = 0.05, ncores = NULL, N.eff = NA,
                            Fair = FALSE, indices_for_clim = NULL,
                            cross.val = FALSE, cat_dim = NULL,
                            clim.cross.val = TRUE, nmemb = NULL,
                            nmemb_ref = NULL, weights_exp = NULL,
                            weights_ref = NULL, comp_dim = NULL,
                            limits = NULL, conf = TRUE,
                            na.rm = FALSE, sig_method.type = NULL,
                            sign = TRUE, pval = FALSE,
                            sig_method = "one-sided Fisher") {
  if (is.null(exp) || is.null(obs)) {
    stop("Parameters 'exp' and 'obs' must be specified. They should be ",
         "s2dv_cube objects that have been loaded and transformed using",
         "clim4health functions.")
  }

  if (is.null(metrics) || !(all(metrics %in% c("BSS", "RPSS", "CRPSS",
                                               "AbsBiasSS", "MSE",
                                               "MSSS", "RMSE", "RMSSS",
                                               "ROCSS")))) {
    stop("Parameter 'metrics' must be specified. This should be a character ",
         "vector of the verification metrics to be calculated. Valid options ",
         "are: \"BSS\", \"RPSS\", \"CRPSS\", \"AbsBiasSS\"",
         ", \"MSE\", \"MSSS\", \"RMSE\", \"RMSSS\", \"ROCSS\".")
  }

  time_dim <- "sdate"
  memb_dim <- "ensemble"

  if (!is.null(ncores)) {
    if (ncores > (parallel::detectCores() - 1)) {
      # if ncores is greater than the number of available cores, stop
      stop("'ncores' cannot be equal to or greater than the available cores.")
    }
  }
  obs_dims <- obs$dims
  out_dims <- exp$dims

  if (length(intersect(names(out_dims), names(obs_dims))) <
        max(length(out_dims), length(obs_dims))) {
    stop(paste0("Please ensure that 'exp' and 'obs' have the same ",
                "named dimensions. 'exp' has dimensions: '",
                paste(names(out_dims), collapse = "', '"),
                "'. 'obs' has dimensions: '",
                paste(names(obs_dims), collapse = "', '"), "'."))
  }
  for (dim in names(out_dims)) {
    if (dim != "dataset" && dim != memb_dim) {
      if (out_dims[[dim]] != obs_dims[[dim]]) {
        stop(paste0("Dimension '", dim, "' has different lengths in ",
                    "'exp' and 'obs'. 'exp' has dimension ", dim, " = ",
                    out_dims[[dim]], " while 'obs' has dimension ", dim, " = ",
                    obs_dims[[dim]], ". Please ensure that 'exp' and 'obs' ",
                    "have the same lengths for all named dimensions except ",
                    "'dataset' and 'ensemble'."))
      }
    }
  }

  out_dims[["sdate"]] <- 1
  out_dims[["ensemble"]] <- 1

  out_coords <- exp$coords
  out_coords[["sdate"]] <- out_coords[["sdate"]][1]
  out_coords[["ensemble"]] <- out_coords[["ensemble"]][1]

  out_attrs <- exp$attrs
  out_attrs$Dates <- out_attrs$Dates[1, , drop = FALSE]

  obs <- .get_array(obs)
  exp <- .get_array(exp)
  if (!is.null(ref)) {
    ref <- .get_array(ref)
  }

  data <- list()

  if (is.null(sig_method.type)) {
    sig_method_type_brier <- "two.sided.approx"
  } else {
    sig_method_type_brier <- sig_method.type
  }

  # skill
  if ("BSS" %in% metrics) {
    for (threshold in brier_thresholds) {
      temp <-
        s2dv::RPSS(exp, obs, ref = ref,
                   time_dim = time_dim, memb_dim = memb_dim, cat_dim = cat_dim,
                   dat_dim = NULL, prob_thresholds = threshold,
                   indices_for_clim = indices_for_clim, Fair = Fair,
                   #nmemb = nmemb, nmemb_ref = nmemb_ref,
                   weights_exp = weights_exp, weights_ref = weights_ref,
                   cross.val = cross.val, na.rm = na.rm,
                   sig_method.type = sig_method_type_brier, alpha = alpha,
                   N.eff = N.eff, ncores = ncores)
      rpss <- .format_output(data = temp$rpss, out_dims = out_dims,
                             out_coords = out_coords, out_attrs = out_attrs)
      signif <- .format_output(data = temp$sign, out_dims = out_dims,
                               out_coords = out_coords, out_attrs = out_attrs)
      rpss$attrs$Variable$varName <- paste0("BSS", 100 * threshold)
      signif$attrs$Variable$varName <- paste0("BSS", 100 * threshold)
      temp <- list(rpss = rpss, sign = signif)
      data[[paste0("BSS", 100 * threshold)]] <- temp
    }
  }
  if ("RPSS" %in% metrics) {
    temp <-
      s2dv::RPSS(exp, obs, ref = ref, time_dim = time_dim,
                 memb_dim = memb_dim, cat_dim = cat_dim, dat_dim = NULL,
                 prob_thresholds = prob_thresholds,
                 indices_for_clim = indices_for_clim, Fair = Fair,
                 #nmemb = nmemb, nmemb_ref = nmemb_ref,
                 weights_exp = weights_exp, weights_ref = weights_ref,
                 cross.val = cross.val, na.rm = na.rm,
                 sig_method.type = sig_method_type_brier,
                 alpha = alpha, N.eff = N.eff, ncores = ncores)
    rpss <- .format_output(data = temp$rpss, out_dims = out_dims,
                           out_coords = out_coords, out_attrs = out_attrs)
    signif <- .format_output(data = temp$sign, out_dims = out_dims,
                             out_coords = out_coords, out_attrs = out_attrs)
    rpss$attrs$Variable$varName <- "RPSS"
    signif$attrs$Variable$varName <- "RPSS"
    temp <- list(rpss = rpss, sign = signif)
    data[[paste0("RPSS")]] <- temp
  }
  if ("CRPSS" %in% metrics) {
    temp <- s2dv::CRPSS(exp, obs, ref = ref, time_dim = time_dim,
                        memb_dim = memb_dim, dat_dim = NULL,
                        Fair = Fair, clim.cross.val = clim.cross.val,
                        sig_method.type = sig_method_type_brier,
                        alpha = alpha, N.eff = N.eff,
                        ncores = ncores)
    crpss <- .format_output(data = temp$crpss, out_dims = out_dims,
                            out_coords = out_coords, out_attrs = out_attrs)
    signif <- .format_output(data = temp$sign, out_dims = out_dims,
                             out_coords = out_coords, out_attrs = out_attrs)
    crpss$attrs$Variable$varName <- "CRPSS"
    signif$attrs$Variable$varName <- "CRPSS"
    temp <- list(crpss = crpss, sign = signif)
    data[[paste0("CRPSS")]] <- temp
  }
  if ("AbsBiasSS" %in% metrics) {
    temp <- s2dv::AbsBiasSS(exp, obs, ref = ref,
                            time_dim = time_dim,
                            memb_dim = memb_dim,
                            dat_dim = NULL, na.rm = na.rm,
                            sig_method.type =
                              sig_method_type_brier,
                            alpha = alpha, ncores = ncores)
    biasss <- .format_output(data = temp$biasSS, out_dims = out_dims,
                             out_coords = out_coords, out_attrs = out_attrs)
    signif <- .format_output(data = temp$sign, out_dims = out_dims,
                             out_coords = out_coords, out_attrs = out_attrs)
    biasss$attrs$Variable$varName <- "AbsBiasSS"
    signif$attrs$Variable$varName <- "AbsBiasSS"
    temp <- list(biasSS = biasss, sign = signif)
    data[["AbsBiasSS"]] <- temp
  }
  if ("MSE" %in% metrics) {
    temp <- s2dv::MSE(exp, obs, time_dim = time_dim,
                      dat_dim = NULL, memb_dim = memb_dim,
                      comp_dim = comp_dim, limits = limits,
                      conf = conf, alpha = alpha, ncores = ncores)
    mse <- .format_output(data = temp$mse, out_dims = out_dims,
                          out_coords = out_coords, out_attrs = out_attrs)
    conf_lower <- .format_output(data = temp$conf.lower, out_dims = out_dims,
                                 out_coords = out_coords, out_attrs = out_attrs)
    conf_upper <- .format_output(data = temp$conf.upper, out_dims = out_dims,
                                 out_coords = out_coords, out_attrs = out_attrs)
    mse$attrs$Variable$varName <- "MSE"
    conf_lower$attrs$Variable$varName <- "MSE"
    conf_upper$attrs$Variable$varName <- "MSE"
    temp <- list(mse = mse, conf.lower = conf_lower,
                 conf.upper = conf_upper)
    data[["MSE"]] <- temp
  }
  if ("MSSS" %in% metrics) {
    temp <- s2dv::MSSS(exp, obs, ref = ref,
                       time_dim = time_dim, dat_dim = NULL,
                       memb_dim = memb_dim, pval = pval, sign = sign,
                       alpha = alpha, N.eff = N.eff,
                       sig_method = sig_method,
                       sig_method.type = sig_method.type,
                       ncores = ncores)
    msss <- .format_output(data = temp$msss, out_dims = out_dims,
                           out_coords = out_coords, out_attrs = out_attrs)
    if (pval == TRUE) {
      pval <- .format_output(data = temp$p.val, out_dims = out_dims,
                             out_coords = out_coords, out_attrs = out_attrs)
      pval$attrs$Variable$varName <- "MSSS"
    } else {
      pval <- NULL
    }
    if (sign == TRUE) {
      signif <- .format_output(data = temp$sign, out_dims = out_dims,
                               out_coords = out_coords, out_attrs = out_attrs)
      signif$attrs$Variable$varName <- "MSSS"
    } else {
      signif <- NULL
    }
    msss$attrs$Variable$varName <- "MSSS"
    temp <- list(msss = msss, p.val = pval, sign = signif)
    data[["MSSS"]] <- temp
  }
  if ("RMSE" %in% metrics) {
    temp <- s2dv::RMS(exp, obs, time_dim = time_dim,
                      dat_dim = NULL, memb_dim = memb_dim,
                      comp_dim = comp_dim, limits = limits,
                      conf = conf, alpha = alpha, ncores = ncores)
    rmse <- .format_output(data = temp$rms, out_dims = out_dims,
                           out_coords = out_coords, out_attrs = out_attrs)
    conf_lower <- .format_output(data = temp$conf.lower, out_dims = out_dims,
                                 out_coords = out_coords, out_attrs = out_attrs)
    conf_upper <- .format_output(data = temp$conf.upper, out_dims = out_dims,
                                 out_coords = out_coords, out_attrs = out_attrs)
    rmse$attrs$Variable$varName <- "RMSE"
    conf_lower$attrs$Variable$varName <- "RMSE"
    conf_upper$attrs$Variable$varName <- "RMSE"
    temp <- list(rmse = rmse, conf.lower = conf_lower,
                 conf.upper = conf_upper)
    data[["RMSE"]] <- temp
  }
  if ("RMSSS" %in% metrics) {
    temp <- s2dv::RMSSS(exp, obs, ref = ref,
                        time_dim = time_dim, dat_dim = NULL,
                        memb_dim = memb_dim, pval = pval,
                        sign = sign, alpha = alpha, N.eff = N.eff,
                        sig_method = sig_method,
                        sig_method.type = sig_method.type,
                        ncores = ncores)
    rmsss <- .format_output(data = temp$rmsss, out_dims = out_dims,
                            out_coords = out_coords, out_attrs = out_attrs)
    if (pval == TRUE) {
      pval <- .format_output(data = temp$p.val, out_dims = out_dims,
                             out_coords = out_coords, out_attrs = out_attrs)
      pval$attrs$Variable$varName <- "RMSSS"
    } else {
      pval <- NULL
    }
    if (sign == TRUE) {
      signif <- .format_output(data = temp$sign, out_dims = out_dims,
                               out_coords = out_coords, out_attrs = out_attrs)
      signif$attrs$Variable$varName <- "RMSSS"
    } else {
      signif <- NULL
    }
    rmsss$attrs$Variable$varName <- "RMSSS"
    temp <- list(rmsss = rmsss, p.val = pval, sign = signif)
    data[["RMSSS"]] <- temp
  }
  if ("ROCSS" %in% metrics) {
    temp <- s2dv::ROCSS(exp, obs,
                        ref = ref, time_dim = time_dim,
                        memb_dim = memb_dim, cat_dim = cat_dim,
                        dat_dim = NULL,
                        prob_thresholds = prob_thresholds,
                        indices_for_clim = indices_for_clim,
                        cross.val = cross.val, ncores = ncores)
    rpss <- .format_output(data = temp$rpss, out_dims = out_dims,
                           out_coords = out_coords, out_attrs = out_attrs)
    signif <- .format_output(data = temp$sign, out_dims = out_dims,
                             out_coords = out_coords, out_attrs = out_attrs)
    rpss$attrs$Variable$varName <- "ROCSS"
    signif$attrs$Variable$varName <- "ROCSS"
    temp <- list(rpss = rpss, sign = signif)
    data[["ROCSS"]] <- temp
  }
  return(data)
}
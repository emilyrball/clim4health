#' Postprocess climate data including calibration...
#'
#' @param exp TBD
#' @param obs TBD
#' @param ref TBD
#' @param var TBD
#' @param lower TBD
#' @param upper TBD
#' @param cross_val TBD
#' @param sdate_dim TBD
#' @param memb_dim TBD
#' @param ncores TBD
#'
#' @returns TBD
#' @export
#'
#' @examples
#' # TBD
clim4health_postprocess <- function(exp,
                                    obs,
                                    ref = NULL,
                                    var = NULL,
                                    lower = 0.1,
                                    upper = 0.9,
                                    cross_val = TRUE,
                                    sdate_dim = "sdate",
                                    memb_dim = "ensemble",
                                    ncores = NULL) {
  if (is.null(exp) || is.null(obs)) {
    stop("Parameters 'exp' and 'obs' must be specified")
  }

  # get var name
  if (is.null(var)) {
    var <- .get_var(obs)
  }

  if (is.null(ncores)) {
    # if ncores is not specified, use half of the available cores
    ncores <- max(floor(parallel::detectCores() / 3), 1)
  } else if (ncores > (parallel::detectCores() - 1)) {
    # if ncores is greater than the number of available cores, stop
    stop("ncores cannot be equal to or greater than the available cores")
  }

  obs <- .get_array(obs)
  exp <- .get_array(exp)

  data <- list()

  # skill
  # if thresholds not specified, use default value
  data[[paste0("bss", lower)]] <- s2dv::RPSS(exp = exp,
                                             obs = obs,
                                             ref = ref,
                                             prob_thresholds = lower,
                                             cross.val = cross_val,
                                             time_dim = sdate_dim,
                                             memb_dim = memb_dim,
                                             ncores = ncores)
  data[[paste0("bss", upper)]] <- s2dv::RPSS(exp = exp,
                                             obs = obs,
                                             ref = ref,
                                             prob_thresholds = upper,
                                             cross.val = cross_val,
                                             time_dim = sdate_dim,
                                             memb_dim = memb_dim,
                                             ncores = ncores)
  data[["crpss"]] <- s2dv::CRPSS(exp = exp,
                                 obs = obs,
                                 ref = ref,
                                 clim.cross.val = cross_val,
                                 time_dim = sdate_dim,
                                 memb_dim = memb_dim,
                                 ncores = ncores)
  return(data)
}
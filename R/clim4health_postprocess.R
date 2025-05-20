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
                                    var = NULL,
                                    lower = 0.1,
                                    upper = 0.9,
                                    sdate_dim = "sdate",
                                    memb_dim = "ensemble",
                                    ncores = NULL) {
  # ref, hcst, fcst s2dv_cube objects with dimensions:
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

  ref <- .get_array(ref)
  hcst <- .get_array(hcst)

  data <- list()

  # skill
  # if thresholds not specified, use default value
  data[[paste0("bss", lower)]] <- s2dv::RPSS(exp = hcst,
                                             obs = ref,
                                             prob_thresholds = lower,
                                             cross.val = TRUE,
                                             time_dim = sdate_dim,
                                             memb_dim = memb_dim,
                                             ncores = ncores)
  data[[paste0("bss", upper)]] <- s2dv::RPSS(exp = hcst,
                                             obs = ref,
                                             prob_thresholds = upper,
                                             cross.val = TRUE,
                                             time_dim = sdate_dim,
                                             memb_dim = memb_dim,
                                             ncores = ncores)
  data[["crpss"]] <- s2dv::CRPSS(exp = hcst,
                                 obs = ref,
                                 clim.cross.val = TRUE,
                                 time_dim = sdate_dim,
                                 memb_dim = memb_dim,
                                 ncores = ncores)
  return(data)
}
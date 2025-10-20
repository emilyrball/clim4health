#' c4h_convert_units
#'
#' @param data an s2dv_cube object containing the data to be converted.
#' @param to a character string specifying the target units to convert to.
#' @param variable a character string specifying the variable to convert.
#' If NULL, the first variable will be used.
#' @param from a character string specifying the current units of the variable.
#' If NULL, the current units will be inferred from the data.
#'
#' @return an s2dv_cube object with the converted units.
#' @export
#'
#' @examples
#' TBD
c4h_convert_units <- function(data,
                              to,
                              variable = NULL,
                              from = NULL) {

  # Check class type
  if (!inherits(data, "s2dv_cube")) {
    stop("'c4h_convert_units' only accepts s2dv_cube objects.")
  }

  # Check existing relevant variable
  if (!is.null(variable) && !is.null(data$attrs$Variable$varName) &&
        !(variable %in% data$attrs$Variable$varName)) {
    stop("'data' does not contain the specified variable. Variables are: ",
         paste(data$attrs$Variable$varName, collapse = ", "))
  } else if (!is.null(variable) && !is.null(data$attrs$Variable$varName) &&
               variable %in% data$attrs$Variable$varName) {
    var_idx <- which(data$attrs$Variable$varName == variable)
  } else {
    var_idx <- 1
  }

  # Check valid from
  if (!is.null(from) &&
        !is.null(data$attrs$Variable$metadata$units[[var_idx]])) {
    if (!(from == data$attrs$Variable$metadata$units[[var_idx]])) {
      stop(paste0("The units of 'from' (", from, ") do not match the variable",
                  " units in 'data' (",
                  data$attrs$Variable$metadata$units[[var_idx]], ")."))
    }
  } else {
    warning("The units of 'from' were not checked because the variable",
            " units in 'data' are missing. Converting anyway.")
  }

  # valid units
  valid_units <- list(
    "mm_per_sec2", "cm_per_sec2", "m_per_sec2", "km_per_sec2", "grav",
    "inch_per_sec2", "ft_per_sec2", "mi_per_sec2", "kph_per_sec", "mph_per_sec",
    "degree", "radian", "grad", "arcmin", "arcsec", "turn",
    "nm2", "um2", "mm2", "cm2", "m2", "hectare", "km2", "inch2", "ft2", "yd2",
    "acre", "mi2", "naut_mi2",
    "dec_deg", "deg_dec_min", "deg_min_sec",
    "fmol", "pmol", "nmol", "umol", "mmol", "mol",
    "nsec", "usec", "msec", "sec", "min", "hr", "day", "wk", "mon", "yr", "dec",
    "cen", "mil", "Ma",
    "J", "kJ", "erg", "cal", "Cal", "Wsec", "kWh", "MWh", "BTU",
    "byte", "KB", "MB", "GB", "TB", "PB", "bit", "Kbit", "Mbit", "Gbit", "Tbit",
    "Pbit",
    "ml_per_sec", "ml_per_min", "ml_per_hr", "l_per_sec", "l_per_min",
    "l_per_hr", "m3_per_sec", "m3_per_min", "m3_per_hr", "gal_per_sec",
    "gal_per_min", "gal_per_hr",
    "ft3_per_sec", "ft3_per_min", "ft3_per_hr", "Sv",
    "angstrom", "nm", "um", "mm", "cm", "dm", "m", "km", "inch", "ft", "yd",
    "fathom", "mi", "naut_mi", "au", "light_yr", "parsec", "point",
    "Da", "fg", "pg", "ng", "ug", "mg", "g", "kg", "Mg", "Gg", "Tg", "Pg",
    "carat", "metric_ton", "oz", "lbs", "short_ton", "long_ton", "stone",
    "uW", "mW", "W", "kW", "MW", "GW", "erg_per_sec", "cal_per_sec",
    "cal_per_hr", "Cal_per_sec", "Cal_per_hr", "BTU_per_sec", "BTU_per_hr",
    "hp",
    "uatm", "atm", "Pa", "hPa", "kPa", "torr", "mmHg", "inHg", "cmH2O", "inH2O",
    "mbar", "bar", "dbar", "psi",
    "mm_per_sec", "cm_per_sec", "m_per_sec", "km_per_sec", "inch_per_sec",
    "ft_per_sec", "kph", "mph", "km_per_day", "mi_per_day", "knot", "mach",
    "light",
    "C", "F", "K", "R",
    "N-m", "ft-lbs", "inch-lbs",
    "ul", "ml", "dl", "l", "cm3", "dm3", "m3", "km3", "us_tsp", "us_tbsp",
    "us_oz", "us_cup", "us_pint", "us_quart", "us_gal", "inch3", "ft3", "mi3",
    "imp_tsp", "imp_tbsp", "imp_oz", "imp_cup", "imp_pint", "imp_quart",
    "imp_gal"
  )

  # stop if not valid units
  if (!(to %in% valid_units)) {
    stop(paste0("The specified 'to' units (", to, ") are not valid. Valid",
                " units are: ", paste(valid_units, collapse = ", "), "."))
  } else if (!is.null(from) && !(from %in% valid_units)) {
    stop(paste0("The specified 'from' units (", from, ") are not valid. Valid",
                " units are: ", paste(valid_units, collapse = ", "), "."))
  }

  from <- ifelse(is.null(from),
                 data$attrs$Variable$metadata$units[[var_idx]],
                 from)
  # Convert units
  data_out <- measurements::conv_unit(x = data$data,
                                      from = from,
                                      to = to)

  # Update variable units
  data$attrs$Variable$metadata$units[[var_idx]] <- to

  # Replace data
  data$data <- data_out

  return(data)
}
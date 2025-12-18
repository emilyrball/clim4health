#' c4h_convert
#'
#' Convert objects between formats including s2dv, terra, csv, polygons.
#'
#' @param data The object to transform
#' @param input_format A string specifying the format of the object 'data'.
#' Valid options are 's2dv', 'terra', 'csv' or 'polygons'
#' @param output_format A string specifying the desired output format.
#' Valid options are 's2dv', 'terra', 'csv' or 'polygons'
#' @param variable A string specifying the variable name to extract from the
#' object data when converting from terra to s2dv. The default is NULL.
#' @param dates  List of the dates you want to assign to the objects
#' format: yyyy-mm-dd. The default is NULL. ## REVIEW
#' @param aux_vals List of auxiliary values used for ensembles
#'
#' @returns An object could be s2db, terra, csv or polygons.
#' @export
#'
#' @examples
#' # Load example forecast data
#' fcst_path <- system.file("extdata", "forecast", package = "clim4health")
#' fcst <- c4h_load(fcst_path, variable = "t2m", ext = 'nc')
#'
#' ### Convert s2dv into terra
#' # out <- c4h_convert(fcst,
#' #                    "s2dv",
#' #                    "terra",
#' #                    variable = NULL,
#' #                    dates = NULL,
#' #                    aux_vals = NULL)
#' ## REVIEW - more examples
#' # s2dv gridded to terra - done
#' # s2dv gridded to csv
#' # s2dv point to csv
#' # s2dv polygon to csv
#' # s2dv polygon to polygons
#' # polygon data, point data,
c4h_convert <- function(data_object,
                        input_format,
                        output_format,
                        variable = NULL,
                        dates = NULL,
                        aux_vals = NULL) {

  if (input_format == "s2dv") {

    if (output_format == "terra") {
      return(.s2dv_to_terra(data_object))

    } else if (output_format == "csv") {
      return(.s2dv_to_csv(data_object))

    } else if (output_format == "polygons") {
      return(.s2dv_to_polygons(data_object)) ### REVIEW crs argument unused
    }

  } else if (input_format == "terra") {
    if (output_format == "s2dv") {
      return(.terra_to_s2dv(data_object,
                            dates = as.Date(dates),
                            ensemble_vals = aux_vals,
                            variable = variable))
    }

  } else if (input_format == "csv") {
    if (output_format == "s2dv") {
      return(.csv_to_s2dv(data_object))
    }

  } else if (input_format == "polygons")  {
    if (output_format == "s2dv") {
      return(.polygons_to_s2dv(data_object,
                               dates = as.Date(dates),
                               ensemble_vals = aux_vals))
                               # REVIEW - variable argument missing?
    }
  }
}
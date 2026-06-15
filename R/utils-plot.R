#' Subset arrays by dimension name
#'
#' @param x array to be subsetted
#' @param ... Additional arguments including the dimension name and the
#' corresponding index to subset.
#'
#' @returns the aggregated array
#' @noRd
.subset_array_named <- function(x, ...) {
  dims <- names(dim(x))
  index_list <- rep(list(TRUE), length(dim(x)))
  names(index_list) <- dims

  args <- list(...)
  for (n in names(args)) {
    if (!n %in% dims) {
      stop(paste("Invalid dimension name:", n))
    }
    index_list[[n]] <- args[[n]]
  }

  do.call(`[`, c(list(x), index_list, list(drop = FALSE)))
}

#' .abs_labels
#'
#' paste the absolute value of the input with a "-" prefix if the input is
#' negative, otherwise just the absolute value. This is used for axis labels
#' to remove the ordinal N/W/S/E.
#'
#' @param x Numeric vector of values for which to create labels.
#'
#' @returns Character vector of formatted labels.
#' @noRd
.abs_labels <- function(x) {
  paste0(ifelse(x < 0, "-", ""), abs(x))
}

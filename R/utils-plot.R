#' Subset arrays by dimension name
#'
#' @param x array to be subsetted
#' @param ...
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

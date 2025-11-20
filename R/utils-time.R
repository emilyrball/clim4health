#' .pad_array
#'
#' Pad data array to 31-day months with NAs so they can be merged.
#'
#' @param aggregated_data_i data array to be padded
#' @param dates_i vector of dates corresponding to the data
#' @param dim_aggregation dimension along which to aggregate
#'
#' @returns padded data array
#' @noRd
.pad_array <- function(aggregated_data_i, dates_i, dim_aggregation = "time") {

  # Identify dim_aggregation
  dims     <- dim(aggregated_data_i)
  dim_pos  <- which(names(dims) == dim_aggregation)

  # Find unique year–month pairs (“blocks”)
  years  <- lubridate::year(dates_i)
  months <- lubridate::month(dates_i)
  combos <- unique(data.frame(year = years, month = months))
  n_blocks <- nrow(combos)

  # New dimensions and empty array using 31 days per month
  new_len <- 31 * n_blocks
  dims_new <- dims
  dims_new[dim_pos] <- new_len
  out <- array(NA_real_, dim = dims_new, dimnames = dimnames(aggregated_data_i))

  # store new date labels
  dates_blocks <- vector("list", n_blocks)

  # Loop over combos' rows
  for (k in seq_len(n_blocks)) {
    y <- combos$year[k]
    m <- combos$month[k]

    # Get old indices and the day numbers
    old_idx <- which(years == y & months == m)
    block_days <- as.integer(format(dates_i[old_idx], "%d"))

    # Determine positions in the new array
    new_block_start <- (k - 1) * 31
    new_idx <- new_block_start + block_days

    idx_new <- rep(list(quote(expr = )), length(dims_new))
    idx_old <- rep(list(quote(expr = )), length(dims))
    idx_new[[dim_pos]] <- new_idx
    idx_old[[dim_pos]] <- old_idx

    # copy data into the right spots
    out <- do.call(
      `[<-`,
      c(
        list(out), idx_new,
        list(do.call(`[`, c(list(aggregated_data_i), idx_old)))
      )
    )
    # Build padded date labels
    padded_labels <- rep(NA_character_, 31)
    padded_labels[block_days] <- format(dates_i[old_idx], "%Y-%m-%d UTC")
    dates_blocks[[k]] <- padded_labels
  }

  # Final combined date vector
  dates_new <- unlist(dates_blocks, use.names = FALSE)

  list(array = out, dates = dates_new)
}

#' .aggregate_temporally
#'
#' Aggregate a vector temporally, allowing for NAs.
#'
#' @param y vector to be aggregated
#' @param fun aggregation function
#' @returns aggregated value or NA if more than 70% of values are NA
#' @noRd
.aggregate_temporally <- function(y, fun) {
  return(if ((sum(is.na(y)) / length(y)) > 0.7) NA else fun(y, na.rm = TRUE))
}
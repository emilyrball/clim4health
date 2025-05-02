#' .paste_hours_minutes
#'
#' @param hours Vector of hours.
#' @param minutes Vector of minutes.
#'
#' @returns A vector of concatenated hours and minutes.
#'
.paste_hours_minutes <- function(hours, minutes) {
  hours <- paste0(hours)
  minutes <- paste0(minutes)
  hours <- .paste_zeros(hours)
  minutes <- .paste_zeros(minutes)
  return(paste(hours, minutes, sep = ':'))
}

#' .paste_zeros
#'
#' @param times vector of time strings.
#'
#' @returns The vector of time strings with a leading 0 if applicable.
#'
.paste_zeros <- function(times) {
  for (i in seq_along(times)) {
    if (nchar(times[i]) == 1) {
      times[i] <- paste0("0", times[i])
    }
  }
  return(times)
}
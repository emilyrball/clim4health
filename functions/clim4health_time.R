# dependencies
print(getwd())
library(startR)
library(lubridate)
library(abind)
source( "./functions/clim4health_load.R" )


clim4health_time <- function (qube, agregation = 'day') {

  agregation = tolower(agregation)

  # if (agregation == 'day') {

  # } else if (agregation == 'week'){

  # } else if (agregation =='month') {

  # } else if (agregation == 'year') {

  # } else if (agregation == 'epi_week')

  return (qube)

}

path <- paste0('/esarchive/exp/ecmwf/s2s-monthly_ensfor/weekly_mean/',
                   'tas_f6h/tas_20191212.nc')
var  <- "tas"
qube <- clim4health_load (path, var)

# Generate the hours through values of timestamps inside the netcdf file
list_hours <- attr(qube, "Variables")$common$tas$dim[[4]]$vals
list_dates <- c()

init_string          <- attr(qube, "Variables")$common$tas$dim[[4]]$units
init_datetime_string <- sub("hours since ", "", init_string)
init_datetime        <- as.POSIXct(init_datetime_string, format="%Y-%m-%d %H:%M:%S.%OS")

for (hours in list_hours) {
  list_dates <- append(list_dates, format((init_datetime + hours(hours)), "%Y-%m-%d %H:%M:%S"))
}

# Generate the index to make sums to generate the matrix with the average of interested times
datetimes <- as.POSIXct(list_dates)
# Extract the hour from each datetime
days   <- as.Date(datetimes)
months <- format(datetimes, "%Y-%m")
years  <- year(datetimes)
weeks  <- paste(year(datetimes), week(datetimes), sep = "-")

# Create a list to hold the indices for each hour
grouped_indices <- split(seq_along(datetimes), years)

# - Also create a function to this part of the code
result_bool <- FALSE
for (time_indices in grouped_indices) {

  # time_indices <- grouped_indices[[1]]
  qube_subset <- qube[, , time_indices, , , , drop = FALSE]

  print(dim(qube_subset))

  # Move time to the last position
  qube_perm <- aperm(qube_subset, c(1, 2, 4, 5, 6, 3))

  # Apply the mean along the last (time) dimension (which is now dimension 6)
  qube_mean <- apply(qube_perm, c(1, 2, 3, 4, 5), mean)

  # Permute the array back to the original order
  qube_mean <- array(qube_mean, dim = c(dim(qube_mean), 1))
  qube_mean <- aperm(qube_mean, c(1, 2, 6, 3, 4, 5))

  # Add the name time for the dimension
  dim_names             <- names(dim(qube))
  names(dim(qube_mean)) <- dim_names

  # append two arrays by the dimension time
  if (result_bool) {
    result_qube             <- abind(result_qube, qube_mean, along = 3)
    dim_names               <- names(dim(qube))
    names(dim(result_qube)) <- dim_names
    

  } else {

    result_qube <- qube_mean
    result_bool <- TRUE

  }

}

a <- 0 

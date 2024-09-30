# dependencies
library(startR)
source( "clim4health_load.R" )

clim4health_time <- function (path, var, agregation = 'day') {

  qube <- clim4health_load (path, var)

  agregation = tolower(agregation)

  if (agregation == 'day') {

  } else if (agregation == 'week'){

  } else if (agregation =='month') {

  } else if (agregation == 'year') {

  } else if (agregation == 'epi_week')

}
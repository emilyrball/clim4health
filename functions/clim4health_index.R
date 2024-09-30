# dependencies
library(startR)
source( "clim4health_load.R" )


clim4health_index <- function (path, var, non_values = NA, sel_val = 1, bigger_val = 0, lower_val = 0, bigger_close = FALSE, lower_close = FALSE) {

  qube <- clim4health_load (path, var)

  if (bigger_close && lower_close) {
    qube_modified <- ifelse(qube >= lower_val & qube <= bigger_val, sel_val, non_values)
  } else if (bigger_close && lower_close == FALSE) {
    qube_modified <- ifelse(qube >= lower_val & qube < bigger_val, sel_val, non_values)
  } else if (bigger_close == FALSE && lower_close) {
    qube_modified <- ifelse(qube > lower_val & qube <= bigger_val, sel_val, non_values)
  } else {
    qube_modified <- ifelse(qube > lower_val & qube < bigger_val, sel_val, non_values)
  }

  return (qube_modified)
}




# dependencies
library(startR)


clim4health_index <- function (qube, mod_vals = TRUE,  non_values = NA, sel_val = 1, bigger_val = 0, lower_val = 0, bigger_close = FALSE, lower_close = FALSE) {

  # Don't modify the values that do not accomplish the condition
  if (mod_vals == FALSE) {

    non_values = qube
  
  }

  # Modify the values in the indicated range
  if (bigger_close && lower_close) {

    qube_modified <- ifelse(qube >= lower_val & qube <= bigger_val, sel_val, non_values)
  
  } else if (bigger_close && lower_close == FALSE) {

    qube_modified <- ifelse(qube >= lower_val & qube < bigger_val, sel_val, non_values)
  
  } else if (bigger_close == FALSE && lower_close) {

    qube_modified <- ifelse(qube > lower_val & qube <= bigger_val, sel_val, non_values)
  
  } else {

    qube_modified <- ifelse(qube > lower_val & qube < bigger_val, sel_val, non_values)
  
  }
  
  qube[] <- qube_modified

  return (qube)
}

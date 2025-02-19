# function to postprocess climate data, maintaining the same object dimensions
# and adding information to the metadata

# dependencies
library(CSTools)

clim4health_calibrate <- function() {
  # calls CST_Calibrate
}

clim4health_skill <- function() {
  # calls CST_RPSS
}

clim4health_postprocess <- function() {
  # calls clim4health_calibrate
  # calls clim4health_skill
}
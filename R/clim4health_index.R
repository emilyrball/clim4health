# function to obtain a threshold-based indicator, maintaining the same object dimensions and adding information to the metadata

# dependencies
# none

clim4health_index <- function (data, mod_vals = TRUE,  lower_threshold = 0, upper_threshold = 0, lower_close = FALSE, upper_close = FALSE) {

    qube <- data$data

    # Modify the values in the indicated range
    if (upper_close && lower_close) {

        qube_modified <- ifelse(qube >= lower_threshold & qube <= upper_threshold, 1, 0)
    
    } else if (upper_close && lower_close == FALSE) {

        qube_modified <- ifelse(qube >= lower_threshold & qube < upper_threshold, 1, 0)
    
    } else if (upper_close == FALSE && lower_close) {

        qube_modified <- ifelse(qube > lower_threshold & qube <= upper_threshold, 1, 0)
    
    } else {

        qube_modified <- ifelse(qube > lower_threshold & qube < upper_threshold, 1, 0)
    
    }
    
    # if mod_vals == TRUE, return original values where the condition is fullfilled and NA where it is not
    # else, return 1 where the condition is fullfielled and 0 where it is not
    if (mod_vals) {
        qube[which(qube_modified == 0)] <- NA
    } else {
        qube <- qube_modified
    }

    # metadata
    result <- data
    result$data <- qube
    result$attrs$index$mod_vals <- mod_vals
    result$attrs$index$lower_threshold <- paste0(ifelse(lower_close, '>=', '>'), lower_threshold)
    result$attrs$index$upper_threshold <- paste0(ifelse(upper_close, '<=', '<'), upper_threshold)
    
    return (result)
}
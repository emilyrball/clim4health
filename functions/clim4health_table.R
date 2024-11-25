# Define a function to process s2dv_cube data and assign levels
clim4health_table <- function(array) {
    
  # Convert s2dv_cube data to a data frame with factor levels
  table.format <- as.data.frame.table(array$data)
  
  # Set the column names of table.format
  # Include dimension names and 'values' column
  names(table.format) <- c(names(dim(array$data)), 'values') 
  
  # Loop through each dimension in the s2dv_cube
  for (dim_name in names(array$dims)) {
    # Check if the column exists in table.format
    if (dim_name %in% colnames(table.format)) {
      # Update factor levels in each column using the corresponding s2dv_cube coords element
      levels(table.format[[dim_name]]) <- array$coords[[dim_name]][1:length(array$coords[[dim_name]])]
    }
  }

  # Round values to 2 digits 
  #table.format$values <- round(table.format$values, 2)
  
  # Return the updated table.format
  return(table.format)
}

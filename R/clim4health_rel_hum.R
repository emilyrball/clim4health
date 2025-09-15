
c4h_rel_hum <- function(tas, tdps){

  # Check class type
  if(!inherits(tas, "s2dv_cube") | !inherits(tdps, "s2dv_cube")){
    stop("'c4h_rel_hum' only accepts s2dv_cube objects as input.")
  }

  # Define constants
  b <- 17.625
  c <- 243.04

  # Initialize s2dv_cube for output
  hurs <- tas

  hurs$data <- 100 * exp(b * c * (tdps$data - tas$data) / 
                ((c + tdps$data) * (c + tas$data))) 
  hurs$attrs$Variable$metadata[["rel_hum"]]$units <- "percent"
  return(hurs)

}
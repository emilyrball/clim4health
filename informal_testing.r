
library(s2dv)
library(lubridate)
library(raster)
library(sf)
library(tidyverse)
library(exactextractr)


path <- getwd()
source(paste0(path, '/functions/clim4health_load.R'))        

path <- getwd()
fcst <- clim4health_load(paste0(path, '/sample_data/ecmwf51/forecast/t2m_20240401.nc'))        

class(fcst)
#[1] "s2dv_cube"

str(fcst)

dim(fcst$data)
#  dataset       var      time  ensemble  latitude longitude
#        1         1         3        51        57        77

########## function with fcst ############

table.test<- clim4health_table(fcst)

fcst$data[1,1,1,1,1,1]



# Example s2dv_cube structure
example_array <- list(
   data = array(data = runif(24), dim = c(2, 3, 4),
                dimnames = list(dataset = NULL, var = NULL, time = NULL)),
   dims = list(dataset = 1:2, var = 1:3, time = 1:4),
   coords = list(
     dataset = c("Model1", "Model2"),
     var = c("Temperature", "Precipitation", "Pressure"),
     time = seq.Date(from = as.Date("2023-01-01"), by = "day", length.out = 4)
   )
 )

 # Convert to a well-formatted data frame
 formatted_table <- clim4health_table(example_array)
 head(formatted_table)

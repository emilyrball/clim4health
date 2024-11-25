
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

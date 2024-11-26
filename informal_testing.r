
library(s2dv)
library(lubridate)
library(raster)
library(sf)
library(tidyverse)
library(exactextractr)


path <- getwd()
source(paste0(path, '/functions/clim4health_load.R'))        

########## function with fcst ############

## load forecast fcst
fcst <- clim4health_load(paste0(path, '/sample_data/ecmwf51/forecast/t2m_20240401.nc'))        

class(fcst)
#[1] "s2dv_cube"

dim(fcst$data)
#  dataset       var      time  ensemble  latitude longitude
#        1         1         3        51        57        77

########## function with hcst ##########

f.table.test<- clim4health_table(fcst)

fcst$data[1,1,1,1,1,1

## load hindcast hcst

source(paste0(path, '/functions/clim4health_path.R'))    
for (yy in c(2010, 2011, 2012)){
    hcst <- clim4health_load(paste0(path, '/sample_data/ecmwf51/hindcast/t2m_', yy, '0401.nc'))        
}


class(hcst)
#[1] "s2dv_cube"

dim(hcst$data)
#  dataset       var      time  ensemble  latitude longitude
#        1         1         3        51        57        77
hcst$coords$time
#[1] "2012-04-01" "2012-05-01" "2012-06-01"

hcst$coords$ensemble
# [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25

h.table.test<- clim4health_table(hcst)

table(h.table.test$time)

table(h.table.test$ensemble)


########## function with non s2dv_cube ##########

array<- fcst
class(array)

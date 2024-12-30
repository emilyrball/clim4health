

library(s2dv)
library(lubridate)
library(raster)
library(sf)
library(tidyverse)
library(exactextractr)


path <- getwd()
source(paste0(path, '/functions/clim4health_load.R'))        

source(paste0(path, '/functions/clim4health_table.R'))        


########## function with fcst ############

## load forecast fcst
fcst <- clim4health_load(paste0(path, '/sample_data/ecmwf51/forecast/t2m_20240401.nc'))        

class(fcst)
#[1] "s2dv_cube"

dim(fcst$data)
#  dataset       var      time  ensemble  latitude longitude
#        1         1         3        51        57        77

########## function with hcst ##########

f.table <- clim4health_table(fcst)

# test lag_cov works with table output 

source(paste0(path, '/lag_cov.R'))       
source(paste0(path, '/helper_functions.R'))    

data_lagged <- lag_cov(
data = f.table, time = "time", area = c("ensemble", "latitude", "longitude"),
var = c("var"), lag = 3
)

f.table.filter<- f.table %>% 
dplyr::select(c("dataset", "var", "time", "ensemble","values")) %>% 
distinct()  %>% 
mutate(time= lubridate::ymd(time))
 
 
f.table.filter.1<- f.table %>% 
dplyr::filter(ensemble==1 & latitude == "33" & longitude== "-93") %>% 
dplyr::select(c("dataset", "var", "time", "values")) %>% 
distinct()  %>% 
mutate(time= lubridate::ymd(time))

data_lagged <- lag_cov(
data = f.table.filter.1, time = "time",
var = "values", lag = 3
)


class(f.table.filter.1$time)

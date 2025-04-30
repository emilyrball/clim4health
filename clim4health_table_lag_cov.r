

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

saveRDS(f.table, "f.table.rds")
# test lag_cov works with table output 

source(paste0(path, '/lag_cov.R'))       
source(paste0(path, '/helper_functions.R'))    

data_lagged <- lag_cov(
data = f.table, time = "time", area = c("ensemble", "latitude", "longitude"),
var = c("var"), lag = 3
)

f.table.filter<- f.table %>% 
dplyr::filter(latitude %in% c("33","32") & longitude %in% c("-93", "-92")) %>% 
#dplyr::select(c("dataset", "var", "time", "values")) %>% 
distinct()  %>% 
mutate(time= as.character(time))
 
 
f.table.filter.1<- f.table %>% 
dplyr::filter(ensemble==1 & latitude == "33" & longitude== "-93") %>% 
dplyr::select(c("dataset", "var", "time", "values")) %>% 
distinct()  %>% 
mutate(time= as.character(time))

data_lagged <- lag_cov(
data = f.table.filter, time = "time",area = c("ensemble", "latitude", "longitude"),
var = "values", lag = c(1,2), merge= TRUE
)


class(f.table.filter.1$time)


### does lag_cov_group work with f.table.filter with multiple groups?

data= f.table.filter

var = "values"

time= "time"

lag= c(1,2)

area = c("ensemble", "latitude", "longitude")

merge=TRUE

f.lagged_group <- lag_cov_group(data= f.table.filter,

var = "values",

time= "time",

lag= c(1,2),

area = c("ensemble", "latitude", "longitude"),

merge=TRUE)

### this works

    i=2
    x= groups[[1]]
    v="values"

    for (v in var) {
      lagged_data[[v]] <- lapply(X = groups, FUN = function(x) {
        for (i in lag[1]:lag[2]) {
          x[[paste0(v, ".l", i)]] <- c(
            rep(NA, i),
            x[[v]]
          )[1:length(x[[v]])]
        }
        return(x)

### this works

        for (i in lag[1]:lag[2]) {
          x[[paste0(v, ".l", i)]] <- c(
            rep(NA, i),
            x[[v]]
          )[1:length(x[[v]])]
        }
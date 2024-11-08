path <- getwd()

install.packages("startR")
library(startR)

install.packages("lubridate")
library(lubridate)

install.packages("easyNCDF")
library(easyNCDF)

install.packages("CSTools")
library(CSTools)

source(paste0(path, '/functions/clim4health_load.R'))

path <- getwd()
fcst <- clim4health_load(paste0(path, '/sample_data/ecmwf51/forecast/t2m_20240401.nc'))        

path = paste0(path, '/sample_data/ecmwf51/forecast/t2m_20240401.nc')

file.dims <- names(NcReadDims(path))



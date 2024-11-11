
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

dim(fcst$data)
#  dataset       var      time  ensemble  latitude longitude
#        1         1         3        51        57        77

fcst$coords$time
# [1] "2024-04-01" "2024-05-01" "2024-06-01"

fcst$coords$latitude[1:length(fcst$coords$latitude)]
# [1]  33  32  31  30  29  28  27  26  25  24  23  22  21  20  19  18  17  16  15
# [20]  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0  -1  -2  -3  -4
# [39]  -5  -6  -7  -8  -9 -10 -11 -12 -13 -14 -15 -16 -17 -18 -19 -20 -21 -22 -23
# simply fcst$coords$latitude will show more information
# on the latitude in addition to the values

fcst$coords$longitude[1:length(fcst$coords$longitude)]
# [1] -93 -92 -91 -90 -89 -88 -87 -86 -85 -84 -83 -82 -81 -80 -79 -78 -77 -76 -75
# [20] -74 -73 -72 -71 -70 -69 -68 -67 -66 -65 -64 -63 -62 -61 -60 -59 -58 -57 -56
# [39] -55 -54 -53 -52 -51 -50 -49 -48 -47 -46 -45 -44 -43 -42 -41 -40 -39 -38 -37
# [58] -36 -35 -34 -33 -32 -31 -30 -29 -28 -27 -26 -25 -24 -23 -22 -21 -20 -19 -18
# [77] -17

path <- getwd()
for (yy in c(2010, 2011, 2012)){
    hcst <- clim4health_load(paste0(path, '/sample_data/ecmwf51/hindcast/t2m_', yy, '0401.nc'))        
}

class(hcst)
#[1] "s2dv_cube"

dim(hcst$data)
#  dataset       var      time  ensemble  latitude longitude

fcst$coords$time
# [1] "2024-04-01" "2024-05-01" "2024-06-01"

fcst$coords$latitude[1:length(fcst$coords$latitude)]
# [1]  33  32  31  30  29  28  27  26  25  24  23  22  21  20  19  18  17  16  15
# [20]  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0  -1  -2  -3  -4
# [39]  -5  -6  -7  -8  -9 -10 -11 -12 -13 -14 -15 -16 -17 -18 -19 -20 -21 -22 -23
# simply fcst$coords$latitude will show more information
# on the latitude in addition to the values

fcst$coords$longitude[1:length(fcst$coords$longitude)]
# [1] -93 -92 -91 -90 -89 -88 -87 -86 -85 -84 -83 -82 -81 -80 -79 -78 -77 -76 -75
# [20] -74 -73 -72 -71 -70 -69 -68 -67 -66 -65 -64 -63 -62 -61 -60 -59 -58 -57 -56
# [39] -55 -54 -53 -52 -51 -50 -49 -48 -47 -46 -45 -44 -43 -42 -41 -40 -39 -38 -37
# [58] -36 -35 -34 -33 -32 -31 -30 -29 -28 -27 -26 -25 -24 -23 -22 -21 -20 -19 -18
# [77] -17


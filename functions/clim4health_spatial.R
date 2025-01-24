# function to aggregate gridded data to a shapefile

# dependencies
#library(lubridate)
#library(raster)
library(sf)
#library(tidyverse)
library(exactextractr)

# main function
clim4health_spatial <- function(data, shapefile, 
                                lat_dim.name = 'latitude', lon_dim.name = 'longitude',
                                aggregation_function = 'mean') {

    # read shapefile from file:
    shp <- st_read(shapefile)
        
    # read metadata
    lat = data$coords$latitude
    lon = data$coords$longitude

    # transform array to raster stack
        out <- NULL
        dt.out <- NULL
        num <- 0
        columns_dates <- c()
        columns_index <- c()

        for (dat in 1:dim(data$data)['dataset']){
            for (vv in 1:dim(data$data)['var']){
                for (yy in 1:dim(data$data)['year']){
                    for (mm in 1:dim(data$data)['month']){
                        for (dd in 1:dim(data$data)['day']){
                            for (tt in 1:dim(data$data)['time_of_day']){
                                for (ee in 1:dim(data$data)['ensemble']){
                                    num <- num + 1
                                    out[[num]] <- raster::raster(data$data[dat,vv,yy,mm,dd,tt,ee,,],
                                                                 xmn = min(lon), xmx = max(lon),
                                                                 ymn = min(lat), ymx = max(lat))
                                    raster::crs(out[[num]]) <- "+init=epsg:4326"  # should be read from data$metadata!!!
                                    #columns_dates <- c(columns_dates, paste0('ensemble_', ifelse(ee<10, '0', ''), ee, '_date_', substr(dates[yy,tt], 1,10)))
                                    columns_index <- c(columns_index, paste0(ifelse(dat<10, '0', ''), dat, '_', 
                                                                             ifelse(vv<10, '0', ''), vv, '_', 
                                                                             ifelse(yy<10, '0', ''), yy, '_',
                                                                             ifelse(mm<10, '0', ''), mm, '_',
                                                                             ifelse(dd<10, '0', ''), dd, '_',
                                                                             ifelse(tt<10, '0', ''), tt, '_',
                                                                             ifelse(ee<10, '0', ''), ee, '_'))
                                }
                            }
                        }
                    }
                }
            }
        }
        out <- raster::stack(out)

    # extract region data (for all regions in shp) with exact_extractr
        out.dt <- exact_extract(out, shp, fun = aggregation_function)
        #if (is.null(dim(out.dt))){out.dt <- data.frame(out.dt)}
        #names(out.dt) <- columns_dates

    # transform to array
        result <- array(NA, dim = c(region = nrow(out.dt), 
                                    dim(data$data)['dataset'],
                                    dim(data$data)['var'],
                                    dim(data$data)['year'],
                                    dim(data$data)['month'],
                                    dim(data$data)['day'],
                                    dim(data$data)['time_of_day'],
                                    dim(data$data)['ensemble']))
        index <- strsplit(columns_index, '_')
        for (cc in 1:length(columns_index)){
            dat <- as.integer(index[[cc]][1])
            vv <- as.integer(index[[cc]][2])
            yy <- as.integer(index[[cc]][3])
            mm <- as.integer(index[[cc]][4])
            dd <- as.integer(index[[cc]][5])
            tt <- as.integer(index[[cc]][6])
            ee <- as.integer(index[[cc]][7])
            result[,dat,vv,yy,mm,dd,tt,ee] <- out.dt[,cc]
        }
    
    return(result)
}


library(s2dv)
library(lubridate)
library(raster)
library(sf)
library(tidyverse)
library(exactextractr)

## ----------------------------------------------------------------
### convert to raster and extract spatial mean with exactextractr
## ----------------------------------------------------------------

raster.extract <- function(data, shp, lat.dim = 'latitude', lon.dim = 'longitude', year.dim = 'syear', time.dim = 'time', ens.dim = 'ensemble'){
    # requirements: ensemble, lat and lon need to be the last 3 coordinates of data
    #               multidimenstional array with year.dim, time.dim, ens.dim, lat.dim, lon.dim
    #               any extra dimension is allowed as long as it has length 1 (and it will be removed)
    
    lon <- attr(data, "Variables")$dat1$longitude
    lat <- attr(data, "Variables")$dat1$latitude
    dates <- drop(attr(data, "Variables")$common$time)
    
    # remove extra dimensions and add necessary dimensions (if they are not in the original data)
        data <- drop(data)
        for (dim.name in c(ens.dim, time.dim, year.dim)){
            if (!(dim.name %in% names(dim(data)))){
                data <- s2dv::InsertDim(data, pos = 1, len = 1, name = dim.name)
            }
        }
    # re-order dimenstions to dimnames.order
        dimnames.order <- c(year.dim, time.dim, ens.dim, lat.dim, lon.dim)
        new.order <- c()
        for(ii in 1:length(dimnames.order)){
            new.order <- c(new.order, which(names(dim(data)) == dimnames.order[ii])) 
        }
        data <- aperm(data, new.order)
    # length of dimensions:
        nensembles <- dim(data)[ens.dim][[1]]
        ntimes <- dim(data)[time.dim][[1]]
        nyears <- dim(data)[year.dim][[1]]

        if (is.null(dim(dates))){
            if (nyears == 1){
                dates <- array(dates, dim = c(syear = 1, time = length(dates)))
            } else {
                dates <- array(dates, dim = c(syear = length(dates), time = 1))
            }
        } else {
            dates <- aperm(dates, c(which(names(dim(dates)) == year.dim), which(names(dim(dates)) == time.dim)))
        }
        
    # transform to raster stack
        out <- NULL
        dt.out <- NULL
        num <- 0
        columns_dates <- c()
        columns_index <- c()
        for (ee in 1:nensembles){
            for (tt in 1:ntimes){
                for (yy in 1:nyears){
                    num <- num + 1
                    out[[num]] <- raster::raster(data[yy,tt,ee,,], 
                                        xmn=min(lon), xmx=max(lon), 
                                        ymn=min(lat), ymx=max(lat))
                    raster::crs(out[[num]]) <- "+init=epsg:4326" 
                    columns_dates <- c(columns_dates, paste0('ensemble_', ifelse(ee<10, '0', ''), ee, '_date_', substr(dates[yy,tt], 1,10)))
                    columns_index <- c(columns_index, paste0(ifelse(ee<10, '0', ''), ee, '_', ifelse(yy<10, '0', ''), yy, '_', ifelse(tt<10, '0', ''), tt))
                }
            }
        }
        out <- raster::stack(out)
    # extract region data (for all regions in shp) with exact_extractr
        out.dt <- exact_extract(out, shp, fun = 'mean')
        if (is.null(dim(out.dt))){out.dt <- data.frame(out.dt)}
        names(out.dt) <- columns_dates

    # transform to array
        result <- array(NA, dim = c(region = nrow(out.dt), syear = nyears, time = ntimes, ensemble = nensembles))
        for (cc in 1:length(columns_index)){
            ee <- as.integer(substr(columns_index[cc], 1, 2))
            yy <- as.integer(substr(columns_index[cc], 4, 5))
            tt <- as.integer(substr(columns_index[cc], 7, 8))
            result[,yy,tt,ee] <- out.dt[,cc]
        }

    return(result)
}
# Tutorial for clim4health_time

## 1. Load the functions from clim4health


```R
source('functions/clim4health_load.R')
source('functions/clim4health_time.R')

library(lubridate)
library(dplyr)
```

-------------------------------------------------------------------------------
## 2. Load the dataset with the function clim4health_load


```R
data_clim <- clim4health_load('./sample_data/ecmwf51/forecast/t2m_20240401.nc')
```

---------------------------------------------------------------------------------
## 3. Daily aggregations
- Perfform daily aggregations
- Plot a map for one day


```R
# Perform Daily Aggregation
daily_data <- clim4health_time(data_clim, aggregation = "daily")
```


```R
PlotEquiMap(var = daily_data$data[1, 1, 1, 1, , ],
            lat = data_clim$coords$latitude,
            lon = data_clim$coords$longitude,
            filled.continents = FALSE,
            units = 'Temperature (C)', # text to appear in the legend
            toptitle = 'Mean daily t2m', # text to appear as overall title
            colNA = '#ffffff'
            )
```


    
![png](figures/clim4health_time/output_7_0.png)
    


------------------------------------------------------------------------
## 4. Monthly aggregations
- Perform monthly aggregation
- Plot a map for one day


```R
# Perform Monthly Aggregation
monthly_data <- clim4health_time(data_clim, aggregation = "monthly")
```


```R
PlotEquiMap(var = monthly_data$data[1, 1, 1, 1, , ],
            lat = data_clim$coords$latitude,
            lon = data_clim$coords$longitude,
            filled.continents = FALSE,
            units = 'Temperature (C)', # text to appear in the legend
            toptitle = 'Mean monthly t2m', # text to appear as overall title
            colNA = '#ffffff'
            )
```


    
![png](figures/clim4health_time/output_10_0.png)
    


-----------------------------------------------------------------------
## 5. Yearly aggregations
- Perform yearly aggregations
- Plot a map for one year


```R
# Perform Yearly Aggregation
yearly_data <- clim4health_time(data_clim, aggregation = "yearly")
```


```R
PlotEquiMap(var = yearly_data$data[1, 1, 1, 1, , ],
            lat = data_clim$coords$latitude,
            lon = data_clim$coords$longitude,
            filled.continents = FALSE,
            units = 'Temperature (C)', # text to appear in the legend
            toptitle = 'Mean yearly t2m', # text to appear as overall title
            colNA = '#ffffff'
            )
```


    
![png](figures/clim4health_time/output_13_0.png)
    


## **. Errors 
- In the dimensions of the s2dvqube the name of the dimension time is 3


```R
dim(data_clim$data) # Datos originales
```


<style>
.dl-inline {width: auto; margin:0; padding: 0}
.dl-inline>dt, .dl-inline>dd {float: none; width: auto; display: inline-block}
.dl-inline>dt::after {content: ":\0020"; padding-right: .5ex}
.dl-inline>dt:not(:first-of-type) {padding-left: .5ex}
</style><dl class=dl-inline><dt>dataset</dt><dd>1</dd><dt>var</dt><dd>1</dd><dt>time</dt><dd>3</dd><dt>ensemble</dt><dd>51</dd><dt>latitude</dt><dd>57</dd><dt>longitude</dt><dd>77</dd></dl>




```R
dim(yearly_data$data) # Datos generados
```


<style>
.dl-inline {width: auto; margin:0; padding: 0}
.dl-inline>dt, .dl-inline>dd {float: none; width: auto; display: inline-block}
.dl-inline>dt::after {content: ":\0020"; padding-right: .5ex}
.dl-inline>dt:not(:first-of-type) {padding-left: .5ex}
</style><dl class=dl-inline><dt>dataset</dt><dd>1</dd><dt>var</dt><dd>1</dd><dt>3</dt><dd>1</dd><dt>ensemble</dt><dd>51</dd><dt>latitude</dt><dd>57</dd><dt>longitude</dt><dd>77</dd></dl>



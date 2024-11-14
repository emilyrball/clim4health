# harmonize-clim4health

This is a private repo for the development of the functions, the public repo is here: https://github.com/harmonize-tools/clim4health

## clim4health current public README

clim4health is a tool developed within the HARMONIZE project with the aim of post-processing climate data harmonized to the spatiotemporal aggregation of health data. The tool consists in an R-package and its documentation including examples on how to use the tool and recommendations of parameter selection in some case studies.

The functions that are part of the tool allow for:

- calibration and quality assessment of climate forecasts
- harmonization of the spatial resolution of the climate data with techniques of statistical spatial downscaling or aggregation to shapefiles
- harmonization of the temporal resolution of the climate data with aggregation to epidemiological week or coarser aggregations
- index calculation (e.g. threshold based index)
- visualization of results
- output as .csv file harmonized to the requested format

## Functions to develop

- GET: (optional use) download from Copernicus and save file -> Daniela
- LOAD: load data into R object (startR) from netcdf or from csv (weather stations) -> Alba
- SPATIAL: spatial harmonization (exactextractR) -> Alba
- POSTPROCESS: calibration/downscaling and quality assessment -> Alba
- INDEX: calculation of simple indices (threshold based) to return NA vs original values or 0 vs 1 -> Raúl
- TIME: temporal harmonization from subdaily to daily, daily to weekly, daily to monthly, and monthly to annual -> Raúl
- TABLE: from array to table -> Alba
- PLOT: visualization (save plot png) -> (to do later)
- SAVE: transform array to table and save as csv or save grid -> (to do later)

# clim4health  <img src='inst/figures/logo_harmonize.png' align="right" width="9%" /> <span> <span> <img src='inst/figures/logo_BSC.png' align="right" width="16%" /> 

<!-- badges: start -->
[![License](https://img.shields.io/badge/License-AGPL_v3-brightgreen.svg?style=flat)](https://www.gnu.org/licenses/agpl-3.0.html)
![coverage](https://gitlab.earth.bsc.es/ghr/clim4health/badges/main/coverage.svg)
<!-- [![CRAN status](https://www.r-pkg.org/badges/version/clim4health)](https://cran.r-project.org/package=clim4health) -->
<!-- [![CRAN downloads total](http://cranlogs.r-pkg.org/badges/grand-total/clim4health)](https://cran.r-project.org/package=clim4health) -->
<!-- [![CRAN downloads month](http://cranlogs.r-pkg.org/badges/clim4health)](https://cran.r-project.org/package=clim4health) -->
<!-- badges: end -->

<img src='inst/figures/clim4health.jpg' align="right" width="14%" />

## Overview

**clim4health** is an R package designed to obtain, transform and export climate 
data for their use in epidemiological analyses and other types of applications. 
The package contains a series of functions structured in three sequential blocks: 
input, transformation, and output. 

In the input block, **clim4health** provides functions to download several types of 
climate data including reanalyses, seasonal forecasts and hindcasts, and weather 
stations and load them into memory for their processing. The transformation block
includes functions to postprocess and downscale climate data, perform spatiotemporal
aggregations, as well as compute threshold-based suitability indicators. Finally, 
in the output block, functions to visualize and export the transformed data are 
provided.

<br>
<div align="center">
<img src='inst/figures/clim4health_package_structure.svg' width="80%" />
</div>
<br> 

**clim4health** is one of the packages developed by the
[Global Health Resilience](https://www.bsc.es/discover-bsc/organisation/research-departments/global-health-resilience) (GHR)
team at the [Barcelona Supercomputing Center](https://www.bsc.es/) (BSC) within 
the [HARMONIZE](https://www.harmonize-tools.org/) project, which comprises 
different R and Python libraries tailored for health, climate, environmental, 
and socioeconomic data acquisition, harmonisation, and visualization.

More information about the R packages developped at GHR for climate and health workflows 
and an online version of the package documentation can be found at the
[GHR tools website](https://bsc-es.github.io/GHRtools/).


## Installation

```R
# Install from CRAN
install.packages("clim4health")

# Get the development version from Gitlab
devtools::install_git('https://earth.bsc.es/gitlab/ghr/clim4health.git')
```

## Usage

```R
# Load climate reanalysis temperature data included in the package
# and plot it for the study area
library("clim4health")
library("sf")

# Region municipalities
mun_path <- system.file("extdata", "areas", "munip_vallecauca.gpkg",
                        package = "clim4health")
mun <- read_sf(mun_path)

# Reanalysis data
rean_path <- system.file("extdata", "reanalysis", package = "clim4health")
rean <- c4h_load(rean_path, variable = "t2m", ext = 'nc')

# clim4health plot
c4h_plot(rean, 
         boundaries = mun,
         mask_boundaries = TRUE, 
         coordgrid = TRUE)
```

<div align="center">
<img src='inst/figures/README-example.png' width="60%" />
</div>

## Developers

**[Emily Ball, PhD](https://www.bsc.es/ball-emily)**
<a href="https://orcid.org/0000-0002-3002-4068" style="margin-left: 15px;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" alt="ORCID" style="width: 16px; height: 16px;" /></a>\
Barcelona Supercomputing Center\
Climate Services

**Alba Llabrés, PhD**
<a href="https://orcid.org/0000-0003-2144-675X" style="margin-left: 15px;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" alt="ORCID" style="width: 16px; height: 16px;" /></a>\
Barcelona Supercomputing Center\
Climate Services

**[Carles Milà, PhD](https://www.bsc.es/mila-garcia-carles)**
<a href="https://orcid.org/0000-0003-0470-0760" style="margin-left: 15px;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" alt="ORCID" style="width: 16px; height: 16px;" /></a>\
Barcelona Supercomputing Center\
Global Health Resilience

**Raul Capellan Fernandez, MSc** \
Barcelona Supercomputing Center\
Earth Data and Diagnostics

**[Rebeca Nunes, MSc](https://www.bsc.es/es/nunes-rodrigues-rebeca)** 
<a href="https://orcid.org/0009-0009-8738-0985" style="margin-left: 15px;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" alt="ORCID" style="width: 16px; height: 16px;" /></a>\
Barcelona Supercomputing Center\
Earth Data and Diagnostics

**[Daniela Lührsen, MSc](https://www.bsc.es/luhrsen-daniela-sofie)**
<a href="https://orcid.org/0009-0002-6340-5964" style="margin-left: 15px;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" alt="ORCID" style="width: 16px; height: 16px;" /></a>\
Barcelona Supercomputing Center\
Global Health Resilience

**[Anna B. Kawiecki, PhD](https://www.bsc.es/kawiecki-peralta-ania)**
<a href="https://orcid.org/0000-0002-0499-2612" style="margin-left: 15px;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" alt="ORCID" style="width: 16px; height: 16px;" /></a>\
Barcelona Supercomputing Center\
Global Health Resilience

**[Rachel Lowe, PhD](https://www.bsc.es/lowe-rachel)**
<a href="https://orcid.org/0000-0003-3939-7343" style="margin-left: 15px;"><img src="https://orcid.org/sites/default/files/images/orcid_16x16.png" alt="ORCID" style="width: 16px; height: 16px;" /></a>\
Barcelona Supercomputing Center\
Global Health Resilience (Group leader)

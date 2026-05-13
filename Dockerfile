# --------------------------------------------------------
# Clim4Health CI Docker Image
# Pre-installs R, system deps, CRAN packages, covr, testthat
# --------------------------------------------------------

# FROM r-base:latest
FROM r-base:4.5.1

# Avoid prompts in apt
ENV DEBIAN_FRONTEND=noninteractive

# --------------------------------------------------------
# System dependencies for R packages
# --------------------------------------------------------
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    build-essential \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libuv1-dev \
    gdal-bin \
    cmake \
    libabsl-dev \
    pandoc \
    qpdf \
    wget \
    cdo \
    nco \
    && rm -rf /var/lib/apt/lists/*

# --------------------------------------------------------
# Pre-install CRAN R packages including coverage tools
# --------------------------------------------------------
RUN R -e "\
    install.packages( \
      c('fs', 'colorspace', 'covr', 'cowplot', 'dplyr', \
        'ISOweek', 'knitr', 'lubridate', 'RColorBrewer', 'remotes', \
        'rlang', 'rmarkdown', 'roxygen2', 'sf', 'testthat', 'tidyr', \
        'CSTools','png', 'filelock', 'rstudioapi', 'sp', 'gridpattern', \
        'keyring', 'getPass', 'cubelyr', 'spData', 'ggpattern', 'CSDownscale', \
        'terra', 'stars', 'ecmwfr', 'GHRexplore'), \
      INSTALL_opts = c('--no-docs','--no-help','--no-demo'))"

# --------------------------------------------------------
# Default command
# --------------------------------------------------------
CMD ["R"]

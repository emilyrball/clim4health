# if(!"devtools" %in% installed.packages()) { 
#   install.packages("pkgdown") 
#   install.packages("devtools") 
# }


install.packages(c('renv','rstudioapi'), repos='https://cloud.r-project.org')
install.packages("devtools", dependencies = TRUE)
options(repos = c(CRAN = "https://cloud.r-project.org"))
install.packages(c("exactextractr", "sf", "stars", "s2"))

library(devtools)
if(!"renv" %in% installed.packages()) { install.packages("renv") }
library(renv)
renv::init()

package_path <- "./"
clim4health_path <- paste0(package_path)

################ temporary package installation ################
if(!"ecmwfr" %in% installed.packages()) {
  renv::install('./build/ecmwfr-master.zip')
}

if(!"PCICt" %in% installed.packages()) {
  renv::install('./build/PCICt-master.zip')
}
if(!"ClimProjDiags" %in% installed.packages()) {
  renv::install('./build/ClimProjDiags.tar.gz')
}
if(!"s2dv" %in% installed.packages()) {
  renv::install('./build/s2dv.tar.gz')
}
if(!"startR" %in% installed.packages()) {
  renv::install('./build/startR.tar.gz')
}
if(!"CSTools" %in% installed.packages()) {
  renv::install('./build/CStools.tar.gz')
}


# devtools::load_all(paste0(package_path, "csdownscale/"))
devtools::load_all(clim4health_path)

##########################################################################
##########################################################################
fcst_path <- paste0("./inst/extdata/forecast/")

fcst <- clim4health::c4h_load(path = fcst_path,
                                      var = "t2m",            # variable to load
                                      data_type = "fcst",     # data type is forecast
                                      date = c("20250101"), # forecast initialisation date
                                      ext = "nc")             # file extension

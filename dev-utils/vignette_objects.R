# Pre-compute the objects shown in the vignettes that need CDO.
#
# c4h_downscale() requires CDO, which is not available on CRAN's check
# machines. The vignettes therefore load the downscaled objects created here
# instead of calling c4h_downscale(). The vignettes still show the
# c4h_downscale() calls (eval = FALSE), and everything else (plots,
# c4h_verify(), c4h_index(), ...) is run live on the stored objects.
#
# Re-run this script (with CDO installed) whenever c4h_downscale(), the
# example data or the downscaling calls in the vignettes change, e.g.:
#   source("dev-utils/vignette_objects.R")
#
# Output: inst/extdata/vignette_objects/<vignette>_<name>.rds
# The data loading and downscaling calls below must be kept in sync with the
# vignettes.

if (!nzchar(Sys.which("cdo"))) {
  stop("CDO is required to run this script.")
}

devtools::load_all()
library(sf)

out_dir <- file.path("inst", "extdata", "vignette_objects")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Data are rounded to 1 decimal (temperature in degrees Celsius) to keep the
# files small, and therefore the package within CRAN's size limits. This has no
# visible effect on the plots.
save_obj <- function(x, vignette, name, digits = 1) {
  for (el in c("exp", "obs")) {
    if (is.numeric(x[[el]]$data)) {
      x[[el]]$data[] <- round(x[[el]]$data, digits)
    }
  }
  file <- file.path(out_dir, paste0(vignette, "_", name, ".rds"))
  saveRDS(x, file, compress = "xz")
  message("Saved ", file, " (", round(file.size(file) / 1024), " KB)")
}

ext <- function(...) {
  paste0(system.file("extdata", ..., package = "clim4health", mustWork = TRUE),
         "/")
}

fcst_path <- ext("forecast")
hcst_path <- ext("hindcast")
rean_path <- ext("reanalysis")

# ---- clim4health_downscaling ------------------------------------------------

fcst <- c4h_load(fcst_path, variable = "t2m", year = 2025, month = 1,
                 leadtime_month = "all", ext = "nc")
hcst <- c4h_load(hcst_path, variable = "t2m", year = 2010:2013, month = 1,
                 leadtime_month = "all", ext = "nc")
rean <- c4h_load(rean_path, variable = "t2m", year = 2010:2013, month = 1,
                 leadtime_month = 1:3, bbox = c(5.5, -78.5, 1.5, -74.5),
                 ext = "nc")
fcst <- c4h_convert_units(fcst, "celsius")
hcst <- c4h_convert_units(hcst, "celsius")
rean <- c4h_convert_units(rean, "celsius")

v <- "downscaling"

# Method 1: Interpolation
save_obj(
  c4h_downscale("Interpolation", exp = hcst, method_remap = "bilinear",
                target_grid = paste0(rean_path, "t2m_201001.nc")),
  v, "interpolation"
)

# Method 2: Interpolation plus bias correction
save_obj(
  c4h_downscale("Intbc", exp = hcst, obs = rean, method_bc = "evmos",
                method_remap = "bilinear"),
  v, "intbc"
)

# Method 3: Interpolation plus linear regression
save_obj(
  c4h_downscale("Intlr", exp = hcst, obs = rean, exp_cor = fcst,
                method_lr = "basic", method_remap = "con"),
  v, "intlr"
)

# Method 4: Analogs
save_obj(c4h_downscale("Analogs", exp = hcst, obs = rean), v, "analogs")

# Downscaling to point locations
point_locs <- c4h_load(ext("stations"), ext = "csv", year = 2010:2013,
                       month = 1, leadtime_month = 1:3,
                       variable = "temp_mean")
point_locs <- c4h_time(point_locs, time_aggregation = "monthly",
                       dim_aggregation = "sdate")
points <- list(
  latitude = point_locs$attrs$location$latitude,
  longitude = point_locs$attrs$location$longitude
)

save_obj(
  c4h_downscale("Interpolation", exp = hcst, points = points,
                method_point_interp = "bilinear"),
  v, "point_interpolation"
)
save_obj(
  c4h_downscale("Intbc", exp = hcst, obs = rean, points = points,
                method_point_interp = "bilinear", method_bc = "evmos"),
  v, "point_intbc"
)

# Calibration of polygon data
munip <- read_sf(system.file("extdata", "areas", "munip_vallecauca.gpkg",
                             package = "clim4health", mustWork = TRUE))
hcst_aggr <- c4h_space(hcst, munip)
rean_aggr <- c4h_space(rean, munip)
save_obj(
  c4h_downscale("Intbc", exp = hcst_aggr, obs = rean_aggr, method_bc = "bias"),
  v, "calibration"
)

# ---- clim4health_overview ---------------------------------------------------

fcst <- c4h_load(fcst_path, variable = "t2m", year = 2025, month = 1,
                 leadtime_month = "all", ext = "nc")
hcst <- c4h_load(hcst_path, variable = "t2m", year = 2010:2013, month = 1,
                 leadtime_month = 1:3, ext = "nc")
rean <- c4h_load(rean_path, variable = "t2m", year = 2010:2013, month = 1,
                 leadtime_month = 1:3, ext = "nc")
fcst <- c4h_convert_units(fcst, to = "celsius", var = "t2m", from = "K")
hcst <- c4h_convert_units(hcst, to = "celsius", var = "t2m", from = "K")
rean <- c4h_convert_units(rean, to = "celsius", var = "t2m", from = "K")

v <- "overview"

save_obj(
  c4h_downscale(downscale_function = "Intbc", obs = rean, exp = hcst,
                exp_cor = fcst, method_bc = "bias", method_remap = "bilinear"),
  v, "fcst"
)
save_obj(
  c4h_downscale(downscale_function = "Intbc", obs = rean, exp = hcst,
                method_bc = "bias", method_remap = "bilinear", loocv = FALSE),
  v, "hcst"
)

# ---- clim4health_verification -----------------------------------------------

hcst <- c4h_load(hcst_path, variable = "t2m", year = 2010:2013, month = 1,
                 leadtime_month = "all", ext = "nc")
rean <- c4h_load(rean_path, variable = "t2m", year = 2010:2013, month = 1,
                 leadtime_month = 1:3, ext = "nc")
hcst <- c4h_convert_units(hcst, to = "celsius")
rean <- c4h_convert_units(rean, to = "celsius")

save_obj(
  c4h_downscale("Intbc", exp = hcst, obs = rean, method_remap = "bilinear",
                method_bc = "evmos"),
  "verification", "intbc"
)

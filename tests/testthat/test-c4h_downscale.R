# test_that("Downscale examples run without problems", {
#   expect_no_error({
#     hcst <- c4h_load(
#       system.file("extdata", "hindcast", package = "clim4health"),
#       variable = "t2m",
#       year = 2010:2012,
#       month = 1,
#       leadtime_month = 1:3,
#       ext = "nc"
#     )
#     lats <- hcst$coords$latitude
#     lons <- hcst$coords$longitude
#     obs <- c4h_load(
#       system.file("extdata", "reanalysis", package = "clim4health"),
#       variable = "t2m",
#       year = 2010:2012,
#       month = 1,
#       leadtime_month = 1:3,
#       bbox = c(max(lats), min(lons), min(lats), max(lons)),
#       ext = "nc"
#     )
#     ### Example with gridded data
#     dwn <- c4h_downscale(
#       "Interpolation",
#       exp = hcst,
#       obs = obs,
#       method_remap = "bilinear",
#       target_grid = system.file(
#         "extdata",
#         "reanalysis/t2m_201001.nc",
#         package = "clim4health"
#       )
#     )
#     ## Example with polygon data
#     shp_path <- system.file(
#       "extdata",
#       "areas",
#       "munip_vallecauca.gpkg",
#       package = "clim4health"
#     )
#     obs_agg <- c4h_space(
#       obs,
#       areas = shp_path,
#       fun = "mean",
#       areas_id = "munip_code"
#     )
#     hcst_agg <- c4h_space(
#       hcst,
#       areas = shp_path,
#       fun = "mean",
#       areas_id = "munip_code"
#     )
#     hcst_agg_cal <- c4h_downscale(
#       downscale_function = "Intbc",
#       exp = hcst_agg,
#       obs = obs_agg,
#       bc_method = "evmos"
#     )
#     ## Example with station data
#     point_locs <- clim4health::c4h_load(
#       system.file("extdata", "stations", package = "clim4health"),
#       ext = "csv",
#       year = 2010:2012,
#       month = 1,
#       leadtime_month = 1:3,
#       variable = "temp_mean"
#     )
#     points <- list(
#       latitude = point_locs$attrs$location$latitude,
#       longitude = point_locs$attrs$location$longitude
#     )
#     point_locs <- c4h_time(
#       point_locs,
#       time_aggregation = "monthly",
#       dim_aggregation = "sdate",
#       fun = "mean"
#     )
#
#     dwn <- c4h_downscale(
#       "Interpolation",
#       exp = hcst,
#       points = points,
#       method_point_interp = "bilinear"
#     )
#   })
# })
#

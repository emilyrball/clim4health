test_that("Time examples run without problems", {
  expect_no_error({
    # Load station data
    station_path <- system.file("extdata", "stations", package = "clim4health")
    station_timeseries <- c4h_load(
      station_path,
      variable = "temp_mean",
      ext = "csv"
    )
    stat_week <- c4h_time(
      station_timeseries,
      time_aggregation = "weekly",
      dim_aggregation = "time",
      week_start = "Monday",
      min_valid = 5
    )
    dim(stat_week$data)

    # Load station data with sdate dimension
    station_data <- c4h_load(
      station_path,
      variable = "temp_mean",
      ext = "csv",
      month = 1,
      leadtime_month = 1:12
    )
    # Aggregate to monthly max across sdate dimension
    stat_month <- c4h_time(
      station_data,
      time_aggregation = "monthly",
      dim_aggregation = "sdate",
      fun = "max",
      min_valid = 20
    )
    dim(stat_month$data)
  })
})

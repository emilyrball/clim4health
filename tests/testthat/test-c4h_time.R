#' ------------------------------------------------------------------------------
# Global Health Resilience (GHR)
# Barcelona Supercomputing Center (BSC-CNS)
#
# Script Name: test-c4h_time.R
# Description: Unit test for c4h_time functions.
#
# Author(s): Rebeca Nunes, Carles Milà
# Date created: 2026-04-08
# Last updated: 2026-05-13
# Environment: local/HPC/remote (hub)
# Requirements: Please see R version and dependencies in the repo README.
#' ------------------------------------------------------------------------------

test_that("Invalid parameters fail", {
  expect_error(
    c4h_time()
  )

  # Time aggregation
  expect_error(
    c4h_time(time_aggregation = "dummy"),
    "Aggregation must be 'daily', 'weekly', 'monthly' or 'yearly'"
  )

  expect_error(
    c4h_time(time_aggregation = "weekly"),
    "'week_start' is required when 'time_aggregation' is weekly."
  )

  expect_error(
    c4h_time(time_aggregation = "weekly", week_start = "dummy"),
    "Invalid 'week_start', please check documentation."
  )

  # Fun
  expect_error(
    c4h_time(time_aggregation = "daily", fun = "dummy"),
    "Invalid choice of parameter 'fun'"
  )

  # Aggregation variable
  expect_error(
    c4h_time(time_aggregation = "daily")
  )

  expect_error(
    c4h_time(time_aggregation = "daily", dim_aggregation = "dummy"),
    "Invalid choice of parameter 'dim_aggregation'"
  )
})

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
    expect_equal(dim(station_timeseries$data)[["time"]], 1096)
    expect_equal(dim(stat_week$data)[["time"]], 158)

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
    expect_equal(dim(station_data$data)[["sdate"]], 93)
    expect_equal(dim(stat_month$data)[["sdate"]], 3)
  })

  # Apply daily aggregation
  stat_daily <- c4h_time(
    station_timeseries,
    time_aggregation = "daily",
    dim_aggregation = "time"
  )
  dim(stat_daily$data)
  expect_equal(dim(station_timeseries$data)[["time"]], 1096)
  expect_equal(dim(stat_daily$data)[["time"]], 1116)
})

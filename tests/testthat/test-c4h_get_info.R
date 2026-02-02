# Data validation

test_that("1. Datasets have correct keys and data types", {
  
  # 1. Define the Requirement Sets
  standard_keys <- c("product_type", "variable", "year", "month", "day", "time", "bbox")
  monthly_keys  <- c("product_type", "variable", "year", "month", "time", "bbox")
  seasonal_keys <- c("product_type", "variable", "year", "month", "bbox")
  
  # 2. Define the Mapping
  requirements_map <- list(
    "reanalysis-era5-land"                        = standard_keys,
    "reanalysis-era5-land-monthly-means"          = monthly_keys,
    "reanalysis-era5-single-levels"               = standard_keys,
    "reanalysis-era5-single-levels-monthly-means" = monthly_keys,
    "seasonal-monthly-single-levels"              = seasonal_keys
  )
  
  for (ds_name in names(.datasets_info)) {
    ds <- .datasets_info[[ds_name]]
    
    # --- Check 1: Key Presence ---
    required <- requirements_map[[ds_name]]
    for (key in required) {
      expect_true(key %in% names(ds), 
                  info = paste("Missing key:", key, "in", ds_name))
    }
    
    # --- Check 2: Data Types ---
    # Years should be numeric (e.g., 1950:2026)
    expect_type(ds$year, "integer") 
    
    # Variable should be a character vector
    expect_type(ds$variable, "character")
    
    # BBox must be a single string (character)
    expect_type(ds$bbox, "character")
    expect_length(ds$bbox, 1)
    
    # Month should be numeric 1:12
    expect_true(all(ds$month %in% 1:12))
  }
})

test_that("2. No fields are empty", {
  for (dataset_name in names(.datasets_info)) {
    dataset <- .datasets_info[[dataset_name]]
    
    # Ensure variables and years aren't NULL or empty vectors
    expect_gt(length(dataset$variable), 0)
    expect_gt(length(dataset$year), 0)
  }
})
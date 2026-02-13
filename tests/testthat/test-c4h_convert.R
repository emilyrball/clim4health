test_that("1. c4h_convert dispatches to .s2dv_to_terra", {
  cube <- structure(list(), class = "s2dv_cube")
  mock_terra <- function(x) "terra_result"

  with_mocked_bindings(
    .s2dv_to_terra = mock_terra,
    {
      out <- c4h_convert(cube, "terra")
      expect_equal(out, "terra_result")
    }
  )
})

test_that("2. c4h_convert dispatches to .s2dv_to_csv", {
  cube <- structure(list(), class = "s2dv_cube")
  mock_csv <- function(x) "csv_result"

  with_mocked_bindings(
    .s2dv_to_csv = mock_csv,
    {
      out <- c4h_convert(cube, "csv")
      expect_equal(out, "csv_result")
    }
  )
})

test_that("3. c4h_convert dispatches to .s2dv_to_polygons", {
  cube <- structure(list(), class = "s2dv_cube")
  mock_polygons <- function(x) "polygon_result"

  with_mocked_bindings(
    .s2dv_to_polygons = mock_polygons,
    {
      out <- c4h_convert(cube, "polygons")
      expect_equal(out, "polygon_result")
    }
  )
})

test_that("4. c4h_convert returns NULL for unsupported format", {
  cube <- structure(list(), class = "s2dv_cube")
  out <- c4h_convert(cube, "unsupported_format")

  expect_null(out)
})
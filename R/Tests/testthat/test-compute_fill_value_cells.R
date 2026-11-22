#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("compute_fill_value_cells() validates its arguments", {
  testthat::expect_error(
    compute_fill_value_cells(
      path_file = base::file.path(withr::local_tempdir(), "none.tif"),
      fill_value = 6553.5,
      lat_max = -70,
      aggregation_factor = 2
    ),
    regexp = "path_file"
  )

  path_file <-
    base::file.path(withr::local_tempdir(), "in.tif")

  build_quality_raster_fixture(
    path_file = path_file,
    mat_values = base::matrix(1, nrow = 8, ncol = 16)
  )

  testthat::expect_error(
    compute_fill_value_cells(
      path_file = path_file,
      fill_value = "6553.5",
      lat_max = -70,
      aggregation_factor = 2
    ),
    regexp = "fill_value"
  )

  testthat::expect_error(
    compute_fill_value_cells(
      path_file = path_file,
      fill_value = 6553.5,
      lat_max = NA_real_,
      aggregation_factor = 2
    ),
    regexp = "lat_max"
  )

  testthat::expect_error(
    compute_fill_value_cells(
      path_file = path_file,
      fill_value = 6553.5,
      lat_max = -70,
      aggregation_factor = 1
    ),
    regexp = "aggregation_factor"
  )
})


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("compute_fill_value_cells() maps raw to coarse cells", {
  path_file <-
    base::file.path(withr::local_tempdir(), "in.tif")

  # Fill values in the bottom row (centre 78.75 S) and in the top row,
  #   which lies north of lat_max and is not read
  build_quality_raster_fixture(
    path_file = path_file,
    mat_values = base::matrix(1000, nrow = 8, ncol = 16) |>
      base::replace(
        list = base::cbind(c(8, 8, 8, 1), c(1, 2, 9, 4)),
        values = 6553.5
      )
  )

  res <-
    compute_fill_value_cells(
      path_file = path_file,
      fill_value = 6553.5,
      lat_max = -70,
      aggregation_factor = 2
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(res, c("cell", "n_raw_cells"))
  testthat::expect_type(dplyr::pull(res, "cell"), "integer")
  # The coarse grid has 4 rows and 8 columns: raw (8, 1) and (8, 2) are in
  #   coarse (4, 1), cell 25; raw (8, 9) is in coarse (4, 5), cell 29
  testthat::expect_equal(dplyr::pull(res, "cell"), c(25L, 29L))
  testthat::expect_equal(dplyr::pull(res, "n_raw_cells"), c(2L, 1L))
})

testthat::test_that("compute_fill_value_cells() returns no row without fill", {
  path_file <-
    base::file.path(withr::local_tempdir(), "in.tif")

  build_quality_raster_fixture(
    path_file = path_file,
    mat_values = base::matrix(1000, nrow = 8, ncol = 16)
  )

  res <-
    compute_fill_value_cells(
      path_file = path_file,
      fill_value = 6553.5,
      lat_max = -70,
      aggregation_factor = 2
    )

  testthat::expect_equal(base::nrow(res), 0L)
  testthat::expect_named(res, c("cell", "n_raw_cells"))
})

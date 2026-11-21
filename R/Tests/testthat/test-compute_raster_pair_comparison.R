#----------------------------------------------------------#
# Output -----
#----------------------------------------------------------#

testthat::test_that("compute_raster_pair_comparison() returns one row a band", {
  dir_root <-
    withr::local_tempdir()

  path_file <-
    base::file.path(dir_root, "quarter.tif")

  path_reference <-
    base::file.path(dir_root, "annual.tif")

  build_quality_raster_fixture(
    path_file = path_file,
    mat_values = base::matrix(25, nrow = 36, ncol = 72)
  )

  build_quality_raster_fixture(
    path_file = path_reference,
    mat_values = base::matrix(100, nrow = 36, ncol = 72)
  )

  res <-
    compute_raster_pair_comparison(
      path_file = path_file,
      path_file_reference = path_reference,
      band_width = 10
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(
    res,
    c(
      "file_name", "file_name_reference", "band_south", "band_north",
      "n_cells", "n_both_valid", "n_exceeding", "n_missing_only",
      "n_reference_missing_only", "share_exceeding", "share_missing_only",
      "read_error"
    )
  )
  testthat::expect_equal(base::nrow(res), 18L)
  testthat::expect_equal(
    base::unique(dplyr::pull(res, "file_name_reference")),
    "annual.tif"
  )
  testthat::expect_true(base::all(dplyr::pull(res, "n_exceeding") == 0))
  testthat::expect_true(base::all(dplyr::pull(res, "n_missing_only") == 0))
  testthat::expect_true(base::all(dplyr::pull(res, "n_both_valid") == 144))
  testthat::expect_true(base::all(base::is.na(dplyr::pull(res, "read_error"))))
})


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("compute_raster_pair_comparison() counts exceedance", {
  dir_root <-
    withr::local_tempdir()

  path_file <-
    base::file.path(dir_root, "quarter.tif")

  path_reference <-
    base::file.path(dir_root, "annual.tif")

  # Row 20 (-7.5 degrees) exceeds the reference in 6 cells
  mat_quarter <-
    base::replace(
      base::matrix(25, nrow = 36, ncol = 72),
      20 + (0:5) * 36,
      500
    )

  build_quality_raster_fixture(
    path_file = path_file,
    mat_values = mat_quarter
  )

  build_quality_raster_fixture(
    path_file = path_reference,
    mat_values = base::matrix(100, nrow = 36, ncol = 72)
  )

  res <-
    compute_raster_pair_comparison(
      path_file = path_file,
      path_file_reference = path_reference,
      band_width = 10
    )

  data_band <-
    dplyr::filter(res, .data[["band_south"]] == -10)

  testthat::expect_equal(dplyr::pull(data_band, "n_exceeding"), 6)
  testthat::expect_equal(dplyr::pull(data_band, "share_exceeding"), 6 / 144)
  testthat::expect_equal(base::sum(dplyr::pull(res, "n_exceeding")), 6)
})

testthat::test_that("compute_raster_pair_comparison() counts missing cells", {
  dir_root <-
    withr::local_tempdir()

  path_file <-
    base::file.path(dir_root, "quarter.tif")

  path_reference <-
    base::file.path(dir_root, "annual.tif")

  # The file misses row 5 (67.5 degrees), the reference misses row 30
  mat_quarter <-
    base::replace(
      base::matrix(25, nrow = 36, ncol = 72),
      5 + (0:71) * 36,
      NA_real_
    )

  mat_annual <-
    base::replace(
      base::matrix(100, nrow = 36, ncol = 72),
      30 + (0:71) * 36,
      NA_real_
    )

  build_quality_raster_fixture(
    path_file = path_file,
    mat_values = mat_quarter
  )

  build_quality_raster_fixture(
    path_file = path_reference,
    mat_values = mat_annual
  )

  res <-
    compute_raster_pair_comparison(
      path_file = path_file,
      path_file_reference = path_reference,
      band_width = 10
    )

  data_north <-
    dplyr::filter(res, .data[["band_south"]] == 60)

  data_south <-
    dplyr::filter(res, .data[["band_south"]] == -60)

  testthat::expect_equal(dplyr::pull(data_north, "n_missing_only"), 72)
  testthat::expect_equal(dplyr::pull(data_north, "share_missing_only"), 0.5)
  testthat::expect_equal(
    dplyr::pull(data_south, "n_reference_missing_only"),
    72
  )
  testthat::expect_equal(dplyr::pull(data_south, "n_missing_only"), 0)
  testthat::expect_equal(dplyr::pull(data_south, "n_both_valid"), 72)
})

testthat::test_that("compute_raster_pair_comparison() allows equal values", {
  dir_root <-
    withr::local_tempdir()

  path_file <-
    base::file.path(dir_root, "float.tif")

  path_reference <-
    base::file.path(dir_root, "integer.tif")

  # As a 32-bit float 12.3 is slightly larger than 123 times 0.1
  build_quality_raster_fixture(
    path_file = path_file,
    mat_values = base::matrix(12.3, nrow = 36, ncol = 72)
  )

  build_quality_raster_fixture(
    path_file = path_reference,
    mat_values = base::matrix(12.3, nrow = 36, ncol = 72),
    datatype = "INT2U",
    scale = 0.1
  )

  res <-
    compute_raster_pair_comparison(
      path_file = path_file,
      path_file_reference = path_reference,
      band_width = 10
    )

  testthat::expect_true(base::all(dplyr::pull(res, "n_exceeding") == 0))
})


#----------------------------------------------------------#
# Failing closed -----
#----------------------------------------------------------#

testthat::test_that("compute_raster_pair_comparison() refuses other grids", {
  dir_root <-
    withr::local_tempdir()

  path_file <-
    base::file.path(dir_root, "coarse.tif")

  path_reference <-
    base::file.path(dir_root, "fine.tif")

  build_quality_raster_fixture(
    path_file = path_file,
    mat_values = base::matrix(1, nrow = 18, ncol = 36)
  )

  build_quality_raster_fixture(
    path_file = path_reference,
    mat_values = base::matrix(1, nrow = 36, ncol = 72)
  )

  res <-
    compute_raster_pair_comparison(
      path_file = path_file,
      path_file_reference = path_reference,
      band_width = 10
    )

  testthat::expect_equal(base::nrow(res), 1L)
  testthat::expect_match(dplyr::pull(res, "read_error"), "grid")
})

testthat::test_that("compute_raster_pair_comparison() reports missing files", {
  dir_root <-
    withr::local_tempdir()

  path_reference <-
    base::file.path(dir_root, "annual.tif")

  build_quality_raster_fixture(
    path_file = path_reference,
    mat_values = base::matrix(1, nrow = 36, ncol = 72)
  )

  res <-
    compute_raster_pair_comparison(
      path_file = base::file.path(dir_root, "missing.tif"),
      path_file_reference = path_reference,
      band_width = 10
    )

  testthat::expect_equal(base::nrow(res), 1L)
  testthat::expect_equal(dplyr::pull(res, "file_name"), "missing.tif")
  testthat::expect_match(dplyr::pull(res, "read_error"), "does not exist")
})


#----------------------------------------------------------#
# Input validation -----
#----------------------------------------------------------#

testthat::test_that("compute_raster_pair_comparison() validates arguments", {
  testthat::expect_error(
    compute_raster_pair_comparison(
      path_file = 1,
      path_file_reference = "b.tif"
    ),
    regexp = "path_file"
  )

  testthat::expect_error(
    compute_raster_pair_comparison(
      path_file = "a.tif",
      path_file_reference = NA_character_
    ),
    regexp = "path_file_reference"
  )

  testthat::expect_error(
    compute_raster_pair_comparison(
      path_file = "a.tif",
      path_file_reference = "b.tif",
      band_width = -1
    ),
    regexp = "band_width"
  )

  testthat::expect_error(
    compute_raster_pair_comparison(
      path_file = "a.tif",
      path_file_reference = "b.tif",
      n_rows_block = 1.5
    ),
    regexp = "n_rows_block"
  )
})

#----------------------------------------------------------#
# Output -----
#----------------------------------------------------------#

testthat::test_that("compute_tiff_block_extent() returns one row", {
  list_fixture <-
    build_tiff_block_fixture(
      path_file = base::file.path(withr::local_tempdir(), "strip.tif"),
      n_rows = 10L,
      n_cols = 4L
    )

  res <-
    compute_tiff_block_extent(
      path_file = purrr::chuck(list_fixture, "path_file")
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_equal(base::nrow(res), 1L)
  testthat::expect_named(
    res,
    c(
      "file_name", "file_size_bytes", "tiff_format", "block_layout",
      "image_width", "image_length", "rows_per_block", "n_blocks",
      "declared_end_bytes", "n_blocks_beyond_end", "first_affected_row",
      "read_error"
    )
  )
})


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("compute_tiff_block_extent() passes a complete file", {
  list_fixture <-
    build_tiff_block_fixture(
      path_file = base::file.path(withr::local_tempdir(), "strip.tif"),
      n_rows = 10L,
      n_cols = 4L,
      rows_per_block = 2L
    )

  res <-
    compute_tiff_block_extent(
      path_file = purrr::chuck(list_fixture, "path_file")
    )

  testthat::expect_equal(dplyr::pull(res, "file_name"), "strip.tif")
  testthat::expect_equal(dplyr::pull(res, "tiff_format"), "classic")
  testthat::expect_equal(dplyr::pull(res, "block_layout"), "strip")
  testthat::expect_equal(dplyr::pull(res, "image_width"), 4)
  testthat::expect_equal(dplyr::pull(res, "image_length"), 10)
  testthat::expect_equal(dplyr::pull(res, "rows_per_block"), 2)
  testthat::expect_equal(dplyr::pull(res, "n_blocks"), 5)
  testthat::expect_equal(
    dplyr::pull(res, "declared_end_bytes"),
    purrr::chuck(list_fixture, "n_bytes_total")
  )
  testthat::expect_equal(
    dplyr::pull(res, "file_size_bytes"),
    purrr::chuck(list_fixture, "n_bytes_total")
  )
  testthat::expect_equal(dplyr::pull(res, "n_blocks_beyond_end"), 0)
  testthat::expect_true(base::is.na(dplyr::pull(res, "first_affected_row")))
  testthat::expect_true(base::is.na(dplyr::pull(res, "read_error")))
})

testthat::test_that("compute_tiff_block_extent() finds a truncated file", {
  dir_root <-
    withr::local_tempdir()

  list_complete <-
    build_tiff_block_fixture(
      path_file = base::file.path(dir_root, "complete.tif"),
      n_rows = 12L,
      n_cols = 4L,
      rows_per_block = 2L
    )

  # Keep the header, three whole strips and one byte of the fourth
  n_bytes_keep <-
    purrr::chuck(list_complete, "n_bytes_header") +
    3 * purrr::chuck(list_complete, "n_bytes_block") +
    1

  list_truncated <-
    build_tiff_block_fixture(
      path_file = base::file.path(dir_root, "truncated.tif"),
      n_rows = 12L,
      n_cols = 4L,
      rows_per_block = 2L,
      n_bytes_keep = n_bytes_keep
    )

  res <-
    compute_tiff_block_extent(
      path_file = purrr::chuck(list_truncated, "path_file")
    )

  testthat::expect_equal(dplyr::pull(res, "file_size_bytes"), n_bytes_keep)
  testthat::expect_equal(
    dplyr::pull(res, "declared_end_bytes"),
    purrr::chuck(list_complete, "n_bytes_total")
  )
  testthat::expect_equal(dplyr::pull(res, "n_blocks_beyond_end"), 3)
  testthat::expect_equal(dplyr::pull(res, "first_affected_row"), 6)
  testthat::expect_true(base::is.na(dplyr::pull(res, "read_error")))
})

testthat::test_that("compute_tiff_block_extent() reads a BigTIFF", {
  dir_root <-
    withr::local_tempdir()

  list_complete <-
    build_tiff_block_fixture(
      path_file = base::file.path(dir_root, "complete.tif"),
      n_rows = 6L,
      n_cols = 5L,
      flag_bigtiff = TRUE
    )

  list_truncated <-
    build_tiff_block_fixture(
      path_file = base::file.path(dir_root, "truncated.tif"),
      n_rows = 6L,
      n_cols = 5L,
      flag_bigtiff = TRUE,
      n_bytes_keep = purrr::chuck(list_complete, "n_bytes_header") + 10
    )

  res_complete <-
    compute_tiff_block_extent(
      path_file = purrr::chuck(list_complete, "path_file")
    )

  res_truncated <-
    compute_tiff_block_extent(
      path_file = purrr::chuck(list_truncated, "path_file")
    )

  testthat::expect_equal(dplyr::pull(res_complete, "tiff_format"), "bigtiff")
  testthat::expect_equal(dplyr::pull(res_complete, "n_blocks"), 6)
  testthat::expect_equal(dplyr::pull(res_complete, "n_blocks_beyond_end"), 0)
  testthat::expect_equal(dplyr::pull(res_truncated, "n_blocks_beyond_end"), 4)
  testthat::expect_equal(dplyr::pull(res_truncated, "first_affected_row"), 2)
})

testthat::test_that("compute_tiff_block_extent() reads a tiled file", {
  dir_root <-
    withr::local_tempdir()

  list_complete <-
    build_tiff_block_fixture(
      path_file = base::file.path(dir_root, "complete.tif"),
      n_rows = 48L,
      n_cols = 32L,
      rows_per_block = 16L,
      flag_tiled = TRUE
    )

  # Two tiles per tile row: keep the first three tiles
  list_truncated <-
    build_tiff_block_fixture(
      path_file = base::file.path(dir_root, "truncated.tif"),
      n_rows = 48L,
      n_cols = 32L,
      rows_per_block = 16L,
      flag_tiled = TRUE,
      n_bytes_keep = purrr::chuck(list_complete, "n_bytes_header") +
        3 * purrr::chuck(list_complete, "n_bytes_block")
    )

  res_complete <-
    compute_tiff_block_extent(
      path_file = purrr::chuck(list_complete, "path_file")
    )

  res_truncated <-
    compute_tiff_block_extent(
      path_file = purrr::chuck(list_truncated, "path_file")
    )

  testthat::expect_equal(dplyr::pull(res_complete, "block_layout"), "tile")
  testthat::expect_equal(dplyr::pull(res_complete, "rows_per_block"), 16)
  testthat::expect_equal(dplyr::pull(res_complete, "n_blocks"), 6)
  testthat::expect_equal(dplyr::pull(res_complete, "n_blocks_beyond_end"), 0)
  testthat::expect_equal(dplyr::pull(res_truncated, "n_blocks_beyond_end"), 3)
  testthat::expect_equal(dplyr::pull(res_truncated, "first_affected_row"), 16)
})

testthat::test_that("compute_tiff_block_extent() reads a GDAL GeoTIFF", {
  path_file <-
    base::file.path(withr::local_tempdir(), "gdal.tif")

  build_quality_raster_fixture(
    path_file = path_file,
    mat_values = base::matrix(1, nrow = 6, ncol = 12)
  )

  res <-
    compute_tiff_block_extent(path_file = path_file)

  testthat::expect_true(base::is.na(dplyr::pull(res, "read_error")))
  testthat::expect_equal(dplyr::pull(res, "image_length"), 6)
  testthat::expect_equal(dplyr::pull(res, "n_blocks"), 6)
  testthat::expect_equal(dplyr::pull(res, "n_blocks_beyond_end"), 0)
})


#----------------------------------------------------------#
# Failing closed -----
#----------------------------------------------------------#

testthat::test_that("compute_tiff_block_extent() reports a missing file", {
  path_file <-
    base::file.path(withr::local_tempdir(), "missing.tif")

  res <-
    compute_tiff_block_extent(path_file = path_file)

  testthat::expect_equal(base::nrow(res), 1L)
  testthat::expect_equal(dplyr::pull(res, "file_name"), "missing.tif")
  testthat::expect_match(dplyr::pull(res, "read_error"), "does not exist")
})

testthat::test_that("compute_tiff_block_extent() reports a non-TIFF file", {
  path_file <-
    base::file.path(withr::local_tempdir(), "text.tif")

  base::writeLines("not a raster", path_file)

  res <-
    compute_tiff_block_extent(path_file = path_file)

  testthat::expect_match(dplyr::pull(res, "read_error"), "not a TIFF")
})

testthat::test_that("compute_tiff_block_extent() reports a cut header", {
  dir_root <-
    withr::local_tempdir()

  list_fixture <-
    build_tiff_block_fixture(
      path_file = base::file.path(dir_root, "cut.tif"),
      n_rows = 10L,
      n_cols = 4L,
      n_bytes_keep = 20
    )

  res <-
    compute_tiff_block_extent(
      path_file = purrr::chuck(list_fixture, "path_file")
    )

  testthat::expect_false(base::is.na(dplyr::pull(res, "read_error")))
  testthat::expect_equal(dplyr::pull(res, "file_size_bytes"), 20)
})


#----------------------------------------------------------#
# Input validation -----
#----------------------------------------------------------#

testthat::test_that("compute_tiff_block_extent() validates the path", {
  testthat::expect_error(
    compute_tiff_block_extent(path_file = 1),
    regexp = "path_file"
  )

  testthat::expect_error(
    compute_tiff_block_extent(path_file = c("a.tif", "b.tif")),
    regexp = "path_file"
  )

  testthat::expect_error(
    compute_tiff_block_extent(path_file = NA_character_),
    regexp = "path_file"
  )
})

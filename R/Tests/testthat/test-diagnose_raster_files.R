#----------------------------------------------------------#
# Fixtures -----
#----------------------------------------------------------#

data_allow_list_none <-
  tibble::tibble(
    product = base::character(0),
    variable = base::character(0),
    rule = base::character(0),
    reason = base::character(0),
    lat_min = base::numeric(0),
    lat_max = base::numeric(0)
  )

data_band_file_ok <-
  tibble::tibble(
    file_name = "bio1.tif",
    n_col = 72,
    n_row = 36,
    x_min = -180,
    x_max = 180,
    y_min = -90,
    y_max = 90,
    data_type = "INT2U",
    scale = 0.1,
    offset = 0,
    file_size_bytes = 1000,
    band_south = c(80, 70),
    band_north = c(90, 80),
    read_error = NA_character_
  )

data_expected_ok <-
  tibble::tibble(
    product = "chelsa",
    variable = "bio1",
    age = 0,
    file_name = "bio1.tif",
    expected_size_bytes = 1000,
    expected_n_col = 72,
    expected_n_row = 36,
    expected_x_min = -180,
    expected_x_max = 180,
    expected_y_min = -90,
    expected_y_max = 90
  )

data_block_ok <-
  tibble::tibble(
    file_name = "bio1.tif",
    n_blocks = 36,
    n_blocks_beyond_end = 0,
    first_affected_row = NA_real_,
    read_error = NA_character_
  )


#----------------------------------------------------------#
# Output -----
#----------------------------------------------------------#

testthat::test_that("diagnose_raster_files() passes a healthy file", {
  res <-
    diagnose_raster_files(
      data_expected_files = data_expected_ok,
      data_band_summary = data_band_file_ok,
      data_block_extent = data_block_ok,
      data_allow_list = data_allow_list_none
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_true("status" %in% base::colnames(res))
  testthat::expect_true(base::all(dplyr::pull(res, "status") == "pass"))
  testthat::expect_setequal(
    dplyr::pull(res, "rule"),
    c(
      "file_present", "file_readable", "data_type_known", "file_size",
      "block_extent", "dimensions", "extent"
    )
  )
  testthat::expect_true(base::all(base::is.na(dplyr::pull(res, "band_south"))))
  testthat::expect_equal(base::unique(dplyr::pull(res, "variable")), "bio1")
})

testthat::test_that("diagnose_raster_files() skips optional rules", {
  res <-
    diagnose_raster_files(
      data_expected_files = dplyr::mutate(
        data_expected_ok,
        expected_size_bytes = NA_real_
      ),
      data_band_summary = data_band_file_ok,
      data_block_extent = NULL,
      data_allow_list = data_allow_list_none
    )

  testthat::expect_false("file_size" %in% dplyr::pull(res, "rule"))
  testthat::expect_false("block_extent" %in% dplyr::pull(res, "rule"))
})


#----------------------------------------------------------#
# Failing closed -----
#----------------------------------------------------------#

testthat::test_that("diagnose_raster_files() fails a missing file", {
  data_expected_two <-
    dplyr::bind_rows(
      data_expected_ok,
      dplyr::mutate(data_expected_ok, variable = "bio4", file_name = "bio4.tif")
    )

  res <-
    diagnose_raster_files(
      data_expected_files = data_expected_two,
      data_band_summary = data_band_file_ok,
      data_allow_list = data_allow_list_none
    )

  data_fail <-
    dplyr::filter(res, .data[["status"]] == "fail")

  testthat::expect_equal(dplyr::pull(data_fail, "file_name"), "bio4.tif")
  testthat::expect_equal(dplyr::pull(data_fail, "rule"), "file_present")
})

testthat::test_that("diagnose_raster_files() fails an unreadable file", {
  data_band_error <-
    tibble::tibble(
      file_name = "bio1.tif",
      n_col = NA_real_,
      n_row = NA_real_,
      x_min = NA_real_,
      x_max = NA_real_,
      y_min = NA_real_,
      y_max = NA_real_,
      data_type = NA_character_,
      scale = NA_real_,
      offset = NA_real_,
      file_size_bytes = 1000,
      band_south = NA_real_,
      band_north = NA_real_,
      read_error = "Cannot open the file."
    )

  res <-
    diagnose_raster_files(
      data_expected_files = data_expected_ok,
      data_band_summary = data_band_error,
      data_allow_list = data_allow_list_none
    )

  data_fail <-
    dplyr::filter(res, .data[["status"]] == "fail")

  testthat::expect_equal(dplyr::pull(data_fail, "rule"), "file_readable")
  testthat::expect_match(dplyr::pull(data_fail, "detail"), "Cannot open")
  testthat::expect_false("dimensions" %in% dplyr::pull(res, "rule"))
})

testthat::test_that("diagnose_raster_files() fails an unknown data type", {
  res <-
    diagnose_raster_files(
      data_expected_files = data_expected_ok,
      data_band_summary = dplyr::mutate(
        data_band_file_ok,
        data_type = NA_character_
      ),
      data_allow_list = data_allow_list_none
    )

  testthat::expect_equal(
    dplyr::pull(dplyr::filter(res, .data[["status"]] == "fail"), "rule"),
    "data_type_known"
  )
})

testthat::test_that("diagnose_raster_files() fails a small file", {
  res <-
    diagnose_raster_files(
      data_expected_files = data_expected_ok,
      data_band_summary = dplyr::mutate(
        data_band_file_ok,
        file_size_bytes = 125
      ),
      data_allow_list = data_allow_list_none
    )

  data_fail <-
    dplyr::filter(res, .data[["status"]] == "fail")

  testthat::expect_equal(dplyr::pull(data_fail, "rule"), "file_size")
  testthat::expect_equal(dplyr::pull(data_fail, "value"), 875)
  testthat::expect_match(dplyr::pull(data_fail, "detail"), "125")
})

testthat::test_that("diagnose_raster_files() fails blocks beyond the end", {
  data_block_truncated <-
    dplyr::mutate(
      data_block_ok,
      n_blocks_beyond_end = 22,
      first_affected_row = 14
    )

  res <-
    diagnose_raster_files(
      data_expected_files = data_expected_ok,
      data_band_summary = data_band_file_ok,
      data_block_extent = data_block_truncated,
      data_allow_list = data_allow_list_none
    )

  data_fail <-
    dplyr::filter(res, .data[["status"]] == "fail")

  testthat::expect_equal(dplyr::pull(data_fail, "rule"), "block_extent")
  testthat::expect_equal(dplyr::pull(data_fail, "value"), 22)
  # Row 14 of 36 rows of 5 degrees starts at 20 degrees north
  testthat::expect_match(dplyr::pull(data_fail, "detail"), "row 14")
  testthat::expect_match(dplyr::pull(data_fail, "detail"), "20")
})

testthat::test_that("diagnose_raster_files() names the row of an unread cut", {
  data_band_unread <-
    dplyr::mutate(
      data_band_file_ok,
      dplyr::across(
        .cols = c("n_col", "n_row", "x_min", "x_max", "y_min", "y_max"),
        .fns = ~ NA_real_
      ),
      read_error = "Cannot read values."
    )

  res <-
    diagnose_raster_files(
      data_expected_files = data_expected_ok,
      data_band_summary = data_band_unread,
      data_block_extent = dplyr::mutate(
        data_block_ok,
        n_blocks_beyond_end = 22,
        first_affected_row = 14
      ),
      data_allow_list = data_allow_list_none
    )

  vec_detail_block <-
    res |>
    dplyr::filter(.data[["rule"]] == "block_extent") |>
    dplyr::pull("detail")

  testthat::expect_false(base::is.na(vec_detail_block))
  testthat::expect_match(vec_detail_block, "row 14")
  testthat::expect_match(vec_detail_block, "latitude is unknown")
})

testthat::test_that("diagnose_raster_files() fails a missing block row", {
  res <-
    diagnose_raster_files(
      data_expected_files = data_expected_ok,
      data_band_summary = data_band_file_ok,
      data_block_extent = dplyr::mutate(data_block_ok, file_name = "x.tif"),
      data_allow_list = data_allow_list_none
    )

  testthat::expect_equal(
    dplyr::pull(dplyr::filter(res, .data[["status"]] == "fail"), "rule"),
    "block_extent"
  )
})

testthat::test_that("diagnose_raster_files() fails wrong dimensions", {
  res <-
    diagnose_raster_files(
      data_expected_files = data_expected_ok,
      data_band_summary = dplyr::mutate(data_band_file_ok, n_row = 35),
      data_allow_list = data_allow_list_none
    )

  testthat::expect_equal(
    dplyr::pull(dplyr::filter(res, .data[["status"]] == "fail"), "rule"),
    "dimensions"
  )
})

testthat::test_that("diagnose_raster_files() fails a wrong extent", {
  res <-
    diagnose_raster_files(
      data_expected_files = data_expected_ok,
      data_band_summary = dplyr::mutate(data_band_file_ok, y_max = 84),
      data_allow_list = data_allow_list_none
    )

  data_fail <-
    dplyr::filter(res, .data[["status"]] == "fail")

  testthat::expect_equal(dplyr::pull(data_fail, "rule"), "extent")
  testthat::expect_equal(dplyr::pull(data_fail, "value"), 6)

  res_tolerated <-
    diagnose_raster_files(
      data_expected_files = data_expected_ok,
      data_band_summary = dplyr::mutate(
        data_band_file_ok,
        y_max = 90 + 1e-9
      ),
      data_allow_list = data_allow_list_none
    )

  testthat::expect_true(
    base::all(dplyr::pull(res_tolerated, "status") == "pass")
  )
})


#----------------------------------------------------------#
# Input validation -----
#----------------------------------------------------------#

testthat::test_that("diagnose_raster_files() validates the inputs", {
  testthat::expect_error(
    diagnose_raster_files(
      data_expected_files = dplyr::select(data_expected_ok, -"file_name"),
      data_band_summary = data_band_file_ok,
      data_allow_list = data_allow_list_none
    ),
    regexp = "data_expected_files"
  )

  testthat::expect_error(
    diagnose_raster_files(
      data_expected_files = dplyr::bind_rows(
        data_expected_ok,
        data_expected_ok
      ),
      data_band_summary = data_band_file_ok,
      data_allow_list = data_allow_list_none
    ),
    regexp = "data_expected_files"
  )

  testthat::expect_error(
    diagnose_raster_files(
      data_expected_files = data_expected_ok,
      data_band_summary = dplyr::select(data_band_file_ok, -"read_error"),
      data_allow_list = data_allow_list_none
    ),
    regexp = "data_band_summary"
  )

  testthat::expect_error(
    diagnose_raster_files(
      data_expected_files = data_expected_ok,
      data_band_summary = data_band_file_ok,
      data_allow_list = data_allow_list_none,
      extent_tolerance = -1
    ),
    regexp = "extent_tolerance"
  )
})

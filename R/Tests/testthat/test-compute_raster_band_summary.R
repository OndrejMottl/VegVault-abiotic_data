#----------------------------------------------------------#
# Output -----
#----------------------------------------------------------#

testthat::test_that("compute_raster_band_summary() returns one row a band", {
  path_file <-
    base::file.path(withr::local_tempdir(), "healthy.tif")

  # 36 rows of 5 degrees: 18 bands of 10 degrees, 2 rows each
  build_quality_raster_fixture(
    path_file = path_file,
    mat_values = base::outer(
      base::seq(10, 45, length.out = 36),
      base::seq(1, 2, length.out = 72),
      FUN = "*"
    )
  )

  res <-
    compute_raster_band_summary(
      path_file = path_file,
      band_width = 10,
      vec_type_limit_values = 6553.5
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(
    res,
    c(
      "file_name", "n_col", "n_row", "x_min", "x_max", "y_min", "y_max",
      "data_type", "scale", "offset", "file_size_bytes", "band_south",
      "band_north", "n_cells", "n_missing", "n_zero", "n_type_limit",
      "n_at_file_max", "n_at_file_min", "value_min", "value_max",
      "share_missing", "share_zero", "share_type_limit", "read_error"
    )
  )
  testthat::expect_equal(base::nrow(res), 18L)
  testthat::expect_equal(
    dplyr::pull(res, "band_south"),
    base::seq(80, -90, by = -10)
  )
  testthat::expect_equal(
    dplyr::pull(res, "band_north"),
    base::seq(90, -80, by = -10)
  )
  testthat::expect_true(base::all(dplyr::pull(res, "n_cells") == 144))
  testthat::expect_equal(
    base::unique(dplyr::pull(res, "file_name")),
    "healthy.tif"
  )
  testthat::expect_equal(base::unique(dplyr::pull(res, "n_col")), 72)
  testthat::expect_equal(base::unique(dplyr::pull(res, "n_row")), 36)
  testthat::expect_equal(base::unique(dplyr::pull(res, "y_max")), 90)
  testthat::expect_equal(base::unique(dplyr::pull(res, "data_type")), "FLT4S")
  testthat::expect_true(base::all(base::is.na(dplyr::pull(res, "read_error"))))
})


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("compute_raster_band_summary() counts a healthy file", {
  path_file <-
    base::file.path(withr::local_tempdir(), "healthy.tif")

  build_quality_raster_fixture(
    path_file = path_file,
    mat_values = base::outer(
      base::seq(10, 45, length.out = 36),
      base::seq(1, 2, length.out = 72),
      FUN = "*"
    )
  )

  res <-
    compute_raster_band_summary(
      path_file = path_file,
      band_width = 10,
      vec_type_limit_values = 6553.5
    )

  testthat::expect_true(base::all(dplyr::pull(res, "n_missing") == 0))
  testthat::expect_true(base::all(dplyr::pull(res, "n_zero") == 0))
  testthat::expect_true(base::all(dplyr::pull(res, "n_type_limit") == 0))
  testthat::expect_true(base::all(dplyr::pull(res, "share_zero") == 0))
  testthat::expect_equal(
    base::sum(dplyr::pull(res, "n_at_file_max")),
    1
  )
  testthat::expect_equal(
    base::max(dplyr::pull(res, "value_max")),
    90,
    tolerance = 1e-6
  )
})

testthat::test_that("compute_raster_band_summary() locates a zero cut", {
  path_file <-
    base::file.path(withr::local_tempdir(), "cut.tif")

  mat_healthy <-
    base::matrix(5, nrow = 36, ncol = 72)

  # Rows 15 to 36 lie south of 20 degrees north
  mat_cut <-
    base::rbind(
      mat_healthy[1:14, ],
      base::matrix(0, nrow = 22, ncol = 72)
    )

  build_quality_raster_fixture(
    path_file = path_file,
    mat_values = mat_cut
  )

  res <-
    compute_raster_band_summary(
      path_file = path_file,
      band_width = 10,
      vec_type_limit_values = 6553.5
    )

  data_north <-
    dplyr::filter(res, .data[["band_south"]] >= 20)

  data_south <-
    dplyr::filter(res, .data[["band_south"]] < 20)

  testthat::expect_true(base::all(dplyr::pull(data_north, "share_zero") == 0))
  testthat::expect_true(base::all(dplyr::pull(data_south, "share_zero") == 1))
  testthat::expect_true(base::all(dplyr::pull(data_south, "n_zero") == 144))
})

testthat::test_that("compute_raster_band_summary() counts type limits", {
  path_file <-
    base::file.path(withr::local_tempdir(), "capped.tif")

  mat_values <-
    base::matrix(100, nrow = 36, ncol = 72)

  # Row 3 (77.5 degrees north), columns 1 to 4
  mat_capped <-
    base::replace(mat_values, 3 + (0:3) * 36, 6553.5)

  build_quality_raster_fixture(
    path_file = path_file,
    mat_values = mat_capped
  )

  res <-
    compute_raster_band_summary(
      path_file = path_file,
      band_width = 10,
      vec_type_limit_values = c(255, 6553.5)
    )

  data_band <-
    dplyr::filter(res, .data[["band_south"]] == 70)

  testthat::expect_equal(dplyr::pull(data_band, "n_type_limit"), 4)
  testthat::expect_equal(dplyr::pull(data_band, "n_at_file_max"), 4)
  testthat::expect_equal(
    dplyr::pull(data_band, "share_type_limit"),
    4 / 144
  )
  testthat::expect_equal(base::sum(dplyr::pull(res, "n_type_limit")), 4)
  testthat::expect_equal(base::sum(dplyr::pull(res, "n_at_file_max")), 4)
})

testthat::test_that("compute_raster_band_summary() counts the file minimum", {
  path_file <-
    base::file.path(withr::local_tempdir(), "floor.tif")

  # Row 30 (-57.5 degrees), columns 1 to 5, at the offset limit
  mat_values <-
    base::replace(
      base::matrix(10, nrow = 36, ncol = 72),
      30 + (0:4) * 36,
      -273.15
    )

  build_quality_raster_fixture(
    path_file = path_file,
    mat_values = mat_values
  )

  res <-
    compute_raster_band_summary(
      path_file = path_file,
      band_width = 10,
      vec_type_limit_values = -273.15
    )

  data_band <-
    dplyr::filter(res, .data[["band_south"]] == -60)

  testthat::expect_equal(dplyr::pull(data_band, "n_at_file_min"), 5)
  testthat::expect_equal(base::sum(dplyr::pull(res, "n_at_file_min")), 5)
  testthat::expect_equal(
    dplyr::pull(data_band, "value_min"),
    -273.15,
    tolerance = 1e-6
  )
})

testthat::test_that("compute_raster_band_summary() treats types alike", {
  dir_root <-
    withr::local_tempdir()

  # Filled by row, so every raster row holds all four values
  mat_values <-
    base::matrix(
      base::rep(c(0, 12.3, 6553.5, 100), length.out = 36 * 72),
      nrow = 36,
      ncol = 72,
      byrow = TRUE
    )

  path_integer <-
    base::file.path(dir_root, "integer.tif")

  path_float <-
    base::file.path(dir_root, "float.tif")

  build_quality_raster_fixture(
    path_file = path_integer,
    mat_values = mat_values,
    datatype = "INT2U",
    scale = 0.1
  )

  build_quality_raster_fixture(
    path_file = path_float,
    mat_values = mat_values
  )

  res_integer <-
    compute_raster_band_summary(
      path_file = path_integer,
      band_width = 10,
      vec_type_limit_values = 6553.5
    )

  res_float <-
    compute_raster_band_summary(
      path_file = path_float,
      band_width = 10,
      vec_type_limit_values = 6553.5
    )

  vec_columns <-
    c("n_zero", "n_type_limit", "share_zero", "share_type_limit")

  testthat::expect_equal(
    dplyr::select(res_integer, dplyr::all_of(vec_columns)),
    dplyr::select(res_float, dplyr::all_of(vec_columns))
  )
  testthat::expect_equal(
    base::unique(dplyr::pull(res_integer, "data_type")),
    "INT2U"
  )
  testthat::expect_equal(base::unique(dplyr::pull(res_integer, "scale")), 0.1)
  testthat::expect_true(base::all(dplyr::pull(res_integer, "n_type_limit") > 0))
})

testthat::test_that("compute_raster_band_summary() counts missing bands", {
  path_file <-
    base::file.path(withr::local_tempdir(), "missing_band.tif")

  mat_values <-
    base::rbind(
      base::matrix(7, nrow = 34, ncol = 72),
      base::matrix(NA_real_, nrow = 2, ncol = 72)
    )

  build_quality_raster_fixture(
    path_file = path_file,
    mat_values = mat_values
  )

  res <-
    compute_raster_band_summary(
      path_file = path_file,
      band_width = 10,
      vec_type_limit_values = 6553.5
    )

  data_band <-
    dplyr::filter(res, .data[["band_south"]] == -90)

  testthat::expect_equal(dplyr::pull(data_band, "n_missing"), 144)
  testthat::expect_equal(dplyr::pull(data_band, "share_missing"), 1)
  testthat::expect_true(base::is.na(dplyr::pull(data_band, "share_zero")))
  testthat::expect_true(base::is.na(dplyr::pull(data_band, "value_max")))
})

testthat::test_that("compute_raster_band_summary() ignores the block size", {
  path_file <-
    base::file.path(withr::local_tempdir(), "values.tif")

  base::set.seed(900723)

  mat_values <-
    base::matrix(
      base::sample(c(0, 1, 2, 6553.5, NA), 36 * 72, replace = TRUE),
      nrow = 36,
      ncol = 72
    )

  build_quality_raster_fixture(
    path_file = path_file,
    mat_values = mat_values
  )

  list_res <-
    c(1L, 7L, 100L) |>
    purrr::map(
      .f = ~ compute_raster_band_summary(
        path_file = path_file,
        band_width = 10,
        vec_type_limit_values = 6553.5,
        n_rows_block = .x
      )
    )

  testthat::expect_equal(purrr::chuck(list_res, 1), purrr::chuck(list_res, 2))
  testthat::expect_equal(purrr::chuck(list_res, 1), purrr::chuck(list_res, 3))
})

testthat::test_that("compute_raster_band_summary() uses fixed band edges", {
  path_file <-
    base::file.path(withr::local_tempdir(), "values.tif")

  build_quality_raster_fixture(
    path_file = path_file,
    mat_values = base::matrix(1, nrow = 36, ncol = 72)
  )

  res <-
    compute_raster_band_summary(
      path_file = path_file,
      band_width = 15,
      vec_type_limit_values = 6553.5
    )

  testthat::expect_equal(
    dplyr::pull(res, "band_south"),
    base::seq(75, -90, by = -15)
  )
  testthat::expect_equal(
    base::sum(dplyr::pull(res, "n_cells")),
    36 * 72
  )
})


#----------------------------------------------------------#
# Failing closed -----
#----------------------------------------------------------#

testthat::test_that("compute_raster_band_summary() reports a missing file", {
  path_file <-
    base::file.path(withr::local_tempdir(), "missing.tif")

  res <-
    compute_raster_band_summary(
      path_file = path_file,
      band_width = 10,
      vec_type_limit_values = 6553.5
    )

  testthat::expect_equal(base::nrow(res), 1L)
  testthat::expect_equal(dplyr::pull(res, "file_name"), "missing.tif")
  testthat::expect_match(dplyr::pull(res, "read_error"), "does not exist")
  testthat::expect_true(base::is.na(dplyr::pull(res, "band_south")))
})

testthat::test_that("compute_raster_band_summary() reports a bad file", {
  path_file <-
    base::file.path(withr::local_tempdir(), "text.tif")

  base::writeLines("not a raster", path_file)

  res <-
    compute_raster_band_summary(
      path_file = path_file,
      band_width = 10,
      vec_type_limit_values = 6553.5
    )

  testthat::expect_equal(base::nrow(res), 1L)
  testthat::expect_false(base::is.na(dplyr::pull(res, "read_error")))
})

testthat::test_that("compute_raster_band_summary() refuses two layers", {
  path_file <-
    base::file.path(withr::local_tempdir(), "two_layers.tif")

  rast_one <-
    terra::rast(nrows = 4, ncols = 8, vals = 1)

  terra::writeRaster(
    x = c(rast_one, rast_one),
    filename = path_file
  )

  res <-
    compute_raster_band_summary(
      path_file = path_file,
      band_width = 10,
      vec_type_limit_values = 6553.5
    )

  testthat::expect_equal(base::nrow(res), 1L)
  testthat::expect_match(dplyr::pull(res, "read_error"), "layers")
})


#----------------------------------------------------------#
# Input validation -----
#----------------------------------------------------------#

testthat::test_that("compute_raster_band_summary() validates arguments", {
  testthat::expect_error(
    compute_raster_band_summary(
      path_file = 1,
      vec_type_limit_values = 6553.5
    ),
    regexp = "path_file"
  )

  testthat::expect_error(
    compute_raster_band_summary(
      path_file = "a.tif",
      band_width = 0,
      vec_type_limit_values = 6553.5
    ),
    regexp = "band_width"
  )

  testthat::expect_error(
    compute_raster_band_summary(
      path_file = "a.tif",
      vec_type_limit_values = "6553.5"
    ),
    regexp = "vec_type_limit_values"
  )

  testthat::expect_error(
    compute_raster_band_summary(
      path_file = "a.tif",
      vec_type_limit_values = 6553.5,
      n_rows_block = 0
    ),
    regexp = "n_rows_block"
  )

  testthat::expect_error(
    compute_raster_band_summary(
      path_file = "a.tif",
      vec_type_limit_values = 6553.5,
      value_tolerance = -1
    ),
    regexp = "value_tolerance"
  )
})

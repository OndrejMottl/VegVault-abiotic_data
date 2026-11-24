#----------------------------------------------------------#
# Fixtures -----
#----------------------------------------------------------#

# Band statistics of one file: 18 bands of 10 degrees from north to south,
#   values from 1 to 50, one cell at each extreme
data_bands_template <-
  tibble::tibble(
    band_south = base::seq(80, -90, by = -10),
    band_north = base::seq(90, -80, by = -10),
    n_cells = 144,
    n_missing = 0,
    n_zero = 0,
    n_type_limit = 0,
    n_at_file_max = c(1, base::rep(0, 17)),
    n_at_file_min = c(base::rep(0, 17), 1),
    value_min = c(base::rep(2, 17), 1),
    value_max = c(50, base::rep(40, 17)),
    share_zero = 0,
    read_error = NA_character_
  )

vec_limits_example <-
  c(-273.15, 25.5, 6280.35, 6553.5)

data_allow_list_none <-
  tibble::tibble(
    product = base::character(0),
    variable = base::character(0),
    rule = base::character(0),
    reason = base::character(0),
    lat_min = base::numeric(0),
    lat_max = base::numeric(0)
  )

data_thresholds_neo <-
  tidyr::expand_grid(
    product = "chelsa",
    variable = c("bio4", "bio12"),
    rule = c("zero_share_max", "zero_share_step")
  ) |>
  dplyr::mutate(
    threshold = dplyr::if_else(.data[["rule"]] == "zero_share_max", 0.5, 0.3)
  )

data_expected_neo <-
  tibble::tibble(
    product = "chelsa",
    variable = c("bio4", "bio12"),
    age = 0,
    file_name = c("bio4.tif", "bio12.tif")
  )


#----------------------------------------------------------#
# Output -----
#----------------------------------------------------------#

testthat::test_that("diagnose_raster_bands() passes healthy rasters", {
  data_bands <-
    dplyr::bind_rows(
      dplyr::mutate(data_bands_template, file_name = "bio4.tif"),
      dplyr::mutate(data_bands_template, file_name = "bio12.tif")
    )

  res <-
    diagnose_raster_bands(
      data_expected_files = data_expected_neo,
      data_band_summary = data_bands,
      data_thresholds = data_thresholds_neo,
      data_allow_list = data_allow_list_none,
      vec_type_limit_values = vec_limits_example
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_true(base::all(dplyr::pull(res, "status") == "pass"))
  testthat::expect_setequal(
    dplyr::pull(res, "rule"),
    c("type_limit", "zero_share_max", "zero_share_step", "band_all_missing")
  )
  testthat::expect_false(
    "zero_share_age_deviation" %in% dplyr::pull(res, "rule")
  )
})


#----------------------------------------------------------#
# Zero rules -----
#----------------------------------------------------------#

testthat::test_that("diagnose_raster_bands() fails a zero cut", {
  # Every cell is 0 south of 60 degrees north, as in the truncated bio4
  data_bands_cut <-
    data_bands_template |>
    dplyr::mutate(
      file_name = "bio4.tif",
      share_zero = dplyr::if_else(.data[["band_north"]] <= 60, 1, 0),
      n_zero = .data[["share_zero"]] * 144
    )

  data_bands <-
    dplyr::bind_rows(
      data_bands_cut,
      dplyr::mutate(data_bands_template, file_name = "bio12.tif")
    )

  res <-
    diagnose_raster_bands(
      data_expected_files = data_expected_neo,
      data_band_summary = data_bands,
      data_thresholds = data_thresholds_neo,
      data_allow_list = data_allow_list_none,
      vec_type_limit_values = vec_limits_example
    )

  data_step <-
    dplyr::filter(
      res,
      .data[["status"]] == "fail",
      .data[["rule"]] == "zero_share_step"
    )

  # The share jumps in the band from 50 to 60 degrees
  testthat::expect_equal(base::nrow(data_step), 1L)
  testthat::expect_equal(dplyr::pull(data_step, "band_south"), 50)
  testthat::expect_equal(dplyr::pull(data_step, "file_name"), "bio4.tif")
  testthat::expect_equal(dplyr::pull(data_step, "value"), 1)

  testthat::expect_equal(
    base::nrow(
      dplyr::filter(
        res,
        .data[["status"]] == "fail",
        .data[["rule"]] == "zero_share_max"
      )
    ),
    15L
  )
  testthat::expect_false(
    "bio12.tif" %in%
      dplyr::pull(dplyr::filter(res, .data[["status"]] == "fail"), "file_name")
  )
})

testthat::test_that("diagnose_raster_bands() passes a desert", {
  # A quarter of three bands is legitimately zero
  data_bands_desert <-
    data_bands_template |>
    dplyr::mutate(
      file_name = "bio12.tif",
      share_zero = dplyr::if_else(
        .data[["band_south"]] %in% c(10, 20, 30),
        0.25,
        0
      ),
      n_zero = .data[["share_zero"]] * 144
    )

  data_bands <-
    dplyr::bind_rows(
      dplyr::mutate(data_bands_template, file_name = "bio4.tif"),
      data_bands_desert
    )

  res <-
    diagnose_raster_bands(
      data_expected_files = data_expected_neo,
      data_band_summary = data_bands,
      data_thresholds = data_thresholds_neo,
      data_allow_list = data_allow_list_none,
      vec_type_limit_values = vec_limits_example
    )

  testthat::expect_true(base::all(dplyr::pull(res, "status") == "pass"))
})

testthat::test_that("diagnose_raster_bands() compares ages", {
  data_expected_ages <-
    tibble::tibble(
      product = "trace21k",
      variable = "bio15",
      age = c(0, 500, 1000, 1500),
      file_name = c("a0.tif", "a500.tif", "a1000.tif", "a1500.tif")
    )

  # Only age 0 has zeros, in the two southernmost bands
  data_bands <-
    data_expected_ages |>
    dplyr::select("file_name") |>
    dplyr::cross_join(data_bands_template) |>
    dplyr::mutate(
      share_zero = dplyr::if_else(
        .data[["file_name"]] == "a0.tif" & .data[["band_south"]] < -70,
        0.4,
        0
      )
    )

  data_thresholds <-
    tidyr::expand_grid(
      product = "trace21k",
      variable = "bio15",
      rule = c("zero_share_max", "zero_share_step", "zero_share_age_deviation")
    ) |>
    dplyr::mutate(threshold = c(0.5, 0.5, 0.2))

  res <-
    diagnose_raster_bands(
      data_expected_files = data_expected_ages,
      data_band_summary = data_bands,
      data_thresholds = data_thresholds,
      data_allow_list = data_allow_list_none,
      vec_type_limit_values = vec_limits_example
    )

  data_fail <-
    dplyr::filter(res, .data[["status"]] == "fail")

  testthat::expect_equal(
    base::unique(dplyr::pull(data_fail, "rule")),
    "zero_share_age_deviation"
  )
  testthat::expect_equal(
    dplyr::pull(data_fail, "file_name"),
    c("a0.tif", "a0.tif")
  )
  testthat::expect_equal(dplyr::pull(data_fail, "band_south"), c(-80, -90))
  testthat::expect_equal(dplyr::pull(data_fail, "value"), c(0.4, 0.4))
})


#----------------------------------------------------------#
# Type limits -----
#----------------------------------------------------------#

testthat::test_that("diagnose_raster_bands() fails a capped maximum", {
  # The file maximum is the 16-bit limit 6553.5, held by 3 cells at 20 N
  data_bands <-
    dplyr::bind_rows(
      dplyr::mutate(data_bands_template, file_name = "bio4.tif"),
      dplyr::mutate(
        data_bands_template,
        file_name = "bio12.tif",
        value_max = dplyr::if_else(.data[["band_south"]] == 20, 6553.5, 40),
        n_at_file_max = dplyr::if_else(.data[["band_south"]] == 20, 3, 0),
        n_type_limit = dplyr::if_else(.data[["band_south"]] == 20, 3, 0)
      )
    )

  res <-
    diagnose_raster_bands(
      data_expected_files = data_expected_neo,
      data_band_summary = data_bands,
      data_thresholds = data_thresholds_neo,
      data_allow_list = data_allow_list_none,
      vec_type_limit_values = vec_limits_example
    )

  data_fail <-
    dplyr::filter(res, .data[["status"]] == "fail")

  testthat::expect_equal(dplyr::pull(data_fail, "rule"), "type_limit")
  testthat::expect_equal(dplyr::pull(data_fail, "variable"), "bio12")
  testthat::expect_equal(dplyr::pull(data_fail, "band_south"), 20)

  data_allow_list <-
    tibble::tibble(
      product = "chelsa",
      variable = "bio12",
      rule = "type_limit",
      reason = "CHELSA stores bio12 as 16-bit integers (cap 6553.5).",
      lat_min = NA_real_,
      lat_max = NA_real_
    )

  res_allowed <-
    diagnose_raster_bands(
      data_expected_files = data_expected_neo,
      data_band_summary = data_bands,
      data_thresholds = data_thresholds_neo,
      data_allow_list = data_allow_list,
      vec_type_limit_values = vec_limits_example
    )

  testthat::expect_false("fail" %in% dplyr::pull(res_allowed, "status"))
  testthat::expect_equal(
    base::sum(dplyr::pull(res_allowed, "status") == "allowed"),
    1L
  )
})

testthat::test_that("diagnose_raster_bands() fails a capped minimum", {
  # The file minimum is the offset limit -273.15, held by 2 cells
  data_bands <-
    dplyr::bind_rows(
      dplyr::mutate(
        data_bands_template,
        file_name = "bio4.tif",
        value_min = dplyr::if_else(.data[["band_south"]] == -90, -273.15, 2),
        n_at_file_min = dplyr::if_else(.data[["band_south"]] == -90, 2, 0)
      ),
      dplyr::mutate(data_bands_template, file_name = "bio12.tif")
    )

  res <-
    diagnose_raster_bands(
      data_expected_files = data_expected_neo,
      data_band_summary = data_bands,
      data_thresholds = data_thresholds_neo,
      data_allow_list = data_allow_list_none,
      vec_type_limit_values = vec_limits_example
    )

  data_fail <-
    dplyr::filter(res, .data[["status"]] == "fail")

  testthat::expect_equal(dplyr::pull(data_fail, "rule"), "type_limit")
  testthat::expect_equal(dplyr::pull(data_fail, "file_name"), "bio4.tif")
  testthat::expect_equal(dplyr::pull(data_fail, "value"), 2)
  testthat::expect_match(dplyr::pull(data_fail, "detail"), "273.15")
})

testthat::test_that("diagnose_raster_bands() passes ordinary limit values", {
  # 25.5 is a type-limit value but not the file maximum: an ordinary value
  data_bands <-
    dplyr::bind_rows(
      dplyr::mutate(data_bands_template, file_name = "bio4.tif"),
      dplyr::mutate(
        data_bands_template,
        file_name = "bio12.tif",
        n_type_limit = 40
      )
    )

  res <-
    diagnose_raster_bands(
      data_expected_files = data_expected_neo,
      data_band_summary = data_bands,
      data_thresholds = data_thresholds_neo,
      data_allow_list = data_allow_list_none,
      vec_type_limit_values = vec_limits_example
    )

  testthat::expect_true(base::all(dplyr::pull(res, "status") == "pass"))
})


#----------------------------------------------------------#
# Missing bands -----
#----------------------------------------------------------#

testthat::test_that("diagnose_raster_bands() fails a band without data", {
  data_bands <-
    dplyr::bind_rows(
      dplyr::mutate(data_bands_template, file_name = "bio12.tif"),
      dplyr::mutate(
        data_bands_template,
        file_name = "bio4.tif",
        n_missing = dplyr::if_else(.data[["band_south"]] == 0, 144, 0),
        share_zero = dplyr::if_else(
          .data[["band_south"]] == 0,
          NA_real_,
          0
        )
      )
    )

  res <-
    diagnose_raster_bands(
      data_expected_files = data_expected_neo,
      data_band_summary = data_bands,
      data_thresholds = data_thresholds_neo,
      data_allow_list = data_allow_list_none,
      vec_type_limit_values = vec_limits_example
    )

  data_fail <-
    dplyr::filter(res, .data[["status"]] == "fail")

  testthat::expect_equal(dplyr::pull(data_fail, "rule"), "band_all_missing")
  testthat::expect_equal(dplyr::pull(data_fail, "file_name"), "bio4.tif")
  testthat::expect_equal(dplyr::pull(data_fail, "band_south"), 0)
  testthat::expect_match(dplyr::pull(data_fail, "detail"), "bio12")
})

testthat::test_that("diagnose_raster_bands() passes a band missing in all", {
  data_bands <-
    dplyr::bind_rows(
      dplyr::mutate(data_bands_template, file_name = "bio4.tif"),
      dplyr::mutate(data_bands_template, file_name = "bio12.tif")
    ) |>
    dplyr::mutate(
      n_missing = dplyr::if_else(.data[["band_south"]] == -90, 144, 0),
      share_zero = dplyr::if_else(
        .data[["band_south"]] == -90,
        NA_real_,
        0
      )
    )

  res <-
    diagnose_raster_bands(
      data_expected_files = data_expected_neo,
      data_band_summary = data_bands,
      data_thresholds = data_thresholds_neo,
      data_allow_list = data_allow_list_none,
      vec_type_limit_values = vec_limits_example
    )

  testthat::expect_true(base::all(dplyr::pull(res, "status") == "pass"))
})


#----------------------------------------------------------#
# Failing closed -----
#----------------------------------------------------------#

testthat::test_that("diagnose_raster_bands() fails a missing threshold", {
  data_bands <-
    dplyr::bind_rows(
      dplyr::mutate(data_bands_template, file_name = "bio4.tif"),
      dplyr::mutate(data_bands_template, file_name = "bio12.tif")
    )

  res <-
    diagnose_raster_bands(
      data_expected_files = data_expected_neo,
      data_band_summary = data_bands,
      data_thresholds = dplyr::filter(
        data_thresholds_neo,
        !(.data[["variable"]] == "bio12" &
          .data[["rule"]] == "zero_share_step")
      ),
      data_allow_list = data_allow_list_none,
      vec_type_limit_values = vec_limits_example
    )

  data_fail <-
    dplyr::filter(res, .data[["status"]] == "fail")

  testthat::expect_equal(base::nrow(data_fail), 1L)
  testthat::expect_equal(dplyr::pull(data_fail, "variable"), "bio12")
  testthat::expect_equal(dplyr::pull(data_fail, "rule"), "zero_share_step")
})

testthat::test_that("diagnose_raster_bands() ignores unreadable files", {
  data_bands <-
    dplyr::bind_rows(
      dplyr::mutate(data_bands_template, file_name = "bio12.tif"),
      tibble::tibble(
        file_name = "bio4.tif",
        band_south = NA_real_,
        band_north = NA_real_,
        read_error = "Cannot open the file."
      )
    )

  res <-
    diagnose_raster_bands(
      data_expected_files = data_expected_neo,
      data_band_summary = data_bands,
      data_thresholds = data_thresholds_neo,
      data_allow_list = data_allow_list_none,
      vec_type_limit_values = vec_limits_example
    )

  testthat::expect_false("bio4.tif" %in% dplyr::pull(res, "file_name"))
})


#----------------------------------------------------------#
# Input validation -----
#----------------------------------------------------------#

testthat::test_that("diagnose_raster_bands() validates the inputs", {
  testthat::expect_error(
    diagnose_raster_bands(
      data_expected_files = dplyr::select(data_expected_neo, -"age"),
      data_band_summary = data_bands_template,
      data_thresholds = data_thresholds_neo,
      data_allow_list = data_allow_list_none,
      vec_type_limit_values = vec_limits_example
    ),
    regexp = "data_expected_files"
  )

  testthat::expect_error(
    diagnose_raster_bands(
      data_expected_files = data_expected_neo,
      data_band_summary = dplyr::select(data_bands_template, -"share_zero"),
      data_thresholds = data_thresholds_neo,
      data_allow_list = data_allow_list_none,
      vec_type_limit_values = vec_limits_example
    ),
    regexp = "data_band_summary"
  )

  testthat::expect_error(
    diagnose_raster_bands(
      data_expected_files = data_expected_neo,
      data_band_summary = dplyr::mutate(
        data_bands_template,
        file_name = "bio4.tif"
      ),
      data_thresholds = data_thresholds_neo,
      data_allow_list = data_allow_list_none,
      vec_type_limit_values = "6553.5"
    ),
    regexp = "vec_type_limit_values"
  )
})

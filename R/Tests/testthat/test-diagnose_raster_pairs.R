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

data_expected_pair <-
  tibble::tibble(
    product = "chelsa",
    variable = "bio18",
    age = 0,
    file_name = "bio18.tif",
    file_name_reference = "bio12.tif"
  )

data_pair_ok <-
  tibble::tibble(
    file_name = "bio18.tif",
    file_name_reference = "bio12.tif",
    band_south = c(10, 0),
    band_north = c(20, 10),
    n_cells = 144,
    n_both_valid = 144,
    n_exceeding = 0,
    n_missing_only = 0,
    n_reference_missing_only = 0,
    share_exceeding = 0,
    share_missing_only = 0,
    read_error = NA_character_
  )

data_thresholds_pair <-
  tibble::tibble(
    product = "chelsa",
    variable = "bio18",
    rule = c("pair_exceeding", "pair_missing_only"),
    threshold = c(0.001, 0)
  )


#----------------------------------------------------------#
# Output -----
#----------------------------------------------------------#

testthat::test_that("diagnose_raster_pairs() passes a consistent pair", {
  res <-
    diagnose_raster_pairs(
      data_expected_pairs = data_expected_pair,
      data_pair_comparison = data_pair_ok,
      data_thresholds = data_thresholds_pair,
      data_allow_list = data_allow_list_none
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_true(base::all(dplyr::pull(res, "status") == "pass"))
  testthat::expect_setequal(
    dplyr::pull(res, "rule"),
    c("pair_present", "pair_readable", "pair_exceeding", "pair_missing_only")
  )
  testthat::expect_true(
    base::all(stringr::str_detect(dplyr::pull(res, "detail"), "bio12.tif"))
  )
})


#----------------------------------------------------------#
# Rules -----
#----------------------------------------------------------#

testthat::test_that("diagnose_raster_pairs() fails exceeding cells", {
  data_pair_exceeding <-
    dplyr::mutate(
      data_pair_ok,
      n_exceeding = c(0, 3),
      share_exceeding = c(0, 3 / 144)
    )

  res <-
    diagnose_raster_pairs(
      data_expected_pairs = data_expected_pair,
      data_pair_comparison = data_pair_exceeding,
      data_thresholds = data_thresholds_pair,
      data_allow_list = data_allow_list_none
    )

  data_fail <-
    dplyr::filter(res, .data[["status"]] == "fail")

  testthat::expect_equal(dplyr::pull(data_fail, "rule"), "pair_exceeding")
  testthat::expect_equal(dplyr::pull(data_fail, "band_south"), 0)
  testthat::expect_equal(dplyr::pull(data_fail, "value"), 3 / 144)
})

testthat::test_that("diagnose_raster_pairs() fails cells missing alone", {
  data_pair_missing <-
    dplyr::mutate(
      data_pair_ok,
      n_missing_only = c(2, 0),
      share_missing_only = c(2 / 144, 0)
    )

  res <-
    diagnose_raster_pairs(
      data_expected_pairs = data_expected_pair,
      data_pair_comparison = data_pair_missing,
      data_thresholds = data_thresholds_pair,
      data_allow_list = data_allow_list_none
    )

  data_fail <-
    dplyr::filter(res, .data[["status"]] == "fail")

  testthat::expect_equal(dplyr::pull(data_fail, "rule"), "pair_missing_only")
  testthat::expect_equal(dplyr::pull(data_fail, "band_south"), 10)
})


#----------------------------------------------------------#
# Failing closed -----
#----------------------------------------------------------#

testthat::test_that("diagnose_raster_pairs() fails a missing comparison", {
  res <-
    diagnose_raster_pairs(
      data_expected_pairs = data_expected_pair,
      data_pair_comparison = data_pair_ok[0, ],
      data_thresholds = data_thresholds_pair,
      data_allow_list = data_allow_list_none
    )

  testthat::expect_equal(
    dplyr::pull(dplyr::filter(res, .data[["status"]] == "fail"), "rule"),
    "pair_present"
  )
})

testthat::test_that("diagnose_raster_pairs() fails an unreadable pair", {
  data_pair_error <-
    tibble::tibble(
      file_name = "bio18.tif",
      file_name_reference = "bio12.tif",
      band_south = NA_real_,
      band_north = NA_real_,
      share_exceeding = NA_real_,
      share_missing_only = NA_real_,
      read_error = "The two rasters are not on the same grid."
    )

  res <-
    diagnose_raster_pairs(
      data_expected_pairs = data_expected_pair,
      data_pair_comparison = data_pair_error,
      data_thresholds = data_thresholds_pair,
      data_allow_list = data_allow_list_none
    )

  data_fail <-
    dplyr::filter(res, .data[["status"]] == "fail")

  testthat::expect_equal(dplyr::pull(data_fail, "rule"), "pair_readable")
  testthat::expect_match(dplyr::pull(data_fail, "detail"), "same grid")
})

testthat::test_that("diagnose_raster_pairs() fails a missing threshold", {
  res <-
    diagnose_raster_pairs(
      data_expected_pairs = data_expected_pair,
      data_pair_comparison = data_pair_ok,
      data_thresholds = data_thresholds_pair[1, ],
      data_allow_list = data_allow_list_none
    )

  testthat::expect_equal(
    dplyr::pull(dplyr::filter(res, .data[["status"]] == "fail"), "rule"),
    "pair_missing_only"
  )
})


#----------------------------------------------------------#
# Input validation -----
#----------------------------------------------------------#

testthat::test_that("diagnose_raster_pairs() validates the inputs", {
  testthat::expect_error(
    diagnose_raster_pairs(
      data_expected_pairs = dplyr::select(
        data_expected_pair,
        -"file_name_reference"
      ),
      data_pair_comparison = data_pair_ok,
      data_thresholds = data_thresholds_pair,
      data_allow_list = data_allow_list_none
    ),
    regexp = "data_expected_pairs"
  )

  testthat::expect_error(
    diagnose_raster_pairs(
      data_expected_pairs = dplyr::bind_rows(
        data_expected_pair,
        data_expected_pair
      ),
      data_pair_comparison = data_pair_ok,
      data_thresholds = data_thresholds_pair,
      data_allow_list = data_allow_list_none
    ),
    regexp = "data_expected_pairs"
  )

  testthat::expect_error(
    diagnose_raster_pairs(
      data_expected_pairs = data_expected_pair,
      data_pair_comparison = dplyr::select(data_pair_ok, -"share_exceeding"),
      data_thresholds = data_thresholds_pair,
      data_allow_list = data_allow_list_none
    ),
    regexp = "data_pair_comparison"
  )
})

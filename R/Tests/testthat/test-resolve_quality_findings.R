#----------------------------------------------------------#
# Fixtures -----
#----------------------------------------------------------#

data_evaluations_example <-
  tibble::tibble(
    product = "chelsa",
    variable = c("bio4", "bio4", "bio12", "bio12"),
    age = 0,
    file_name = c("bio4.tif", "bio4.tif", "bio12.tif", "bio12.tif"),
    band_south = c(60, 50, 60, NA),
    band_north = c(70, 60, 70, NA),
    rule = c("zero_share_max", "zero_share_max", "type_limit", "file_size"),
    value = c(0.1, 1, 12, 0),
    threshold = c(NA, NA, 0, 0),
    detail = c("a", "b", "c", "d")
  )

data_thresholds_example <-
  tibble::tibble(
    product = "chelsa",
    variable = "bio4",
    rule = "zero_share_max",
    threshold = 0.5
  )

data_allow_list_empty <-
  tibble::tibble(
    product = base::character(0),
    variable = base::character(0),
    rule = base::character(0),
    reason = base::character(0),
    lat_min = base::numeric(0),
    lat_max = base::numeric(0)
  )


#----------------------------------------------------------#
# Output -----
#----------------------------------------------------------#

testthat::test_that("resolve_quality_findings() adds a status", {
  res <-
    resolve_quality_findings(
      data_evaluations = data_evaluations_example,
      data_thresholds = data_thresholds_example,
      data_allow_list = data_allow_list_empty
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(
    res,
    c(
      "product", "variable", "age", "file_name", "band_south",
      "band_north", "rule", "value", "threshold", "detail", "status"
    )
  )
  testthat::expect_equal(base::nrow(res), 4L)
  testthat::expect_equal(
    dplyr::pull(res, "status"),
    c("pass", "fail", "fail", "pass")
  )
  testthat::expect_equal(
    dplyr::pull(res, "threshold"),
    c(0.5, 0.5, 0, 0)
  )
})

testthat::test_that("resolve_quality_findings() accepts no evaluations", {
  res <-
    resolve_quality_findings(
      data_evaluations = data_evaluations_example[0, ],
      data_thresholds = data_thresholds_example,
      data_allow_list = data_allow_list_empty
    )

  testthat::expect_equal(base::nrow(res), 0L)
  testthat::expect_true("status" %in% base::colnames(res))
})


#----------------------------------------------------------#
# Failing closed -----
#----------------------------------------------------------#

testthat::test_that("resolve_quality_findings() fails a missing threshold", {
  data_thresholds_none <-
    data_thresholds_example[0, ]

  res <-
    resolve_quality_findings(
      data_evaluations = data_evaluations_example,
      data_thresholds = data_thresholds_none,
      data_allow_list = data_allow_list_empty
    )

  data_missing <-
    dplyr::filter(res, .data[["rule"]] == "zero_share_max")

  # Collapsed to one finding for the file and rule
  testthat::expect_equal(base::nrow(data_missing), 1L)
  testthat::expect_equal(dplyr::pull(data_missing, "status"), "fail")
  testthat::expect_true(base::is.na(dplyr::pull(data_missing, "band_south")))
  testthat::expect_match(dplyr::pull(data_missing, "detail"), "threshold")
})

testthat::test_that("resolve_quality_findings() fails a missing value", {
  data_evaluations_na <-
    dplyr::mutate(
      data_evaluations_example,
      value = c(NA, 0.1, 0, 0)
    )

  res <-
    resolve_quality_findings(
      data_evaluations = data_evaluations_na,
      data_thresholds = data_thresholds_example,
      data_allow_list = data_allow_list_empty
    )

  testthat::expect_equal(dplyr::pull(res, "status")[1], "fail")
  testthat::expect_match(dplyr::pull(res, "detail")[1], "evaluated")
})


#----------------------------------------------------------#
# Allow-list -----
#----------------------------------------------------------#

testthat::test_that("resolve_quality_findings() allows a listed failure", {
  data_allow_list <-
    tibble::tibble(
      product = "chelsa",
      variable = "bio12",
      rule = "type_limit",
      reason = "The source stores bio12 as 16-bit integers.",
      lat_min = NA_real_,
      lat_max = NA_real_
    )

  res <-
    resolve_quality_findings(
      data_evaluations = data_evaluations_example,
      data_thresholds = data_thresholds_example,
      data_allow_list = data_allow_list
    )

  data_allowed <-
    dplyr::filter(res, .data[["rule"]] == "type_limit")

  testthat::expect_equal(dplyr::pull(data_allowed, "status"), "allowed")
  testthat::expect_match(
    dplyr::pull(data_allowed, "detail"),
    "16-bit integers"
  )
  testthat::expect_equal(
    dplyr::pull(dplyr::filter(res, .data[["variable"]] == "bio4"), "status"),
    c("pass", "fail")
  )
})

testthat::test_that("resolve_quality_findings() limits allowed latitudes", {
  data_allow_list <-
    tibble::tibble(
      product = "chelsa",
      variable = "bio4",
      rule = c("zero_share_max", "file_size"),
      reason = "Accepted.",
      lat_min = c(-90, -90),
      lat_max = c(40, 40)
    )

  data_evaluations_failing <-
    dplyr::mutate(
      data_evaluations_example,
      value = c(1, 1, 0, 5),
      variable = c("bio4", "bio4", "bio12", "bio4")
    )

  res <-
    resolve_quality_findings(
      data_evaluations = data_evaluations_failing,
      data_thresholds = data_thresholds_example,
      data_allow_list = data_allow_list
    )

  # Bands at 50 to 70 degrees lie outside the entry; a whole-file finding
  #   never matches an entry with latitude limits
  testthat::expect_equal(
    dplyr::pull(res, "status"),
    c("fail", "fail", "pass", "fail")
  )

  data_allow_list_wide <-
    dplyr::mutate(data_allow_list, lat_max = 90)

  res_wide <-
    resolve_quality_findings(
      data_evaluations = data_evaluations_failing,
      data_thresholds = data_thresholds_example,
      data_allow_list = data_allow_list_wide
    )

  testthat::expect_equal(
    dplyr::pull(res_wide, "status"),
    c("allowed", "allowed", "pass", "fail")
  )
})

testthat::test_that("resolve_quality_findings() never allows no threshold", {
  data_allow_list <-
    tibble::tibble(
      product = "chelsa",
      variable = "bio4",
      rule = "zero_share_max",
      reason = "Accepted.",
      lat_min = NA_real_,
      lat_max = NA_real_
    )

  res <-
    resolve_quality_findings(
      data_evaluations = data_evaluations_example,
      data_thresholds = data_thresholds_example[0, ],
      data_allow_list = data_allow_list
    )

  testthat::expect_equal(
    dplyr::pull(
      dplyr::filter(res, .data[["rule"]] == "zero_share_max"),
      "status"
    ),
    "fail"
  )
})


#----------------------------------------------------------#
# Input validation -----
#----------------------------------------------------------#

testthat::test_that("resolve_quality_findings() validates the inputs", {
  testthat::expect_error(
    resolve_quality_findings(
      data_evaluations = dplyr::select(data_evaluations_example, -"rule"),
      data_thresholds = data_thresholds_example,
      data_allow_list = data_allow_list_empty
    ),
    regexp = "data_evaluations"
  )

  testthat::expect_error(
    resolve_quality_findings(
      data_evaluations = data_evaluations_example,
      data_thresholds = dplyr::bind_rows(
        data_thresholds_example,
        data_thresholds_example
      ),
      data_allow_list = data_allow_list_empty
    ),
    regexp = "data_thresholds"
  )

  testthat::expect_error(
    resolve_quality_findings(
      data_evaluations = data_evaluations_example,
      data_thresholds = dplyr::mutate(
        data_thresholds_example,
        threshold = NA_real_
      ),
      data_allow_list = data_allow_list_empty
    ),
    regexp = "data_thresholds"
  )

  testthat::expect_error(
    resolve_quality_findings(
      data_evaluations = data_evaluations_example,
      data_thresholds = data_thresholds_example,
      data_allow_list = tibble::tibble(
        product = "chelsa",
        variable = "bio12",
        rule = "type_limit",
        reason = "",
        lat_min = NA_real_,
        lat_max = NA_real_
      )
    ),
    regexp = "reason"
  )
})

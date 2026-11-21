#----------------------------------------------------------#
# Fixtures -----
#----------------------------------------------------------#

data_expected_three <-
  tibble::tibble(
    product = "chelsa",
    variable = c("bio1", "bio4", "bio12"),
    age = 0,
    file_name = c("bio1.tif", "bio4.tif", "bio12.tif")
  )

data_findings_example <-
  tibble::tibble(
    product = "chelsa",
    variable = c("bio1", "bio4", "bio4", "bio4", "bio12", "bio12"),
    age = 0,
    file_name = c(
      "bio1.tif", "bio4.tif", "bio4.tif", "bio4.tif", "bio12.tif",
      "bio12.tif"
    ),
    band_south = c(NA, 50, 40, NA, 20, NA),
    band_north = c(NA, 60, 50, NA, 30, NA),
    rule = c(
      "file_present", "zero_share_step", "zero_share_max", "file_size",
      "type_limit", "file_present"
    ),
    value = c(0, 1, 1, 0, 3, 0),
    threshold = c(0, 0.3, 0.5, 0, 0, 0),
    detail = "",
    status = c("pass", "fail", "fail", "pass", "allowed", "pass")
  )


#----------------------------------------------------------#
# Output -----
#----------------------------------------------------------#

testthat::test_that("summarise_raster_quality() returns one row a file", {
  res <-
    summarise_raster_quality(
      data_expected_files = data_expected_three,
      data_findings = data_findings_example
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(
    res,
    c(
      "product", "variable", "age", "file_name", "status", "n_failures",
      "failing_rules", "allowed_rules"
    )
  )
  testthat::expect_equal(
    dplyr::pull(res, "file_name"),
    c("bio1.tif", "bio4.tif", "bio12.tif")
  )
})

testthat::test_that("summarise_raster_quality() sets the status", {
  res <-
    summarise_raster_quality(
      data_expected_files = data_expected_three,
      data_findings = data_findings_example
    )

  testthat::expect_equal(
    dplyr::pull(res, "status"),
    c("pass", "fail", "pass")
  )
  testthat::expect_equal(dplyr::pull(res, "n_failures"), c(0, 2, 0))
  testthat::expect_equal(
    dplyr::pull(res, "failing_rules"),
    c(NA, "zero_share_max, zero_share_step", NA)
  )
  testthat::expect_equal(
    dplyr::pull(res, "allowed_rules"),
    c(NA, NA, "type_limit")
  )
})


#----------------------------------------------------------#
# Failing closed -----
#----------------------------------------------------------#

testthat::test_that("summarise_raster_quality() fails an unchecked file", {
  data_expected_four <-
    dplyr::bind_rows(
      data_expected_three,
      tibble::tibble(
        product = "chelsa",
        variable = "bio6",
        age = 0,
        file_name = "bio6.tif"
      )
    )

  res <-
    summarise_raster_quality(
      data_expected_files = data_expected_four,
      data_findings = data_findings_example
    )

  data_unchecked <-
    dplyr::filter(res, .data[["file_name"]] == "bio6.tif")

  testthat::expect_equal(dplyr::pull(data_unchecked, "status"), "fail")
  testthat::expect_equal(
    dplyr::pull(data_unchecked, "failing_rules"),
    "not_checked"
  )
})


#----------------------------------------------------------#
# Input validation -----
#----------------------------------------------------------#

testthat::test_that("summarise_raster_quality() validates the inputs", {
  testthat::expect_error(
    summarise_raster_quality(
      data_expected_files = dplyr::select(data_expected_three, -"file_name"),
      data_findings = data_findings_example
    ),
    regexp = "data_expected_files"
  )

  testthat::expect_error(
    summarise_raster_quality(
      data_expected_files = data_expected_three,
      data_findings = dplyr::select(data_findings_example, -"status")
    ),
    regexp = "data_findings"
  )
})

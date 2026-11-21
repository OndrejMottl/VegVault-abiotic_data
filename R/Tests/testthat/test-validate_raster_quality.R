#----------------------------------------------------------#
# Fixtures -----
#----------------------------------------------------------#

data_findings_mixed <-
  tibble::tibble(
    product = "chelsa",
    variable = c("bio4", "bio4", "bio12"),
    age = 0,
    file_name = c("bio4.tif", "bio4.tif", "bio12.tif"),
    band_south = c(60, 50, 20),
    band_north = c(70, 60, 30),
    rule = c("zero_share_max", "zero_share_step", "type_limit"),
    value = c(0, 1, 3),
    threshold = c(0.5, 0.3, 0),
    detail = c("", "Zero share changes from 0 to 1.", "Allowed: cap."),
    status = c("pass", "fail", "allowed")
  )


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("validate_raster_quality() passes clean findings", {
  data_findings_clean <-
    dplyr::filter(data_findings_mixed, .data[["status"]] != "fail")

  res <-
    validate_raster_quality(data_findings = data_findings_clean)

  testthat::expect_equal(res, data_findings_clean)
  testthat::expect_invisible(
    validate_raster_quality(data_findings = data_findings_clean)
  )
})

testthat::test_that("validate_raster_quality() names the failures", {
  res_error <-
    testthat::expect_error(
      validate_raster_quality(data_findings = data_findings_mixed),
      regexp = "1 failing"
    )

  vec_message <-
    base::conditionMessage(res_error)

  testthat::expect_match(vec_message, "bio4.tif")
  testthat::expect_match(vec_message, "zero_share_step")
  testthat::expect_match(vec_message, "50")
  testthat::expect_false(stringr::str_detect(vec_message, "bio12.tif"))
})

testthat::test_that("validate_raster_quality() counts unlisted failures", {
  data_findings_many <-
    tibble::tibble(
      product = "chelsa",
      variable = "bio4",
      age = 0,
      file_name = "bio4.tif",
      band_south = base::seq(80, -90, by = -10),
      band_north = base::seq(90, -80, by = -10),
      rule = "zero_share_max",
      value = 1,
      threshold = 0.5,
      detail = "",
      status = "fail"
    )

  testthat::expect_error(
    validate_raster_quality(
      data_findings = data_findings_many,
      n_show = 5
    ),
    regexp = "13 more"
  )
})


#----------------------------------------------------------#
# Failing closed -----
#----------------------------------------------------------#

testthat::test_that("validate_raster_quality() fails empty findings", {
  testthat::expect_error(
    validate_raster_quality(data_findings = data_findings_mixed[0, ]),
    regexp = "No finding"
  )
})


#----------------------------------------------------------#
# Input validation -----
#----------------------------------------------------------#

testthat::test_that("validate_raster_quality() validates the inputs", {
  testthat::expect_error(
    validate_raster_quality(
      data_findings = dplyr::select(data_findings_mixed, -"status")
    ),
    regexp = "data_findings"
  )

  testthat::expect_error(
    validate_raster_quality(
      data_findings = data_findings_mixed,
      n_show = 0
    ),
    regexp = "n_show"
  )
})

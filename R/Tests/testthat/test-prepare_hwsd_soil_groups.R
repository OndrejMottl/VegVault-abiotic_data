#----------------------------------------------------------#
# Fixtures -----
#----------------------------------------------------------#

data_units_valid <-
  tibble::tibble(
    HWSD2_SMU_ID = c(5L, 5L, 2L, 2L, 3L, 4L, 6L),
    SEQUENCE = c(1L, 2L, 2L, 1L, 1L, 1L, 1L),
    SHARE = c(70, 30, 40, 60, 100, 100, 100),
    WRB2 = c("LV", "AC", "LV", "AC", "WR", "LK", NA)
  )

list_soil_valid <-
  base::list(AC = 1L, LV = 19L)

list_non_soil_valid <-
  base::list(GG = 33L, WR = 34L, IS = 35L, ND = 36L)


#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("prepare_hwsd_soil_groups() validates its arguments", {
  testthat::expect_error(
    prepare_hwsd_soil_groups(
      data_soil_units = dplyr::select(data_units_valid, -"WRB2"),
      list_soil_group_codes = list_soil_valid,
      list_non_soil_codes = list_non_soil_valid,
      code_unlisted = 37L
    ),
    regexp = "data_soil_units"
  )

  testthat::expect_error(
    prepare_hwsd_soil_groups(
      data_soil_units = data_units_valid,
      list_soil_group_codes = base::list(AC = 1L, LV = 1L),
      list_non_soil_codes = list_non_soil_valid,
      code_unlisted = 37L
    ),
    regexp = "codes"
  )

  testthat::expect_error(
    prepare_hwsd_soil_groups(
      data_soil_units = data_units_valid,
      list_soil_group_codes = list_soil_valid,
      list_non_soil_codes = base::list(WR = 19L),
      code_unlisted = 37L
    ),
    regexp = "codes"
  )

  testthat::expect_error(
    prepare_hwsd_soil_groups(
      data_soil_units = data_units_valid,
      list_soil_group_codes = list_soil_valid,
      list_non_soil_codes = list_non_soil_valid,
      code_unlisted = 30L
    ),
    regexp = "code_unlisted"
  )
})


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("prepare_hwsd_soil_groups() classes the dominant soil", {
  res <-
    prepare_hwsd_soil_groups(
      data_soil_units = data_units_valid,
      list_soil_group_codes = list_soil_valid,
      list_non_soil_codes = list_non_soil_valid,
      code_unlisted = 37L
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(
    res,
    c("HWSD2_SMU_ID", "WRB2", "class_code", "class_type")
  )
  testthat::expect_equal(dplyr::pull(res, "HWSD2_SMU_ID"), 2:6)
  testthat::expect_equal(
    dplyr::pull(res, "WRB2"),
    c("AC", "WR", "LK", "LV", NA)
  )
  testthat::expect_equal(
    dplyr::pull(res, "class_code"),
    c(1L, 34L, 37L, 19L, 37L)
  )
  testthat::expect_equal(
    dplyr::pull(res, "class_type"),
    c("soil", "non_soil", "unlisted", "soil", "unlisted")
  )
})


#----------------------------------------------------------#
# Failing closed -----
#----------------------------------------------------------#

testthat::test_that("prepare_hwsd_soil_groups() needs a first component", {
  testthat::expect_error(
    prepare_hwsd_soil_groups(
      data_soil_units = dplyr::filter(
        data_units_valid,
        !(.data[["HWSD2_SMU_ID"]] == 2L & .data[["SEQUENCE"]] == 1L)
      ),
      list_soil_group_codes = list_soil_valid,
      list_non_soil_codes = list_non_soil_valid,
      code_unlisted = 37L
    ),
    regexp = "SEQUENCE"
  )
})

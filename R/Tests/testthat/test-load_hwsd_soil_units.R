#----------------------------------------------------------#
# Fixtures -----
#----------------------------------------------------------#

data_smu_valid <-
  tibble::tibble(
    HWSD2_SMU_ID = c(2, 2, 3),
    SEQUENCE = c(1, 2, 1),
    SHARE = c(60, 40, 100),
    WRB2 = c("AC", "LV", "WR"),
    WRB2_CODE = c(1, 18, 33),
    WRB4 = c("ACha", "LVha", "WR")
  )


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("load_hwsd_soil_units() reads the soil-unit table", {
  dir_root <-
    withr::local_tempdir()

  vec_paths <-
    base::file.path(dir_root, c("d_wrb2.csv", "hwsd2_smu.csv"))

  readr::write_csv(x = tibble::tibble(CODE = "AC"), file = vec_paths[1])

  readr::write_csv(x = data_smu_valid, file = vec_paths[2])

  res <-
    load_hwsd_soil_units(vec_paths = vec_paths)

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(res, c("HWSD2_SMU_ID", "SEQUENCE", "SHARE", "WRB2"))
  testthat::expect_type(dplyr::pull(res, "HWSD2_SMU_ID"), "integer")
  testthat::expect_type(dplyr::pull(res, "SEQUENCE"), "integer")
  testthat::expect_type(dplyr::pull(res, "SHARE"), "double")
  testthat::expect_equal(dplyr::pull(res, "WRB2"), c("AC", "LV", "WR"))
})


#----------------------------------------------------------#
# Failing closed -----
#----------------------------------------------------------#

testthat::test_that("load_hwsd_soil_units() needs the soil-unit file", {
  testthat::expect_error(
    load_hwsd_soil_units(
      vec_paths = base::file.path(withr::local_tempdir(), "d_wrb2.csv")
    ),
    regexp = "hwsd2_smu.csv"
  )
})

testthat::test_that("load_hwsd_soil_units() checks columns and duplicates", {
  dir_root <-
    withr::local_tempdir()

  path_smu <-
    base::file.path(dir_root, "hwsd2_smu.csv")

  readr::write_csv(
    x = dplyr::select(data_smu_valid, -"SEQUENCE"),
    file = path_smu
  )

  testthat::expect_error(
    load_hwsd_soil_units(vec_paths = path_smu),
    regexp = "SEQUENCE"
  )

  readr::write_csv(
    x = dplyr::bind_rows(data_smu_valid, dplyr::slice(data_smu_valid, 1)),
    file = path_smu
  )

  testthat::expect_error(
    load_hwsd_soil_units(vec_paths = path_smu),
    regexp = "twice"
  )
})

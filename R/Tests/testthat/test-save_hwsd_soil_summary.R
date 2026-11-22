#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("save_hwsd_soil_summary() counts cells by class", {
  dir_root <-
    withr::local_tempdir()

  path_class_raster <-
    base::file.path(dir_root, "classes.tif")

  build_quality_raster_fixture(
    path_file = path_class_raster,
    mat_values = base::rbind(c(1, 1, 1, 19), c(34, 37, NA, NA))
  )

  data_groups <-
    tibble::tibble(
      HWSD2_SMU_ID = 2:6,
      WRB2 = c("AC", "WR", "LK", "LV", "FK"),
      class_code = c(1L, 34L, 37L, 19L, 37L),
      class_type = c("soil", "non_soil", "unlisted", "soil", "unlisted")
    )

  path_output <-
    base::file.path(dir_root, "Tables", "summary.csv")

  res <-
    save_hwsd_soil_summary(
      path_class_raster = path_class_raster,
      data_soil_groups = data_groups,
      path_output = path_output
    )

  testthat::expect_equal(res, path_output)

  data_summary <-
    readr::read_csv(file = path_output, show_col_types = FALSE)

  testthat::expect_named(
    data_summary,
    c(
      "class_code", "class_type", "wrb_codes", "n_soil_units",
      "n_processed_cells"
    )
  )
  testthat::expect_equal(
    dplyr::pull(data_summary, "class_code"),
    c(1, 19, 34, 37, NA)
  )
  testthat::expect_equal(
    dplyr::pull(data_summary, "class_type"),
    c("soil", "soil", "non_soil", "unlisted", "missing")
  )
  testthat::expect_equal(
    dplyr::pull(data_summary, "wrb_codes"),
    c("AC", "LV", "WR", "FK;LK", NA)
  )
  testthat::expect_equal(
    dplyr::pull(data_summary, "n_soil_units"),
    c(1, 1, 1, 2, NA)
  )
  testthat::expect_equal(
    dplyr::pull(data_summary, "n_processed_cells"),
    c(3, 1, 1, 1, 2)
  )
})

testthat::test_that("save_hwsd_soil_summary() validates its arguments", {
  testthat::expect_error(
    save_hwsd_soil_summary(
      path_class_raster = base::file.path(withr::local_tempdir(), "none.tif"),
      data_soil_groups = tibble::tibble(class_code = 1L),
      path_output = "x.csv"
    ),
    regexp = "path_class_raster"
  )
})

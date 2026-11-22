#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("build_hwsd_processing_targets() checks the config", {
  list_fixture <-
    build_processing_pipeline_fixture(
      dir_root = withr::local_tempdir(),
      flag_upstream = FALSE
    )

  list_processing <-
    list_fixture[["list_fixture"]][["list_data_processing"]]

  testthat::expect_error(
    build_hwsd_processing_targets(
      list_data_processing = purrr::assign_in(
        list_processing,
        base::list("hwsd", "code_unlisted"),
        5L
      ),
      path_data_storage = list_fixture[["path_data_storage"]],
      dir_tables = list_fixture[["dir_tables"]]
    ),
    regexp = "code_unlisted"
  )

  testthat::expect_error(
    build_hwsd_processing_targets(
      list_data_processing = purrr::assign_in(
        list_processing,
        base::list("hwsd", "template_variable"),
        NULL
      ),
      path_data_storage = list_fixture[["path_data_storage"]],
      dir_tables = list_fixture[["dir_tables"]]
    ),
    regexp = "template_variable"
  )
})


#----------------------------------------------------------#
# Outputs -----
#----------------------------------------------------------#

testthat::test_that("build_hwsd_processing_targets() names the targets", {
  list_fixture <-
    build_processing_pipeline_fixture(
      dir_root = withr::local_tempdir(),
      flag_upstream = FALSE
    )

  vec_names <-
    build_hwsd_processing_targets(
      list_data_processing = list_fixture[["list_fixture"]][[
        "list_data_processing"
      ]],
      path_data_storage = list_fixture[["path_data_storage"]],
      dir_tables = list_fixture[["dir_tables"]]
    ) |>
    purrr::map_chr(.f = ~ .x[["settings"]][["name"]])

  testthat::expect_setequal(
    vec_names,
    c(
      "fingerprint_hwsd_acquisition", "hwsd_extracted_files",
      "hwsd_class_tables", "fingerprint_hwsd_template",
      "hwsd_template_raw_files", "hwsd_soil_units", "hwsd_soil_groups",
      "hwsd_class_raster", "hwsd_class_raster_resampled",
      "hwsd_class_raster_aggregated", "hwsd_soil_summary",
      "hwsd_soil_raster"
    )
  )
})


#----------------------------------------------------------#
# Pipeline runs -----
#----------------------------------------------------------#

testthat::test_that("the processing stage builds the soil-group raster", {
  list_fixture <-
    build_processing_pipeline_fixture(dir_root = withr::local_tempdir())

  run_target_pipeline(
    script = list_fixture[["path_script"]],
    store = list_fixture[["path_store"]],
    verbose = FALSE
  )

  path_soil <-
    targets::tar_read_raw(
      name = "hwsd_soil_raster",
      store = list_fixture[["path_store"]]
    )

  rast_soil <-
    terra::rast(path_soil)

  testthat::expect_equal(base::dim(rast_soil)[1:2], c(8, 18))
  testthat::expect_equal(
    base::unname(base::as.vector(terra::ext(rast_soil))),
    c(-180, 180, -90, 70)
  )

  mat_soil <-
    terra::as.matrix(rast_soil, wide = TRUE)

  # No data in the first column; open water and LK are not soils; the
  #   Luvisols block keeps its code; a cell with one lake cell stays soil
  testthat::expect_true(base::all(base::is.na(mat_soil[, 1])))
  testthat::expect_true(base::all(base::is.na(mat_soil[1, c(2, 3)])))
  testthat::expect_true(base::all(base::is.na(mat_soil[2, c(4, 5)])))
  testthat::expect_equal(mat_soil[4, c(6, 7)], c(19, 19))
  testthat::expect_equal(mat_soil[6, 8], 1)
  testthat::expect_equal(base::sum(mat_soil == 1, na.rm = TRUE), 130)

  data_summary <-
    readr::read_csv(
      file = targets::tar_read_raw(
        name = "hwsd_soil_summary",
        store = list_fixture[["path_store"]]
      ),
      show_col_types = FALSE
    )

  testthat::expect_equal(
    dplyr::pull(data_summary, "class_code"),
    c(1, 19, 34, 37, NA)
  )
  testthat::expect_equal(
    dplyr::pull(data_summary, "n_processed_cells"),
    c(130, 2, 2, 2, 8)
  )
})

#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("build_hwsd_extraction_targets() checks the config", {
  list_fixture <-
    build_extraction_pipeline_fixture(
      dir_root = withr::local_tempdir(),
      flag_upstream = FALSE
    )

  list_extraction <-
    list_fixture[["list_fixture"]][["list_data_extraction"]]

  testthat::expect_error(
    build_hwsd_extraction_targets(
      list_data_extraction = purrr::assign_in(list_extraction, "hwsd", NULL),
      path_data_storage = list_fixture[["path_data_storage"]]
    ),
    regexp = "hwsd"
  )

  testthat::expect_error(
    build_hwsd_extraction_targets(
      list_data_extraction = purrr::assign_in(
        list_extraction,
        base::list("hwsd", "product"),
        NULL
      ),
      path_data_storage = list_fixture[["path_data_storage"]]
    ),
    regexp = "product"
  )

  testthat::expect_error(
    build_hwsd_extraction_targets(
      list_data_extraction = purrr::assign_in(
        list_extraction,
        base::list("hwsd", "columns"),
        c("long", "lat")
      ),
      path_data_storage = list_fixture[["path_data_storage"]]
    ),
    regexp = "columns"
  )

  testthat::expect_error(
    build_hwsd_extraction_targets(
      list_data_extraction = purrr::assign_in(
        list_extraction,
        base::list("hwsd", "var_names"),
        base::list(other = "HWSD2")
      ),
      path_data_storage = list_fixture[["path_data_storage"]]
    ),
    regexp = "var_names"
  )
})


#----------------------------------------------------------#
# Outputs -----
#----------------------------------------------------------#

testthat::test_that("build_hwsd_extraction_targets() names the targets", {
  list_fixture <-
    build_extraction_pipeline_fixture(
      dir_root = withr::local_tempdir(),
      flag_upstream = FALSE
    )

  vec_names <-
    build_hwsd_extraction_targets(
      list_data_extraction = list_fixture[["list_fixture"]][[
        "list_data_extraction"
      ]],
      path_data_storage = list_fixture[["path_data_storage"]]
    ) |>
    purrr::map_chr(.f = ~ .x[["settings"]][["name"]])

  testthat::expect_setequal(
    vec_names,
    c(
      "fingerprint_hwsd_soil_raster", "hwsd_soil_raster",
      "hwsd_extraction_files", "hwsd_processed_raster", "hwsd_cell_table",
      "hwsd_extracted_files"
    )
  )
})


#----------------------------------------------------------#
# Pipeline runs -----
#----------------------------------------------------------#

testthat::test_that("the extraction stage saves the soil cell table", {
  list_fixture <-
    build_extraction_pipeline_fixture(dir_root = withr::local_tempdir())

  run_target_pipeline(
    script = list_fixture[["path_script"]],
    store = list_fixture[["path_store"]],
    verbose = FALSE
  )

  data_hwsd <-
    targets::tar_read_raw(
      name = "hwsd_extracted_files",
      store = list_fixture[["path_store"]]
    )

  testthat::expect_equal(base::nrow(data_hwsd), 1L)
  testthat::expect_equal(dplyr::pull(data_hwsd, "product"), "prod_hwsd")
  testthat::expect_equal(dplyr::pull(data_hwsd, "variable"), "soil_group")
  testthat::expect_equal(dplyr::pull(data_hwsd, "age"), 0)
  testthat::expect_equal(dplyr::pull(data_hwsd, "time_id"), NA_integer_)
  testthat::expect_equal(
    dplyr::pull(data_hwsd, "path_file"),
    base::file.path(
      list_fixture[["path_data_storage"]],
      "Processed/Hwsd/Cell_tables/hwsd_soil_groups.qs2"
    )
  )

  data_soil <-
    qs2::qs_read(file = dplyr::pull(data_hwsd, "path_file"))

  testthat::expect_named(data_soil, c("long", "lat", "value", "var_name"))
  testthat::expect_type(dplyr::pull(data_soil, "value"), "double")
  # Cells 3 and 5 are missing and get no row
  testthat::expect_equal(
    dplyr::pull(data_soil, "value"),
    c(1, 19, 1, 19, 19, 1)
  )
  testthat::expect_equal(
    dplyr::pull(data_soil, "var_name"),
    base::rep("HWSD2", 6)
  )
})

#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("build_climate_processing_targets() checks the config", {
  list_fixture <-
    build_processing_pipeline_fixture(
      dir_root = withr::local_tempdir(),
      flag_upstream = FALSE
    )

  list_processing <-
    list_fixture[["list_fixture"]][["list_data_processing"]]

  testthat::expect_error(
    build_climate_processing_targets(
      sel_product = "none",
      list_data_processing = list_processing,
      path_data_storage = list_fixture[["path_data_storage"]],
      dir_tables = list_fixture[["dir_tables"]]
    ),
    regexp = "sel_product"
  )

  testthat::expect_error(
    build_climate_processing_targets(
      sel_product = "neo",
      list_data_processing = purrr::assign_in(
        list_processing,
        base::list("climate_products", "neo", "aggregation_fun"),
        "mean"
      ),
      path_data_storage = list_fixture[["path_data_storage"]],
      dir_tables = list_fixture[["dir_tables"]]
    ),
    regexp = "aggregation_fun"
  )

  testthat::expect_error(
    build_climate_processing_targets(
      sel_product = "neo",
      list_data_processing = purrr::assign_in(
        list_processing,
        "aggregation_factor",
        1
      ),
      path_data_storage = list_fixture[["path_data_storage"]],
      dir_tables = list_fixture[["dir_tables"]]
    ),
    regexp = "aggregation_factor"
  )

  testthat::expect_error(
    build_climate_processing_targets(
      sel_product = "palaeo",
      list_data_processing = purrr::assign_in(
        list_processing,
        base::list("climate_products", "palaeo", "artefact_mask", "ratio_min"),
        0
      ),
      path_data_storage = list_fixture[["path_data_storage"]],
      dir_tables = list_fixture[["dir_tables"]]
    ),
    regexp = "ratio_min"
  )
})


#----------------------------------------------------------#
# Outputs -----
#----------------------------------------------------------#

testthat::test_that("build_climate_processing_targets() names the targets", {
  list_fixture <-
    build_processing_pipeline_fixture(
      dir_root = withr::local_tempdir(),
      flag_upstream = FALSE
    )

  list_processing <-
    list_fixture[["list_fixture"]][["list_data_processing"]]

  vec_names_neo <-
    build_climate_processing_targets(
      sel_product = "neo",
      list_data_processing = list_processing,
      path_data_storage = list_fixture[["path_data_storage"]],
      dir_tables = list_fixture[["dir_tables"]]
    ) |>
    purrr::map_chr(.f = ~ .x[["settings"]][["name"]])

  testthat::expect_setequal(
    vec_names_neo,
    c(
      "fingerprint_neo_raw_files", "neo_raw_files",
      "neo_processing_files", "neo_aggregated", "neo_fill_files",
      "neo_fill_cells", "neo_mask_cells", "neo_final",
      "neo_masked_cell_counts", "neo_processed_files"
    )
  )

  vec_names_palaeo <-
    build_climate_processing_targets(
      sel_product = "palaeo",
      list_data_processing = list_processing,
      path_data_storage = list_fixture[["path_data_storage"]],
      dir_tables = list_fixture[["dir_tables"]]
    ) |>
    purrr::map_chr(.f = ~ .x[["settings"]][["name"]])

  testthat::expect_setequal(
    vec_names_palaeo,
    c(
      "fingerprint_palaeo_raw_files", "palaeo_raw_files",
      "palaeo_processing_files", "palaeo_aggregated",
      "palaeo_artefact_cells", "palaeo_mask_cells", "palaeo_final",
      "palaeo_masked_cell_counts", "palaeo_processed_files"
    )
  )
})


#----------------------------------------------------------#
# Pipeline runs -----
#----------------------------------------------------------#

testthat::test_that("the processing stage stops without the download store", {
  list_fixture <-
    build_processing_pipeline_fixture(
      dir_root = withr::local_tempdir(),
      flag_upstream = FALSE
    )

  testthat::expect_error(
    run_target_pipeline(
      script = list_fixture[["path_script"]],
      store = list_fixture[["path_store"]],
      verbose = FALSE
    )
  )

  data_meta <-
    targets::tar_meta(
      fields = "error",
      store = list_fixture[["path_store"]]
    ) |>
    dplyr::filter(!base::is.na(.data[["error"]]))

  testthat::expect_gt(base::nrow(data_meta), 0L)
  testthat::expect_true(
    base::any(
      stringr::str_detect(
        dplyr::pull(data_meta, "error"),
        "01_Data_acquisition/00_run.R"
      )
    )
  )
})

testthat::test_that("the processing stage aggregates and masks the climate", {
  list_fixture <-
    build_processing_pipeline_fixture(dir_root = withr::local_tempdir())

  run_target_pipeline(
    script = list_fixture[["path_script"]],
    store = list_fixture[["path_store"]],
    verbose = FALSE
  )

  data_neo <-
    targets::tar_read_raw(
      name = "neo_processed_files",
      store = list_fixture[["path_store"]]
    )

  testthat::expect_equal(
    dplyr::pull(data_neo, "file_name"),
    c("neo_bio1.tif", "neo_bio12.tif")
  )
  testthat::expect_true(
    base::all(base::file.exists(dplyr::pull(data_neo, "path_file")))
  )

  vec_neo_bio12 <-
    terra::values(
      terra::rast(dplyr::pull(data_neo, "path_file")[2]),
      mat = FALSE
    )

  testthat::expect_length(vec_neo_bio12, 32L)
  # The fill-value cells 25 and 29 are missing, every other cell is 1000
  testthat::expect_true(base::all(base::is.na(vec_neo_bio12[c(25, 29)])))
  testthat::expect_true(base::all(vec_neo_bio12[-c(25, 29)] == 1000))

  data_palaeo <-
    targets::tar_read_raw(
      name = "palaeo_processed_files",
      store = list_fixture[["path_store"]]
    )

  vec_bio12_500 <-
    data_palaeo |>
    dplyr::filter(
      .data[["variable"]] == "bio12",
      .data[["age"]] == 500
    ) |>
    dplyr::pull("path_file") |>
    terra::rast() |>
    terra::values(mat = FALSE)

  vec_bio15_500 <-
    data_palaeo |>
    dplyr::filter(
      .data[["variable"]] == "bio15",
      .data[["age"]] == 500
    ) |>
    dplyr::pull("path_file") |>
    terra::rast() |>
    terra::values(mat = FALSE)

  vec_bio12_1000 <-
    data_palaeo |>
    dplyr::filter(
      .data[["variable"]] == "bio12",
      .data[["age"]] == 1000
    ) |>
    dplyr::pull("path_file") |>
    terra::rast() |>
    terra::values(mat = FALSE)

  testthat::expect_true(base::all(base::is.na(vec_bio12_500[c(11, 12)])))
  testthat::expect_equal(vec_bio12_500[15], 600)
  testthat::expect_true(base::all(base::is.na(vec_bio15_500[c(11, 12)])))
  testthat::expect_true(base::all(vec_bio15_500[-c(11, 12)] == 50))
  testthat::expect_equal(vec_bio12_1000[19], 600)

  data_counts_neo <-
    readr::read_csv(
      file = targets::tar_read_raw(
        name = "neo_masked_cell_counts",
        store = list_fixture[["path_store"]]
      ),
      show_col_types = FALSE
    )

  testthat::expect_equal(dplyr::pull(data_counts_neo, "n_cells"), 2)
  testthat::expect_equal(dplyr::pull(data_counts_neo, "n_cells_with_value"), 2)

  data_counts_palaeo <-
    readr::read_csv(
      file = targets::tar_read_raw(
        name = "palaeo_masked_cell_counts",
        store = list_fixture[["path_store"]]
      ),
      show_col_types = FALSE
    )

  testthat::expect_equal(
    dplyr::pull(data_counts_palaeo, "variable"),
    c("bio12", "bio15")
  )
  testthat::expect_equal(dplyr::pull(data_counts_palaeo, "n_cells"), c(2, 2))
  testthat::expect_true(
    base::all(dplyr::pull(data_counts_palaeo, "mask") == "artefact")
  )
})

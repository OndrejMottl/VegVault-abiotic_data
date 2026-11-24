#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("build_climate_extraction_targets() checks the config", {
  list_fixture <-
    build_extraction_pipeline_fixture(
      dir_root = withr::local_tempdir(),
      flag_upstream = FALSE
    )

  list_extraction <-
    list_fixture[["list_fixture"]][["list_data_extraction"]]

  testthat::expect_error(
    build_climate_extraction_targets(
      sel_product = "none",
      list_data_extraction = list_extraction,
      path_data_storage = list_fixture[["path_data_storage"]]
    ),
    regexp = "sel_product"
  )

  testthat::expect_error(
    build_climate_extraction_targets(
      sel_product = "neo",
      list_data_extraction = purrr::assign_in(
        list_extraction,
        base::list("climate_products", "neo", "store_processing"),
        NULL
      ),
      path_data_storage = list_fixture[["path_data_storage"]]
    ),
    regexp = "store_processing"
  )

  testthat::expect_error(
    build_climate_extraction_targets(
      sel_product = "palaeo",
      list_data_extraction = purrr::assign_in(
        list_extraction,
        base::list("climate_products", "palaeo", "columns"),
        c("long", "lat", "time_id")
      ),
      path_data_storage = list_fixture[["path_data_storage"]]
    ),
    regexp = "columns"
  )

  testthat::expect_error(
    build_climate_extraction_targets(
      sel_product = "palaeo",
      list_data_extraction = purrr::assign_in(
        list_extraction,
        base::list("climate_products", "palaeo", "columns"),
        c("long", "lat", "value", "age")
      ),
      path_data_storage = list_fixture[["path_data_storage"]]
    ),
    regexp = "columns"
  )

  testthat::expect_error(
    build_climate_extraction_targets(
      sel_product = "neo",
      list_data_extraction = purrr::assign_in(
        list_extraction,
        base::list("climate_products", "neo", "var_names"),
        NULL
      ),
      path_data_storage = list_fixture[["path_data_storage"]]
    ),
    regexp = "var_names"
  )
})


#----------------------------------------------------------#
# Outputs -----
#----------------------------------------------------------#

testthat::test_that("build_climate_extraction_targets() names the targets", {
  list_fixture <-
    build_extraction_pipeline_fixture(
      dir_root = withr::local_tempdir(),
      flag_upstream = FALSE
    )

  vec_names <-
    build_climate_extraction_targets(
      sel_product = "palaeo",
      list_data_extraction = list_fixture[["list_fixture"]][[
        "list_data_extraction"
      ]],
      path_data_storage = list_fixture[["path_data_storage"]]
    ) |>
    purrr::map_chr(.f = ~ .x[["settings"]][["name"]])

  testthat::expect_setequal(
    vec_names,
    c(
      "fingerprint_palaeo_processed_files", "palaeo_processed_files",
      "palaeo_extraction_files", "palaeo_processed_rasters",
      "palaeo_cell_tables", "palaeo_extracted_files"
    )
  )
})


#----------------------------------------------------------#
# Pipeline runs -----
#----------------------------------------------------------#

testthat::test_that("the extraction stage stops without the processing store", {
  list_fixture <-
    build_extraction_pipeline_fixture(
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
        "02_Data_processing/00_run.R"
      )
    )
  )
})

testthat::test_that("the extraction stage saves one cell table per raster", {
  list_fixture <-
    build_extraction_pipeline_fixture(dir_root = withr::local_tempdir())

  run_target_pipeline(
    script = list_fixture[["path_script"]],
    store = list_fixture[["path_store"]],
    verbose = FALSE
  )

  data_neo <-
    targets::tar_read_raw(
      name = "neo_extracted_files",
      store = list_fixture[["path_store"]]
    )

  testthat::expect_equal(
    dplyr::pull(data_neo, "file_name"),
    c("neo_bio1.tif", "neo_bio12.tif")
  )
  testthat::expect_equal(
    dplyr::pull(data_neo, "path_file"),
    base::file.path(
      list_fixture[["path_data_storage"]],
      "Processed/Neo/Cell_tables",
      c("neo_bio1.qs2", "neo_bio12.qs2")
    )
  )
  testthat::expect_null(base::names(dplyr::pull(data_neo, "path_file")))

  data_neo_bio12 <-
    qs2::qs_read(file = dplyr::pull(data_neo, "path_file")[2])

  testthat::expect_named(
    data_neo_bio12,
    c("long", "lat", "value", "var_name")
  )
  # Cells 2 and 7 are missing and get no row
  testthat::expect_equal(
    dplyr::pull(data_neo_bio12, "value"),
    c(10, 30, 40, 50, 60, 80)
  )
  testthat::expect_equal(
    dplyr::pull(data_neo_bio12, "var_name"),
    base::rep("bio_12", 6)
  )

  data_palaeo <-
    targets::tar_read_raw(
      name = "palaeo_extracted_files",
      store = list_fixture[["path_store"]]
    )

  data_palaeo_15 <-
    data_palaeo |>
    dplyr::filter(.data[["time_id"]] == 15L) |>
    dplyr::pull("path_file") |>
    qs2::qs_read()

  testthat::expect_named(
    data_palaeo_15,
    c("long", "lat", "time_id", "value")
  )
  testthat::expect_type(dplyr::pull(data_palaeo_15, "time_id"), "integer")
  testthat::expect_equal(
    dplyr::pull(data_palaeo_15, "time_id"),
    base::rep(15L, 7)
  )
  testthat::expect_equal(
    dplyr::pull(data_palaeo_15, "value"),
    c(101, 201, 401, 501, 601, 701, 801)
  )
  testthat::expect_equal(
    dplyr::pull(data_palaeo_15, "long"),
    c(-135, -45, 135, -135, -45, 45, 135)
  )
})

testthat::test_that("a changed raster reruns only its own cell table", {
  list_fixture <-
    build_extraction_pipeline_fixture(dir_root = withr::local_tempdir())

  run_target_pipeline(
    script = list_fixture[["path_script"]],
    store = list_fixture[["path_store"]],
    verbose = FALSE
  )

  # The same path with new values, as a rerun of processing writes it
  build_quality_raster_fixture(
    path_file = list_fixture[["list_paths"]][["palaeo_bio12_15"]],
    mat_values = base::matrix(9, nrow = 2, ncol = 4)
  )

  run_target_pipeline(
    script = list_fixture[["path_script"]],
    store = list_fixture[["path_store"]],
    verbose = FALSE
  )

  data_progress <-
    targets::tar_progress(store = list_fixture[["path_store"]])

  # Branches only; the pattern itself is listed as completed too
  vec_branches_completed <-
    data_progress |>
    dplyr::filter(
      stringr::str_detect(.data[["name"]], "_cell_tables_[0-9a-f]+$"),
      .data[["progress"]] == "completed"
    ) |>
    dplyr::pull("name")

  testthat::expect_length(vec_branches_completed, 1L)
  testthat::expect_match(vec_branches_completed, "^palaeo_cell_tables_")
  testthat::expect_equal(
    data_progress |>
      dplyr::filter(.data[["name"]] == "hwsd_cell_table") |>
      dplyr::pull("progress"),
    "skipped"
  )

  data_palaeo_15 <-
    targets::tar_read_raw(
      name = "palaeo_extracted_files",
      store = list_fixture[["path_store"]]
    ) |>
    dplyr::filter(.data[["time_id"]] == 15L) |>
    dplyr::pull("path_file") |>
    qs2::qs_read()

  testthat::expect_equal(
    dplyr::pull(data_palaeo_15, "value"),
    base::rep(9, 8)
  )
})

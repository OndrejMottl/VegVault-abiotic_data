#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("build_export_targets() validates the configuration", {
  list_fixture <-
    build_export_pipeline_fixture(dir_root = withr::local_tempdir())

  list_config <-
    purrr::chuck(list_fixture, "list_fixture")

  list_export <-
    purrr::chuck(list_config, "list_data_export")

  list_product_neo <-
    purrr::chuck(list_export, "products", "neo")

  # Each case: a broken configuration and the text its error must name
  list_cases <-
    base::list(
      base::list(
        config = purrr::list_modify(list_export, products = rlang::zap()),
        regexp = "products"
      ),
      base::list(
        config = purrr::assign_in(list_export, "dir_data", 1),
        regexp = "dir_data"
      ),
      base::list(
        config = purrr::assign_in(list_export, "flag_replace_previous", "no"),
        regexp = "flag_replace_previous"
      ),
      base::list(
        config = purrr::assign_in(
          list_export,
          "quality_check",
          base::list(store = "x")
        ),
        regexp = "quality_check"
      ),
      base::list(
        config = purrr::assign_in(
          list_export,
          "soil_group_names_missing",
          base::list("Durisols")
        ),
        regexp = "soil_group_names_missing"
      ),
      base::list(
        config = purrr::assign_in(
          list_export,
          "products",
          base::list(`Neo-1` = list_product_neo)
        ),
        regexp = "Neo-1"
      ),
      base::list(
        config = purrr::assign_in(
          list_export,
          c("products", "neo", "contract"),
          "missing_contract"
        ),
        regexp = "missing_contract"
      ),
      base::list(
        config = purrr::assign_in(
          list_export,
          "products",
          base::list(neo = list_product_neo, other = list_product_neo)
        ),
        regexp = "contract"
      )
    )

  purrr::walk(
    .x = list_cases,
    .f = ~ testthat::expect_error(
      build_export_targets(
        list_data_export = purrr::chuck(.x, "config"),
        path_data_storage = purrr::chuck(list_fixture, "path_data_storage"),
        path_project = purrr::chuck(list_fixture, "path_project"),
        list_soil_group_codes = purrr::chuck(
          list_config,
          "list_soil_group_codes"
        ),
        path_wrb_lookup = purrr::chuck(list_config, "path_wrb_lookup")
      ),
      regexp = purrr::chuck(.x, "regexp")
    )
  )
})


#----------------------------------------------------------#
# Outputs -----
#----------------------------------------------------------#

testthat::test_that("build_export_targets() declares the expected targets", {
  list_fixture <-
    build_export_pipeline_fixture(dir_root = withr::local_tempdir())

  list_config <-
    purrr::chuck(list_fixture, "list_fixture")

  res <-
    build_export_targets(
      list_data_export = purrr::chuck(list_config, "list_data_export"),
      path_data_storage = purrr::chuck(list_fixture, "path_data_storage"),
      path_project = purrr::chuck(list_fixture, "path_project"),
      list_soil_group_codes = purrr::chuck(
        list_config,
        "list_soil_group_codes"
      ),
      path_wrb_lookup = purrr::chuck(list_config, "path_wrb_lookup")
    )

  vec_names <-
    purrr::map_chr(
      .x = res,
      .f = ~ purrr::chuck(.x, "settings", "name")
    )

  vec_products <-
    c("neo", "palaeo", "hwsd")

  vec_expected <-
    c(
      "fingerprint_quality_checks", "raster_quality_validated",
      "path_contract_copy_record", "contract_copy_record",
      stringr::str_c(
        base::rep(c("path_contract_", "contract_"), times = 3),
        base::rep(
          c(
            "chelsa_neoclimate", "chelsa_trace21k_palaeoclimate",
            "hwsd_soil"
          ),
          each = 2
        )
      ),
      "path_hwsd_wrb_lookup",
      stringr::str_c(
        base::rep(vec_products, each = 7),
        base::rep(
          c(
            "_extracted_files", "_export_groups", "_export_cell_tables",
            "_export_files", "_export_folder", "_export_validated", ""
          ),
          times = 3
        )
      ) |>
        stringr::str_replace(
          pattern = "^(neo|palaeo|hwsd)$",
          replacement = "fingerprint_\\1_extracted_files"
        ),
      "path_export_manifest", "path_export_summary", "path_hwsd_soil_legend"
    )

  testthat::expect_setequal(vec_names, vec_expected)
  testthat::expect_equal(base::anyDuplicated(vec_names), 0L)
})


#----------------------------------------------------------#
# Pipeline runs -----
#----------------------------------------------------------#

testthat::test_that("the export stops when the quality check failed", {
  list_fixture <-
    build_export_pipeline_fixture(
      dir_root = withr::local_tempdir(),
      flag_quality_passed = FALSE
    )

  testthat::expect_error(
    run_target_pipeline(
      script = purrr::chuck(list_fixture, "path_script"),
      store = purrr::chuck(list_fixture, "path_store"),
      verbose = FALSE
    )
  )

  data_meta <-
    targets::tar_meta(
      fields = "error",
      store = purrr::chuck(list_fixture, "path_store")
    ) |>
    dplyr::filter(!base::is.na(.data[["error"]]))

  testthat::expect_true(
    base::any(
      stringr::str_detect(
        dplyr::pull(data_meta, "error"),
        "03_Quality_checks/00_run.R"
      )
    )
  )
  testthat::expect_false(
    base::dir.exists(
      base::file.path(
        purrr::chuck(list_fixture, "path_project"),
        "Outputs", "Data"
      )
    )
  )
})

testthat::test_that("the export saves and checks every artefact", {
  list_fixture <-
    build_export_pipeline_fixture(dir_root = withr::local_tempdir())

  run_target_pipeline(
    script = purrr::chuck(list_fixture, "path_script"),
    store = purrr::chuck(list_fixture, "path_store"),
    verbose = FALSE
  )

  path_project <-
    purrr::chuck(list_fixture, "path_project")

  path_store <-
    purrr::chuck(list_fixture, "path_store")

  list_counts <-
    base::list(Neoclimate = 7L, Palaoclimate = 28L, HWSD = 1L)

  purrr::iwalk(
    .x = list_counts,
    .f = ~ testthat::expect_length(
      base::list.files(base::file.path(path_project, "Outputs", "Data", .y)),
      .x
    )
  )

  vec_validated <-
    targets::tar_read_raw(name = "palaeo_export_validated", store = path_store)

  testthat::expect_setequal(
    vec_validated,
    base::list.files(
      base::file.path(path_project, "Outputs", "Data", "Palaoclimate"),
      full.names = TRUE
    )
  )

  data_manifest <-
    readr::read_csv(
      file = base::file.path(
        path_project, "Outputs", "Tables", "export_manifest.csv"
      ),
      show_col_types = FALSE
    )

  testthat::expect_equal(base::nrow(data_manifest), 36L)
  testthat::expect_equal(
    base::sort(base::unique(dplyr::pull(data_manifest, "contract_id"))),
    c("chelsa_neoclimate", "chelsa_trace21k_palaeoclimate", "hwsd_soil")
  )
  testthat::expect_true(
    base::all(
      stringr::str_starts(dplyr::pull(data_manifest, "path"), "Outputs/Data/")
    )
  )

  data_summary <-
    readr::read_csv(
      file = base::file.path(
        path_project, "Outputs", "Tables", "export_summary.csv"
      ),
      show_col_types = FALSE
    )

  testthat::expect_equal(base::nrow(data_summary), 15L)

  data_legend <-
    readr::read_csv(
      file = base::file.path(
        path_project, "Outputs", "Tables", "hwsd_soil_group_legend.csv"
      ),
      show_col_types = FALSE
    )

  testthat::expect_equal(
    dplyr::pull(data_legend, "soil_group"),
    c("Acrisols", "Cryosols", "Durisols", "Luvisols")
  )
})

testthat::test_that("the export stops on a file left in a folder", {
  list_fixture <-
    build_export_pipeline_fixture(dir_root = withr::local_tempdir())

  run_target_pipeline(
    script = purrr::chuck(list_fixture, "path_script"),
    store = purrr::chuck(list_fixture, "path_store"),
    verbose = FALSE
  )

  # A stale batch, as an older export would have left it
  base::file.create(
    base::file.path(
      purrr::chuck(list_fixture, "path_project"),
      "Outputs", "Data", "Palaoclimate",
      "bio01_batch_5_2024-01-02__05b0b43b6640a26c729b0403e711993f__.qs"
    )
  )

  testthat::expect_error(
    run_target_pipeline(
      script = purrr::chuck(list_fixture, "path_script"),
      store = purrr::chuck(list_fixture, "path_store"),
      verbose = FALSE
    )
  )

  data_meta <-
    targets::tar_meta(
      fields = "error",
      store = purrr::chuck(list_fixture, "path_store")
    ) |>
    dplyr::filter(!base::is.na(.data[["error"]]))

  testthat::expect_equal(
    dplyr::pull(data_meta, "name"),
    "palaeo_export_folder"
  )
  testthat::expect_match(
    dplyr::pull(data_meta, "error"),
    "bio01_batch_5"
  )
})

testthat::test_that("a changed cell table reruns only its own variable", {
  list_fixture <-
    build_export_pipeline_fixture(
      dir_root = withr::local_tempdir(),
      flag_replace_previous = TRUE
    )

  run_target_pipeline(
    script = purrr::chuck(list_fixture, "path_script"),
    store = purrr::chuck(list_fixture, "path_store"),
    verbose = FALSE
  )

  path_bio4 <-
    purrr::chuck(list_fixture, "list_cell_tables", "neo") |>
    dplyr::filter(.data[["variable"]] == "bio4") |>
    dplyr::pull("path_file")

  # The same path with new values, as a rerun of the extraction writes it
  qs2::qs_save(
    object = qs2::qs_read(file = path_bio4) |>
      dplyr::mutate(value = .data[["value"]] + 100),
    file = path_bio4
  )

  run_target_pipeline(
    script = purrr::chuck(list_fixture, "path_script"),
    store = purrr::chuck(list_fixture, "path_store"),
    verbose = FALSE
  )

  data_progress <-
    targets::tar_progress(store = purrr::chuck(list_fixture, "path_store"))

  # Branches only; the pattern itself is listed as completed too
  vec_branches_completed <-
    data_progress |>
    dplyr::filter(
      stringr::str_detect(.data[["name"]], "_export_files_[0-9a-f]+$"),
      .data[["progress"]] == "completed"
    ) |>
    dplyr::pull("name")

  testthat::expect_length(vec_branches_completed, 1L)
  testthat::expect_match(vec_branches_completed, "^neo_export_files_")

  # The earlier bio4 file is replaced, so the folder keeps seven files
  vec_files_neo <-
    base::list.files(
      base::file.path(
        purrr::chuck(list_fixture, "path_project"),
        "Outputs", "Data", "Neoclimate"
      ),
      full.names = TRUE
    )

  testthat::expect_length(vec_files_neo, 7L)

  data_bio4 <-
    vec_files_neo[stringr::str_detect(vec_files_neo, "CHELSA_bio_04_")] |>
    qs2::qs_read()

  testthat::expect_equal(dplyr::pull(data_bio4, "value"), 101:108)
})

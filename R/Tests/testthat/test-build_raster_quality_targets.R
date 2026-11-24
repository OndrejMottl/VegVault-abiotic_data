#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("build_raster_quality_targets() checks the set names", {
  list_fixture <-
    build_quality_pipeline_fixture(
      dir_root = withr::local_tempdir()
    )

  list_quality_checks <-
    list_fixture[["list_fixture"]][["list_quality_checks"]]

  testthat::expect_error(
    build_raster_quality_targets(
      vec_check_sets = "neo_missing",
      list_quality_checks = list_quality_checks,
      path_data_storage = list_fixture[["path_data_storage"]],
      dir_tables = list_fixture[["dir_tables"]]
    ),
    regexp = "neo_missing"
  )

  testthat::expect_error(
    build_raster_quality_targets(
      vec_check_sets = c("neo_new", "neo_new"),
      list_quality_checks = list_quality_checks,
      path_data_storage = list_fixture[["path_data_storage"]],
      dir_tables = list_fixture[["dir_tables"]]
    ),
    regexp = "vec_check_sets"
  )

  list_quality_checks_upper <-
    purrr::assign_in(
      list_quality_checks,
      where = c("check_sets", "Neo-New"),
      value = list_quality_checks[["check_sets"]][["neo_new"]]
    )

  testthat::expect_error(
    build_raster_quality_targets(
      vec_check_sets = "Neo-New",
      list_quality_checks = list_quality_checks_upper,
      path_data_storage = list_fixture[["path_data_storage"]],
      dir_tables = list_fixture[["dir_tables"]]
    ),
    regexp = "vec_check_sets"
  )
})

testthat::test_that("build_raster_quality_targets() checks the config", {
  list_fixture <-
    build_quality_pipeline_fixture(
      dir_root = withr::local_tempdir()
    )

  list_quality_checks <-
    list_fixture[["list_fixture"]][["list_quality_checks"]]

  testthat::expect_error(
    build_raster_quality_targets(
      vec_check_sets = "neo_new",
      list_quality_checks = base::replace(
        list_quality_checks,
        "thresholds",
        base::list(NULL)
      ),
      path_data_storage = list_fixture[["path_data_storage"]],
      dir_tables = list_fixture[["dir_tables"]]
    ),
    regexp = "thresholds"
  )

  list_quality_checks_grid <-
    purrr::assign_in(
      list_quality_checks,
      where = c("check_sets", "neo_new", "grid"),
      value = "missing_grid"
    )

  testthat::expect_error(
    build_raster_quality_targets(
      vec_check_sets = "neo_new",
      list_quality_checks = list_quality_checks_grid,
      path_data_storage = list_fixture[["path_data_storage"]],
      dir_tables = list_fixture[["dir_tables"]]
    ),
    regexp = "grid"
  )

  list_quality_checks_required <-
    purrr::assign_in(
      list_quality_checks,
      where = c("check_sets", "neo_new", "required_pass"),
      value = NA
    )

  testthat::expect_error(
    build_raster_quality_targets(
      vec_check_sets = "neo_new",
      list_quality_checks = list_quality_checks_required,
      path_data_storage = list_fixture[["path_data_storage"]],
      dir_tables = list_fixture[["dir_tables"]]
    ),
    regexp = "required_pass"
  )

  testthat::expect_error(
    build_raster_quality_targets(
      vec_check_sets = "neo_new",
      list_quality_checks = list_quality_checks,
      path_data_storage = NA_character_,
      dir_tables = list_fixture[["dir_tables"]]
    ),
    regexp = "path_data_storage"
  )

  testthat::expect_error(
    build_raster_quality_targets(
      vec_check_sets = "neo_new",
      list_quality_checks = list_quality_checks,
      path_data_storage = list_fixture[["path_data_storage"]],
      dir_tables = ""
    ),
    regexp = "dir_tables"
  )
})


#----------------------------------------------------------#
# Outputs -----
#----------------------------------------------------------#

testthat::test_that("build_raster_quality_targets() declares the targets", {
  list_fixture <-
    build_quality_pipeline_fixture(
      dir_root = withr::local_tempdir()
    )

  res <-
    build_raster_quality_targets(
      vec_check_sets = c("neo_new", "neo_legacy"),
      list_quality_checks = list_fixture[["list_fixture"]][[
        "list_quality_checks"
      ]],
      path_data_storage = list_fixture[["path_data_storage"]],
      dir_tables = list_fixture[["dir_tables"]]
    )

  testthat::expect_type(res, "list")

  testthat::expect_true(
    base::all(
      purrr::map_lgl(.x = res, .f = ~ base::inherits(.x, "tar_target"))
    )
  )

  vec_names <-
    purrr::map_chr(
      .x = res,
      .f = ~ .x[["settings"]][["name"]]
    )

  testthat::expect_setequal(
    vec_names,
    c(
      "raster_quality_thresholds", "raster_quality_allow_list",
      "raster_quality_type_limit_values", "path_raster_server_sizes",
      "data_raster_server_sizes",
      "neo_new_expected_files", "neo_new_file_inventory",
      "neo_new_block_extent", "neo_new_band_summary",
      "neo_new_expected_pairs", "neo_new_pair_comparison",
      "neo_new_findings_files", "neo_new_findings_bands",
      "neo_new_findings_pairs", "neo_new_findings",
      "neo_new_file_summary", "neo_new_tables",
      "neo_legacy_expected_files", "neo_legacy_file_inventory",
      "neo_legacy_block_extent", "neo_legacy_band_summary",
      "neo_legacy_findings_files", "neo_legacy_findings_bands",
      "neo_legacy_findings", "neo_legacy_file_summary",
      "neo_legacy_tables",
      "raster_quality_findings_required", "raster_quality_validated"
    )
  )

  vec_formats <-
    purrr::map_chr(
      .x = res,
      .f = ~ .x[["settings"]][["format"]]
    ) |>
    rlang::set_names(vec_names)

  testthat::expect_equal(
    base::unname(
      vec_formats[c("path_raster_server_sizes", "neo_new_tables")]
    ),
    c("file", "file")
  )
})

testthat::test_that("build_raster_quality_targets() omits unused targets", {
  list_fixture <-
    build_quality_pipeline_fixture(
      dir_root = withr::local_tempdir()
    )

  res <-
    build_raster_quality_targets(
      vec_check_sets = "neo_legacy",
      list_quality_checks = list_fixture[["list_fixture"]][[
        "list_quality_checks"
      ]],
      path_data_storage = list_fixture[["path_data_storage"]],
      dir_tables = list_fixture[["dir_tables"]]
    )

  vec_names <-
    purrr::map_chr(
      .x = res,
      .f = ~ .x[["settings"]][["name"]]
    )

  testthat::expect_false(
    base::any(
      c(
        "path_raster_server_sizes", "data_raster_server_sizes",
        "neo_legacy_expected_pairs", "raster_quality_validated",
        "raster_quality_findings_required"
      ) %in% vec_names
    )
  )

  testthat::expect_true("neo_legacy_tables" %in% vec_names)
})


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("build_raster_quality_targets() runs the whole check", {
  list_fixture <-
    build_quality_pipeline_fixture(
      dir_root = withr::local_tempdir()
    )

  run_target_pipeline(
    script = list_fixture[["path_script"]],
    store = list_fixture[["path_store"]],
    verbose = FALSE
  )

  vec_tables <-
    base::list.files(list_fixture[["dir_tables"]])

  testthat::expect_setequal(
    vec_tables,
    c(
      "raster_quality_neo_new_bands.csv",
      "raster_quality_neo_new_findings.csv",
      "raster_quality_neo_new_summary.csv",
      "raster_quality_neo_new_pairs.csv",
      "raster_quality_neo_legacy_bands.csv",
      "raster_quality_neo_legacy_findings.csv",
      "raster_quality_neo_legacy_summary.csv"
    )
  )

  data_summary_new <-
    readr::read_csv(
      file = base::file.path(
        list_fixture[["dir_tables"]],
        "raster_quality_neo_new_summary.csv"
      ),
      show_col_types = FALSE
    )

  testthat::expect_equal(
    dplyr::pull(data_summary_new, "status"),
    c("pass", "pass")
  )

  data_summary_legacy <-
    readr::read_csv(
      file = base::file.path(
        list_fixture[["dir_tables"]],
        "raster_quality_neo_legacy_summary.csv"
      ),
      show_col_types = FALSE
    )

  testthat::expect_equal(dplyr::pull(data_summary_legacy, "status"), "fail")

  testthat::expect_match(
    dplyr::pull(data_summary_legacy, "failing_rules"),
    "zero_share_step"
  )

  data_findings_validated <-
    targets::tar_read_raw(
      name = "raster_quality_validated",
      store = list_fixture[["path_store"]]
    )

  testthat::expect_true(
    base::all(dplyr::pull(data_findings_validated, "status") == "pass")
  )

  vec_table_text <-
    base::file.path(list_fixture[["dir_tables"]], vec_tables) |>
    purrr::map(.f = ~ base::readLines(.x)) |>
    base::unlist()

  testthat::expect_false(
    base::any(
      stringr::str_detect(
        string = vec_table_text,
        pattern = stringr::fixed(list_fixture[["path_data_storage"]])
      )
    )
  )
})

testthat::test_that("build_raster_quality_targets() reruns replaced files", {
  list_fixture <-
    build_quality_pipeline_fixture(
      dir_root = withr::local_tempdir()
    )

  run_target_pipeline(
    script = list_fixture[["path_script"]],
    store = list_fixture[["path_store"]],
    verbose = FALSE
  )

  build_quality_raster_fixture(
    path_file = list_fixture[["path_bio18"]],
    mat_values = base::matrix(25, nrow = 18, ncol = 36)
  )

  base::Sys.setFileTime(
    path = list_fixture[["path_bio18"]],
    time = base::as.POSIXct("2024-01-01 00:00:00", tz = "UTC")
  )

  readr::write_csv(
    x = tibble::tibble(
      product = "prod_neo",
      file_name = c("neo_bio12.tif", "neo_bio18.tif"),
      size_bytes = base::file.size(
        c(list_fixture[["path_bio12"]], list_fixture[["path_bio18"]])
      )
    ),
    file = list_fixture[["path_sizes"]]
  )

  run_target_pipeline(
    script = list_fixture[["path_script"]],
    store = list_fixture[["path_store"]],
    verbose = FALSE
  )

  data_progress <-
    targets::tar_progress(
      store = list_fixture[["path_store"]],
      fields = c("type", "parent", "progress")
    )

  data_progress_bands <-
    data_progress |>
    dplyr::filter(
      .data[["type"]] == "branch",
      .data[["parent"]] %in% c(
        "neo_new_band_summary", "neo_legacy_band_summary"
      )
    ) |>
    dplyr::count(.data[["parent"]], .data[["progress"]])

  testthat::expect_equal(
    dplyr::pull(data_progress_bands, "progress"),
    c("skipped", "completed", "skipped")
  )

  testthat::expect_equal(
    dplyr::pull(data_progress_bands, "parent"),
    c(
      "neo_legacy_band_summary",
      "neo_new_band_summary",
      "neo_new_band_summary"
    )
  )

  testthat::expect_equal(dplyr::pull(data_progress_bands, "n"), c(1L, 1L, 1L))
})

testthat::test_that("build_raster_quality_targets() stops after the tables", {
  list_fixture <-
    build_quality_pipeline_fixture(
      dir_root = withr::local_tempdir()
    )

  build_quality_raster_fixture(
    path_file = list_fixture[["path_bio18"]],
    mat_values = base::matrix(150, nrow = 18, ncol = 36)
  )

  readr::write_csv(
    x = tibble::tibble(
      product = "prod_neo",
      file_name = c("neo_bio12.tif", "neo_bio18.tif"),
      size_bytes = base::file.size(
        c(list_fixture[["path_bio12"]], list_fixture[["path_bio18"]])
      )
    ),
    file = list_fixture[["path_sizes"]]
  )

  testthat::expect_error(
    run_target_pipeline(
      script = list_fixture[["path_script"]],
      store = list_fixture[["path_store"]],
      verbose = FALSE
    ),
    regexp = "raster_quality_validated|failing finding"
  )

  data_summary_new <-
    readr::read_csv(
      file = base::file.path(
        list_fixture[["dir_tables"]],
        "raster_quality_neo_new_summary.csv"
      ),
      show_col_types = FALSE
    )

  testthat::expect_equal(
    dplyr::pull(data_summary_new, "status"),
    c("pass", "fail")
  )

  testthat::expect_true(
    base::file.exists(
      base::file.path(
        list_fixture[["dir_tables"]],
        "raster_quality_neo_legacy_summary.csv"
      )
    )
  )
})

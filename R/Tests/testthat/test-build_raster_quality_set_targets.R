#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("build_raster_quality_set_targets() needs a known set", {
  list_fixture <-
    build_quality_pipeline_fixture(
      dir_root = withr::local_tempdir()
    )

  testthat::expect_error(
    build_raster_quality_set_targets(
      sel_check_set = "neo_missing",
      list_quality_checks = list_fixture[["list_fixture"]][[
        "list_quality_checks"
      ]],
      path_data_storage = list_fixture[["path_data_storage"]],
      dir_tables = list_fixture[["dir_tables"]]
    ),
    regexp = "sel_check_set"
  )

  testthat::expect_error(
    build_raster_quality_set_targets(
      sel_check_set = c("neo_new", "neo_legacy"),
      list_quality_checks = list_fixture[["list_fixture"]][[
        "list_quality_checks"
      ]],
      path_data_storage = list_fixture[["path_data_storage"]],
      dir_tables = list_fixture[["dir_tables"]]
    ),
    regexp = "sel_check_set"
  )
})


#----------------------------------------------------------#
# Outputs -----
#----------------------------------------------------------#

testthat::test_that("build_raster_quality_set_targets() names the targets", {
  list_fixture <-
    build_quality_pipeline_fixture(
      dir_root = withr::local_tempdir()
    )

  res <-
    build_raster_quality_set_targets(
      sel_check_set = "neo_new",
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

  testthat::expect_equal(
    purrr::map_chr(.x = res, .f = ~ .x[["settings"]][["name"]]),
    c(
      "neo_new_expected_files", "neo_new_file_inventory",
      "neo_new_block_extent", "neo_new_band_summary",
      "neo_new_expected_pairs", "neo_new_pair_comparison",
      "neo_new_findings_files", "neo_new_findings_bands",
      "neo_new_findings_pairs", "neo_new_findings",
      "neo_new_file_summary", "neo_new_tables"
    )
  )
})

testthat::test_that("build_raster_quality_set_targets() skips no pairs", {
  list_fixture <-
    build_quality_pipeline_fixture(
      dir_root = withr::local_tempdir()
    )

  res <-
    build_raster_quality_set_targets(
      sel_check_set = "neo_legacy",
      list_quality_checks = list_fixture[["list_fixture"]][[
        "list_quality_checks"
      ]],
      path_data_storage = list_fixture[["path_data_storage"]],
      dir_tables = list_fixture[["dir_tables"]]
    )

  testthat::expect_equal(
    purrr::map_chr(.x = res, .f = ~ .x[["settings"]][["name"]]),
    c(
      "neo_legacy_expected_files", "neo_legacy_file_inventory",
      "neo_legacy_block_extent", "neo_legacy_band_summary",
      "neo_legacy_findings_files", "neo_legacy_findings_bands",
      "neo_legacy_findings", "neo_legacy_file_summary",
      "neo_legacy_tables"
    )
  )
})


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("build_raster_quality_set_targets() branches per file", {
  list_fixture <-
    build_quality_pipeline_fixture(
      dir_root = withr::local_tempdir()
    )

  list_targets <-
    build_raster_quality_set_targets(
      sel_check_set = "neo_new",
      list_quality_checks = list_fixture[["list_fixture"]][[
        "list_quality_checks"
      ]],
      path_data_storage = list_fixture[["path_data_storage"]],
      dir_tables = list_fixture[["dir_tables"]]
    )

  res <-
    rlang::set_names(
      list_targets,
      nm = purrr::map_chr(
        .x = list_targets,
        .f = ~ .x[["settings"]][["name"]]
      )
    )

  vec_patterns <-
    purrr::map_chr(
      .x = res[c(
        "neo_new_block_extent", "neo_new_band_summary",
        "neo_new_pair_comparison"
      )],
      .f = ~ base::deparse(.x[["settings"]][["pattern"]][[1]])
    )

  testthat::expect_equal(
    base::unname(vec_patterns),
    c(
      "map(neo_new_file_inventory)",
      "map(neo_new_file_inventory)",
      "map(neo_new_expected_pairs)"
    )
  )

  testthat::expect_equal(
    res[["neo_new_tables"]][["settings"]][["format"]],
    "file"
  )

  testthat::expect_equal(
    res[["neo_new_file_inventory"]][["cue"]][["mode"]],
    "always"
  )
})

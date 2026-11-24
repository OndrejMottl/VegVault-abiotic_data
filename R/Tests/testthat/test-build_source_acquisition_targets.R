#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("build_source_acquisition_targets() checks the source", {
  list_fixture <-
    build_acquisition_pipeline_fixture(dir_root = withr::local_tempdir())

  list_sources <-
    list_fixture[["list_fixture"]][["list_data_sources"]]

  list_acquisition <-
    list_fixture[["list_fixture"]][["list_data_acquisition"]]

  testthat::expect_error(
    build_source_acquisition_targets(
      sel_source = "none",
      list_data_sources = list_sources,
      list_data_acquisition = list_acquisition,
      path_data_storage = list_fixture[["path_data_storage"]]
    ),
    regexp = "sel_source"
  )

  testthat::expect_error(
    build_source_acquisition_targets(
      sel_source = "neo",
      list_data_sources = purrr::assign_in(
        list_sources,
        base::list("neo", "file_check"),
        "netcdf"
      ),
      list_data_acquisition = list_acquisition,
      path_data_storage = list_fixture[["path_data_storage"]]
    ),
    regexp = "file_check"
  )

  testthat::expect_error(
    build_source_acquisition_targets(
      sel_source = "neo",
      list_data_sources = purrr::assign_in(
        list_sources,
        base::list("neo", "flag_download"),
        "yes"
      ),
      list_data_acquisition = list_acquisition,
      path_data_storage = list_fixture[["path_data_storage"]]
    ),
    regexp = "flag_download"
  )

  testthat::expect_error(
    build_source_acquisition_targets(
      sel_source = "neo",
      list_data_sources = list_sources,
      list_data_acquisition = base::list(dir_records = "Input/Records"),
      path_data_storage = list_fixture[["path_data_storage"]]
    ),
    regexp = "dir_download"
  )

  # The source definition is built once while declaring
  testthat::expect_error(
    build_source_acquisition_targets(
      sel_source = "neo",
      list_data_sources = purrr::assign_in(
        list_sources,
        base::list("neo", "ages"),
        base::list(from = 0, to = 10)
      ),
      list_data_acquisition = list_acquisition,
      path_data_storage = list_fixture[["path_data_storage"]]
    ),
    regexp = "ages"
  )
})


#----------------------------------------------------------#
# Outputs -----
#----------------------------------------------------------#

testthat::test_that("build_source_acquisition_targets() names the targets", {
  list_fixture <-
    build_acquisition_pipeline_fixture(dir_root = withr::local_tempdir())

  list_sources <-
    list_fixture[["list_fixture"]][["list_data_sources"]]

  list_acquisition <-
    list_fixture[["list_fixture"]][["list_data_acquisition"]]

  res_neo <-
    build_source_acquisition_targets(
      sel_source = "neo",
      list_data_sources = list_sources,
      list_data_acquisition = list_acquisition,
      path_data_storage = list_fixture[["path_data_storage"]]
    )

  vec_names_neo <-
    purrr::map_chr(
      .x = res_neo,
      .f = ~ .x[["settings"]][["name"]]
    )

  testthat::expect_setequal(
    vec_names_neo,
    c(
      "neo_path_server_sizes", "neo_server_sizes", "neo_expected_files",
      "neo_downloads", "neo_file_inventory", "neo_file_checks",
      "neo_checksums", "neo_download_records", "neo_raw_files"
    )
  )

  res_arch <-
    build_source_acquisition_targets(
      sel_source = "arch",
      list_data_sources = list_sources,
      list_data_acquisition = list_acquisition,
      path_data_storage = list_fixture[["path_data_storage"]]
    )

  vec_names_arch <-
    purrr::map_chr(
      .x = res_arch,
      .f = ~ .x[["settings"]][["name"]]
    )

  testthat::expect_setequal(
    vec_names_arch,
    c(
      "arch_expected_files", "arch_downloads", "arch_file_inventory",
      "arch_file_checks", "arch_checksums", "arch_download_records",
      "arch_raw_files", "arch_extracted_files"
    )
  )
})

testthat::test_that("build_source_acquisition_targets() sets cues by flag", {
  list_fixture <-
    build_acquisition_pipeline_fixture(dir_root = withr::local_tempdir())

  list_sources <-
    list_fixture[["list_fixture"]][["list_data_sources"]]

  list_acquisition <-
    list_fixture[["list_fixture"]][["list_data_acquisition"]]

  vec_cue_off <-
    build_source_acquisition_targets(
      sel_source = "neo",
      list_data_sources = list_sources,
      list_data_acquisition = list_acquisition,
      path_data_storage = list_fixture[["path_data_storage"]]
    ) |>
    purrr::keep(
      .p = ~ .x[["settings"]][["name"]] %in%
        c("neo_downloads", "neo_checksums")
    ) |>
    purrr::map_chr(.f = ~ .x[["cue"]][["mode"]])

  testthat::expect_true(base::all(vec_cue_off == "thorough"))

  list_sources_on <-
    list_sources |>
    purrr::assign_in(base::list("neo", "flag_download"), TRUE) |>
    purrr::assign_in(base::list("neo", "flag_record_checksums"), TRUE)

  vec_cue_on <-
    build_source_acquisition_targets(
      sel_source = "neo",
      list_data_sources = list_sources_on,
      list_data_acquisition = list_acquisition,
      path_data_storage = list_fixture[["path_data_storage"]]
    ) |>
    purrr::keep(
      .p = ~ .x[["settings"]][["name"]] %in%
        c("neo_downloads", "neo_checksums")
    ) |>
    purrr::map_chr(.f = ~ .x[["cue"]][["mode"]])

  testthat::expect_true(base::all(vec_cue_on == "always"))
})


#----------------------------------------------------------#
# Pipeline runs -----
#----------------------------------------------------------#

testthat::test_that("the download stage stops when files are missing", {
  list_fixture <-
    build_acquisition_pipeline_fixture(dir_root = withr::local_tempdir())

  testthat::expect_error(
    run_target_pipeline(
      script = list_fixture[["path_script"]],
      store = list_fixture[["path_store"]],
      verbose = FALSE
    )
  )

  # The run stops at the first source that fails, so only one of the two
  #   validation targets may have run
  data_meta <-
    targets::tar_meta(
      fields = "error",
      store = list_fixture[["path_store"]]
    ) |>
    dplyr::filter(
      .data[["name"]] %in% c("neo_raw_files", "arch_raw_files")
    )

  testthat::expect_gt(base::nrow(data_meta), 0L)

  vec_errors <-
    dplyr::pull(data_meta, "error")

  testthat::expect_true(
    base::any(stringr::str_detect(vec_errors, "flag_download"), na.rm = TRUE)
  )

  # Nothing was downloaded and no record was written
  testthat::expect_false(
    base::dir.exists(
      base::file.path(list_fixture[["path_data_storage"]], "Input", "Neo")
    )
  )
  testthat::expect_length(base::list.files(list_fixture[["dir_records"]]), 0L)
})

testthat::test_that("the download stage downloads and records with flags", {
  list_fixture <-
    build_acquisition_pipeline_fixture(dir_root = withr::local_tempdir())

  list_config_on <-
    list_fixture[["list_fixture"]] |>
    purrr::assign_in(
      base::list("list_data_sources", "neo", "flag_download"),
      TRUE
    ) |>
    purrr::assign_in(
      base::list("list_data_sources", "arch", "flag_download"),
      TRUE
    )

  base::saveRDS(object = list_config_on, file = list_fixture[["path_config"]])

  run_target_pipeline(
    script = list_fixture[["path_script"]],
    store = list_fixture[["path_store"]],
    verbose = FALSE
  )

  data_raw_neo <-
    targets::tar_read_raw(
      name = "neo_raw_files",
      store = list_fixture[["path_store"]]
    )

  list_server <-
    list_fixture[["list_server"]]

  testthat::expect_equal(
    dplyr::pull(data_raw_neo, "file_name"),
    c("neo_bio1.tif", "neo_bio12.tif")
  )
  testthat::expect_equal(
    dplyr::pull(data_raw_neo, "sha256"),
    c(
      secretbase::sha256(file = list_server[["path_bio1"]]),
      secretbase::sha256(file = list_server[["path_bio12"]])
    )
  )
  testthat::expect_true(
    base::all(base::file.exists(dplyr::pull(data_raw_neo, "path_file")))
  )

  vec_extracted <-
    targets::tar_read_raw(
      name = "arch_extracted_files",
      store = list_fixture[["path_store"]]
    )

  testthat::expect_equal(
    base::basename(vec_extracted),
    c("db.mdb", "raster.bil")
  )
  testthat::expect_true(base::all(base::file.exists(vec_extracted)))

  testthat::expect_length(base::list.files(list_fixture[["dir_records"]]), 2L)

  # With the flags off again, the stage passes and downloads nothing
  base::saveRDS(
    object = list_fixture[["list_fixture"]],
    file = list_fixture[["path_config"]]
  )

  run_target_pipeline(
    script = list_fixture[["path_script"]],
    store = list_fixture[["path_store"]],
    verbose = FALSE
  )

  testthat::expect_length(base::list.files(list_fixture[["dir_records"]]), 2L)
})

testthat::test_that("the download stage keeps an incomplete local file", {
  list_fixture <-
    build_acquisition_pipeline_fixture(dir_root = withr::local_tempdir())

  dir_neo <-
    base::file.path(list_fixture[["path_data_storage"]], "Input", "Neo")

  base::dir.create(dir_neo, recursive = TRUE)

  path_local <-
    base::file.path(dir_neo, "neo_bio1.tif")

  base::file.copy(
    from = list_fixture[["list_server"]][["path_truncated"]],
    to = path_local
  )

  list_config_on <-
    list_fixture[["list_fixture"]] |>
    purrr::assign_in(
      base::list("list_data_sources", "neo", "flag_download"),
      TRUE
    )

  base::saveRDS(object = list_config_on, file = list_fixture[["path_config"]])

  testthat::expect_error(
    run_target_pipeline(
      script = list_fixture[["path_script"]],
      store = list_fixture[["path_store"]],
      verbose = FALSE
    )
  )

  data_meta <-
    targets::tar_meta(
      names = "neo_downloads",
      fields = "error",
      store = list_fixture[["path_store"]]
    )

  testthat::expect_match(dplyr::pull(data_meta, "error"), "neo_bio1.tif")
  testthat::expect_equal(
    base::file.size(path_local),
    list_fixture[["list_server"]][["size_truncated"]]
  )
})

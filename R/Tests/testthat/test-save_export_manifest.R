#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("save_export_manifest() validates its inputs", {
  dir_root <-
    withr::local_tempdir()

  contract_soil <-
    load_producer_contract(
      path_contract = here::here("Data", "Contracts", "hwsd_soil.yml")
    )

  path_soil <-
    save_export_artefact(
      data_export = tibble::tibble(long = 1, lat = 2, value = 3),
      contract = contract_soil,
      dir_output = base::file.path(dir_root, "HWSD")
    )

  path_output <-
    base::file.path(dir_root, "export_manifest.csv")

  testthat::expect_error(
    save_export_manifest(
      list_export_paths = base::list(path_soil),
      list_contracts = base::list(hwsd_soil = contract_soil),
      vegvault_commit = "abc",
      path_root = dir_root,
      path_output = path_output
    ),
    regexp = "list_export_paths"
  )
  testthat::expect_error(
    save_export_manifest(
      list_export_paths = base::list(hwsd_soil = path_soil),
      list_contracts = base::list(other = contract_soil),
      vegvault_commit = "abc",
      path_root = dir_root,
      path_output = path_output
    ),
    regexp = "list_contracts"
  )
  testthat::expect_error(
    save_export_manifest(
      list_export_paths = base::list(hwsd_soil = path_soil),
      list_contracts = base::list(hwsd_soil = contract_soil),
      vegvault_commit = NA_character_,
      path_root = dir_root,
      path_output = path_output
    ),
    regexp = "vegvault_commit"
  )
  testthat::expect_error(
    save_export_manifest(
      list_export_paths = base::list(
        hwsd_soil = base::file.path(dir_root, "HWSD", "missing.qs")
      ),
      list_contracts = base::list(hwsd_soil = contract_soil),
      vegvault_commit = "abc",
      path_root = dir_root,
      path_output = path_output
    ),
    regexp = "exist"
  )
  testthat::expect_error(
    save_export_manifest(
      list_export_paths = base::list(hwsd_soil = path_soil),
      list_contracts = base::list(hwsd_soil = contract_soil),
      vegvault_commit = "abc",
      path_root = base::file.path(dir_root, "Missing"),
      path_output = path_output
    ),
    regexp = "path_root"
  )
  testthat::expect_false(base::file.exists(path_output))
})


#----------------------------------------------------------#
# Outputs -----
#----------------------------------------------------------#

testthat::test_that("save_export_manifest() writes one row per file", {
  dir_root <-
    base::normalizePath(withr::local_tempdir(), winslash = "/")

  list_cell_tables <-
    build_export_cell_tables_fixture(
      dir_root = base::file.path(dir_root, "Input")
    )

  contract_neo <-
    load_producer_contract(
      path_contract = here::here("Data", "Contracts", "chelsa_neoclimate.yml")
    )

  contract_soil <-
    load_producer_contract(
      path_contract = here::here("Data", "Contracts", "hwsd_soil.yml")
    )

  vec_paths_neo <-
    purrr::chuck(list_cell_tables, "neo") |>
    dplyr::group_split(.data[["variable"]]) |>
    purrr::map(
      .f = ~ save_cell_table_export(
        data_files = .x,
        contract = contract_neo,
        dir_output = base::file.path(dir_root, "Outputs/Data/Neoclimate")
      )
    ) |>
    purrr::list_c()

  path_soil <-
    save_cell_table_export(
      data_files = purrr::chuck(list_cell_tables, "hwsd"),
      contract = contract_soil,
      dir_output = base::file.path(dir_root, "Outputs/Data/HWSD")
    )

  path_output <-
    base::file.path(dir_root, "Tables", "export_manifest.csv")

  # The soil artefact is listed first, so the rows start with it
  res <-
    save_export_manifest(
      list_export_paths = base::list(
        hwsd_soil = path_soil,
        chelsa_neoclimate = base::rev(vec_paths_neo)
      ),
      list_contracts = base::list(
        chelsa_neoclimate = contract_neo,
        hwsd_soil = contract_soil
      ),
      vegvault_commit = "332fa0d3e62f15b9abd9bf1c97daa6c74d5d3062",
      path_root = dir_root,
      path_output = path_output
    )

  testthat::expect_equal(res, path_output)

  data_manifest <-
    readr::read_csv(
      file = res,
      col_types = readr::cols(.default = readr::col_character())
    )

  vec_paths_expected <-
    c(path_soil, base::sort(vec_paths_neo))

  testthat::expect_named(
    data_manifest,
    c(
      "contract_id", "contract_version", "variable", "partition",
      "file_name", "path", "bytes", "sha256", "n_rows", "vegvault_commit"
    )
  )
  testthat::expect_equal(base::nrow(data_manifest), 8L)
  testthat::expect_equal(
    dplyr::pull(data_manifest, "contract_id"),
    c("hwsd_soil", base::rep("chelsa_neoclimate", 7))
  )
  testthat::expect_equal(
    dplyr::pull(data_manifest, "contract_version"),
    c("2.0.0", base::rep("1.1.0", 7))
  )
  testthat::expect_equal(
    dplyr::pull(data_manifest, "variable"),
    c(NA, "bio1", "bio4", "bio6", "bio12", "bio15", "bio18", "bio19")
  )
  testthat::expect_true(
    base::all(base::is.na(dplyr::pull(data_manifest, "partition")))
  )
  testthat::expect_equal(
    dplyr::pull(data_manifest, "file_name"),
    base::basename(vec_paths_expected)
  )
  testthat::expect_equal(
    dplyr::pull(data_manifest, "path"),
    stringr::str_remove(vec_paths_expected, stringr::str_c(dir_root, "/"))
  )
  testthat::expect_equal(
    dplyr::pull(data_manifest, "bytes"),
    base::as.character(base::file.size(vec_paths_expected))
  )
  testthat::expect_equal(
    dplyr::pull(data_manifest, "sha256"),
    base::unname(tools::sha256sum(files = vec_paths_expected))
  )
  testthat::expect_equal(
    dplyr::pull(data_manifest, "n_rows"),
    c("6", base::rep("8", 7))
  )
  testthat::expect_equal(
    base::unique(dplyr::pull(data_manifest, "vegvault_commit")),
    "332fa0d3e62f15b9abd9bf1c97daa6c74d5d3062"
  )
})


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("save_export_manifest() records the batches", {
  dir_root <-
    withr::local_tempdir()

  list_cell_tables <-
    build_export_cell_tables_fixture(
      dir_root = base::file.path(dir_root, "Input")
    )

  contract_palaeo <-
    load_producer_contract(
      path_contract = here::here(
        "Data", "Contracts", "chelsa_trace21k_palaeoclimate.yml"
      )
    )

  vec_paths <-
    save_cell_table_export(
      data_files = purrr::chuck(list_cell_tables, "palaeo") |>
        dplyr::filter(.data[["variable"]] == "bio6"),
      contract = contract_palaeo,
      dir_output = base::file.path(dir_root, "Palaoclimate")
    )

  res <-
    save_export_manifest(
      list_export_paths = base::list(
        chelsa_trace21k_palaeoclimate = vec_paths
      ),
      list_contracts = base::list(
        chelsa_trace21k_palaeoclimate = contract_palaeo
      ),
      vegvault_commit = "abc",
      path_root = dir_root,
      path_output = base::file.path(dir_root, "export_manifest.csv")
    )

  data_manifest <-
    readr::read_csv(
      file = res,
      col_types = readr::cols(.default = readr::col_character())
    )

  testthat::expect_equal(
    dplyr::pull(data_manifest, "partition"),
    c("1", "2", "3", "4")
  )
  testthat::expect_equal(
    dplyr::pull(data_manifest, "variable"),
    base::rep("bio6", 4)
  )
  testthat::expect_equal(
    dplyr::pull(data_manifest, "n_rows"),
    c("4", "4", "4", "3")
  )
})

testthat::test_that("save_export_manifest() stops on files outside root", {
  dir_root <-
    withr::local_tempdir()

  contract_soil <-
    load_producer_contract(
      path_contract = here::here("Data", "Contracts", "hwsd_soil.yml")
    )

  path_soil <-
    save_export_artefact(
      data_export = tibble::tibble(long = 1, lat = 2, value = 3),
      contract = contract_soil,
      dir_output = withr::local_tempdir()
    )

  testthat::expect_error(
    save_export_manifest(
      list_export_paths = base::list(hwsd_soil = path_soil),
      list_contracts = base::list(hwsd_soil = contract_soil),
      vegvault_commit = "abc",
      path_root = dir_root,
      path_output = base::file.path(dir_root, "export_manifest.csv")
    ),
    regexp = "path_root"
  )
})

testthat::test_that("save_export_manifest() stops on a foreign name", {
  dir_root <-
    withr::local_tempdir()

  contract_soil <-
    load_producer_contract(
      path_contract = here::here("Data", "Contracts", "hwsd_soil.yml")
    )

  path_other <-
    base::file.path(dir_root, "other.qs")

  qs2::qs_save(object = tibble::tibble(x = 1), file = path_other)

  testthat::expect_error(
    save_export_manifest(
      list_export_paths = base::list(hwsd_soil = path_other),
      list_contracts = base::list(hwsd_soil = contract_soil),
      vegvault_commit = "abc",
      path_root = dir_root,
      path_output = base::file.path(dir_root, "export_manifest.csv")
    ),
    regexp = "other.qs"
  )
})

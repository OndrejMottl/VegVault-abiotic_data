#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("validate_export_artefact() validates its inputs", {
  contract_soil <-
    load_producer_contract(
      path_contract = here::here("Data", "Contracts", "hwsd_soil.yml")
    )

  testthat::expect_error(
    validate_export_artefact(vec_paths_export = 1, contract = contract_soil),
    regexp = "vec_paths_export"
  )
  testthat::expect_error(
    validate_export_artefact(
      vec_paths_export = base::character(0),
      contract = contract_soil
    ),
    regexp = "vec_paths_export"
  )
  testthat::expect_error(
    validate_export_artefact(
      vec_paths_export = base::tempfile(fileext = ".qs"),
      contract = contract_soil
    ),
    regexp = "vec_paths_export"
  )
  testthat::expect_error(
    validate_export_artefact(
      vec_paths_export = here::here("config.yml"),
      contract = base::list()
    ),
    regexp = "contract"
  )
})


#----------------------------------------------------------#
# Outputs -----
#----------------------------------------------------------#

testthat::test_that("validate_export_artefact() passes valid exports", {
  list_cell_tables <-
    build_export_cell_tables_fixture(dir_root = withr::local_tempdir())

  list_contracts <-
    base::list(
      neo = "chelsa_neoclimate",
      palaeo = "chelsa_trace21k_palaeoclimate",
      hwsd = "hwsd_soil"
    ) |>
    purrr::map(
      .f = ~ load_producer_contract(
        path_contract = here::here(
          "Data", "Contracts", stringr::str_c(.x, ".yml")
        )
      )
    )

  dir_root_output <-
    withr::local_tempdir()

  list_paths <-
    base::names(list_contracts) |>
    rlang::set_names() |>
    purrr::map(
      .f = ~ {
        sel_product <- .x

        dir_output <-
          base::file.path(dir_root_output, sel_product)

        purrr::chuck(list_cell_tables, sel_product) |>
          dplyr::group_split(.data[["variable"]]) |>
          purrr::map(
            .f = ~ save_cell_table_export(
              data_files = .x,
              contract = purrr::chuck(list_contracts, sel_product),
              dir_output = dir_output
            )
          ) |>
          purrr::list_c()
      }
    )

  testthat::expect_equal(
    purrr::map_int(.x = list_paths, .f = ~ base::length(.x)),
    c(neo = 7L, palaeo = 28L, hwsd = 1L)
  )

  purrr::walk(
    .x = base::names(list_paths),
    .f = ~ testthat::expect_equal(
      validate_export_artefact(
        vec_paths_export = purrr::chuck(list_paths, .x),
        contract = purrr::chuck(list_contracts, .x)
      ),
      purrr::chuck(list_paths, .x)
    )
  )
})


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("validate_export_artefact() stops on a broken value", {
  contract_soil <-
    load_producer_contract(
      path_contract = here::here("Data", "Contracts", "hwsd_soil.yml")
    )

  # Code 40 is not a soil group of the contract
  path_soil <-
    save_export_artefact(
      data_export = tibble::tibble(
        long = c(-135, 135),
        lat = c(45, -45),
        value = c(1, 40),
        var_name = "HWSD2"
      ),
      contract = contract_soil,
      dir_output = withr::local_tempdir()
    )

  testthat::expect_error(
    validate_export_artefact(
      vec_paths_export = path_soil,
      contract = contract_soil
    ),
    class = "producer_contract_error"
  )
})

testthat::test_that("validate_export_artefact() needs every variable", {
  list_cell_tables <-
    build_export_cell_tables_fixture(dir_root = withr::local_tempdir())

  contract_palaeo <-
    load_producer_contract(
      path_contract = here::here(
        "Data", "Contracts", "chelsa_trace21k_palaeoclimate.yml"
      )
    )

  dir_output <-
    withr::local_tempdir()

  vec_paths <-
    purrr::chuck(list_cell_tables, "palaeo") |>
    dplyr::filter(.data[["variable"]] != "bio19") |>
    dplyr::group_split(.data[["variable"]]) |>
    purrr::map(
      .f = ~ save_cell_table_export(
        data_files = .x,
        contract = contract_palaeo,
        dir_output = dir_output
      )
    ) |>
    purrr::list_c()

  testthat::expect_error(
    validate_export_artefact(
      vec_paths_export = vec_paths,
      contract = contract_palaeo
    ),
    regexp = "bio19"
  )
})

testthat::test_that("validate_export_artefact() stops on a foreign name", {
  list_cell_tables <-
    build_export_cell_tables_fixture(dir_root = withr::local_tempdir())

  contract_neo <-
    load_producer_contract(
      path_contract = here::here("Data", "Contracts", "chelsa_neoclimate.yml")
    )

  dir_output <-
    withr::local_tempdir()

  vec_paths <-
    purrr::chuck(list_cell_tables, "neo") |>
    dplyr::group_split(.data[["variable"]]) |>
    purrr::map(
      .f = ~ save_cell_table_export(
        data_files = .x,
        contract = contract_neo,
        dir_output = dir_output
      )
    ) |>
    purrr::list_c()

  path_other <-
    base::file.path(dir_output, "other.qs")

  base::file.copy(from = vec_paths[1], to = path_other)

  testthat::expect_error(
    validate_export_artefact(
      vec_paths_export = c(vec_paths, path_other),
      contract = contract_neo
    ),
    regexp = "other.qs"
  )
})

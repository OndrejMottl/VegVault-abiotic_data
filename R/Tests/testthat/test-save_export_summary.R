#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("save_export_summary() validates its inputs", {
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
      dir_output = dir_root
    )

  path_output <-
    base::file.path(dir_root, "Tables", "export_summary.csv")

  testthat::expect_error(
    save_export_summary(
      list_export_paths = base::list(path_soil),
      list_contracts = base::list(hwsd_soil = contract_soil),
      path_output = path_output
    ),
    regexp = "list_export_paths"
  )
  testthat::expect_error(
    save_export_summary(
      list_export_paths = base::list(hwsd_soil = path_soil),
      list_contracts = base::list(other = contract_soil),
      path_output = path_output
    ),
    regexp = "list_contracts"
  )
  testthat::expect_error(
    save_export_summary(
      list_export_paths = base::list(hwsd_soil = path_soil),
      list_contracts = base::list(hwsd_soil = contract_soil),
      path_output = NA_character_
    ),
    regexp = "path_output"
  )
  testthat::expect_false(base::file.exists(path_output))
})


#----------------------------------------------------------#
# Outputs -----
#----------------------------------------------------------#

testthat::test_that("save_export_summary() writes one row per variable", {
  dir_root <-
    withr::local_tempdir()

  list_cell_tables <-
    build_export_cell_tables_fixture(
      dir_root = base::file.path(dir_root, "Input")
    )

  list_contracts <-
    base::list(
      chelsa_neoclimate = "neo",
      chelsa_trace21k_palaeoclimate = "palaeo",
      hwsd_soil = "hwsd"
    ) |>
    purrr::imap(
      .f = ~ load_producer_contract(
        path_contract = here::here(
          "Data", "Contracts", stringr::str_c(.y, ".yml")
        )
      )
    )

  list_paths <-
    base::list(
      chelsa_neoclimate = "neo",
      chelsa_trace21k_palaeoclimate = "palaeo",
      hwsd_soil = "hwsd"
    ) |>
    purrr::imap(
      .f = ~ {
        sel_contract <- .y

        purrr::chuck(list_cell_tables, .x) |>
          dplyr::group_split(.data[["variable"]]) |>
          purrr::map(
            .f = ~ save_cell_table_export(
              data_files = .x,
              contract = purrr::chuck(list_contracts, sel_contract),
              dir_output = base::file.path(dir_root, sel_contract)
            )
          ) |>
          purrr::list_c()
      }
    )

  path_output <-
    base::file.path(dir_root, "Tables", "export_summary.csv")

  res <-
    save_export_summary(
      list_export_paths = list_paths,
      list_contracts = list_contracts,
      path_output = path_output
    )

  testthat::expect_equal(res, path_output)

  data_summary <-
    readr::read_csv(file = res, show_col_types = FALSE)

  vec_variables <-
    c("bio1", "bio4", "bio6", "bio12", "bio15", "bio18", "bio19")

  testthat::expect_named(
    data_summary,
    c(
      "contract_id", "variable", "n_files", "n_rows", "n_cells",
      "n_time_slices", "value_min", "value_max"
    )
  )
  testthat::expect_equal(base::nrow(data_summary), 15L)
  testthat::expect_equal(
    dplyr::pull(data_summary, "contract_id"),
    base::rep(
      c("chelsa_neoclimate", "chelsa_trace21k_palaeoclimate", "hwsd_soil"),
      times = c(7, 7, 1)
    )
  )
  testthat::expect_equal(
    dplyr::pull(data_summary, "variable"),
    c(vec_variables, vec_variables, NA)
  )
  testthat::expect_equal(
    dplyr::pull(data_summary, "n_files"),
    base::rep(c(1, 4, 1), times = c(7, 7, 1))
  )
  testthat::expect_equal(
    dplyr::pull(data_summary, "n_rows"),
    base::rep(c(8, 15, 6), times = c(7, 7, 1))
  )
  testthat::expect_equal(
    dplyr::pull(data_summary, "n_cells"),
    base::rep(c(8, 8, 6), times = c(7, 7, 1))
  )
  testthat::expect_equal(
    dplyr::pull(data_summary, "n_time_slices"),
    base::rep(c(NA, 2, NA), times = c(7, 7, 1))
  )
  testthat::expect_equal(
    dplyr::pull(data_summary, "value_min"),
    base::rep(1, 15)
  )
  testthat::expect_equal(
    dplyr::pull(data_summary, "value_max"),
    base::rep(c(8, 18, 19), times = c(7, 7, 1))
  )
})


#----------------------------------------------------------#
# Edge cases -----
#----------------------------------------------------------#

testthat::test_that("save_export_summary() stops on a foreign name", {
  dir_root <-
    withr::local_tempdir()

  contract_soil <-
    load_producer_contract(
      path_contract = here::here("Data", "Contracts", "hwsd_soil.yml")
    )

  path_other <-
    base::file.path(dir_root, "other.qs")

  qs2::qs_save(
    object = tibble::tibble(long = 1, lat = 2, value = 3),
    file = path_other
  )

  testthat::expect_error(
    save_export_summary(
      list_export_paths = base::list(hwsd_soil = path_other),
      list_contracts = base::list(hwsd_soil = contract_soil),
      path_output = base::file.path(dir_root, "export_summary.csv")
    ),
    regexp = "other.qs"
  )
})

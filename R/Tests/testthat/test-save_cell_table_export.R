#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("save_cell_table_export() validates data_files", {
  list_cell_tables <-
    build_export_cell_tables_fixture(dir_root = withr::local_tempdir())

  contract_palaeo <-
    load_producer_contract(
      path_contract = here::here(
        "Data", "Contracts", "chelsa_trace21k_palaeoclimate.yml"
      )
    )

  data_bio1 <-
    purrr::chuck(list_cell_tables, "palaeo") |>
    dplyr::filter(.data[["variable"]] == "bio1")

  dir_output <-
    withr::local_tempdir()

  testthat::expect_error(
    save_cell_table_export(
      data_files = dplyr::select(data_bio1, -"age"),
      contract = contract_palaeo,
      dir_output = dir_output
    ),
    regexp = "data_files"
  )
  testthat::expect_error(
    save_cell_table_export(
      data_files = dplyr::slice(data_bio1, 0),
      contract = contract_palaeo,
      dir_output = dir_output
    ),
    regexp = "data_files"
  )
  testthat::expect_error(
    save_cell_table_export(
      data_files = purrr::chuck(list_cell_tables, "palaeo"),
      contract = contract_palaeo,
      dir_output = dir_output
    ),
    regexp = "one variable"
  )
  testthat::expect_error(
    save_cell_table_export(
      data_files = dplyr::mutate(data_bio1, age = 0),
      contract = contract_palaeo,
      dir_output = dir_output
    ),
    regexp = "age"
  )
  testthat::expect_error(
    save_cell_table_export(
      data_files = dplyr::mutate(
        data_bio1,
        path_file = base::file.path(dir_output, "missing.qs2")
      ),
      contract = contract_palaeo,
      dir_output = dir_output
    ),
    regexp = "exist"
  )
  testthat::expect_error(
    save_cell_table_export(
      data_files = dplyr::mutate(data_bio1, variable = "bio99"),
      contract = contract_palaeo,
      dir_output = dir_output
    ),
    regexp = "bio99"
  )
  testthat::expect_length(base::list.files(dir_output), 0L)
})

testthat::test_that("save_cell_table_export() needs the batch count", {
  list_cell_tables <-
    build_export_cell_tables_fixture(dir_root = withr::local_tempdir())

  contract_palaeo <-
    load_producer_contract(
      path_contract = here::here(
        "Data", "Contracts", "chelsa_trace21k_palaeoclimate.yml"
      )
    )

  # list_modify() merges nested lists, so only `count` is removed
  contract_no_count <-
    purrr::list_modify(
      contract_palaeo,
      partitioning = base::list(count = rlang::zap())
    )

  testthat::expect_null(
    purrr::pluck(contract_no_count, "partitioning", "count")
  )

  testthat::expect_error(
    save_cell_table_export(
      data_files = purrr::chuck(list_cell_tables, "palaeo") |>
        dplyr::filter(.data[["variable"]] == "bio1"),
      contract = contract_no_count,
      dir_output = withr::local_tempdir()
    ),
    regexp = "count"
  )
})


#----------------------------------------------------------#
# Outputs -----
#----------------------------------------------------------#

testthat::test_that("save_cell_table_export() saves one present-day file", {
  list_cell_tables <-
    build_export_cell_tables_fixture(dir_root = withr::local_tempdir())

  contract_neo <-
    load_producer_contract(
      path_contract = here::here("Data", "Contracts", "chelsa_neoclimate.yml")
    )

  data_bio4 <-
    purrr::chuck(list_cell_tables, "neo") |>
    dplyr::filter(.data[["variable"]] == "bio4")

  res <-
    save_cell_table_export(
      data_files = data_bio4,
      contract = contract_neo,
      dir_output = withr::local_tempdir(),
      date_save = base::as.Date("2026-11-23")
    )

  data_cells <-
    qs2::qs_read(file = dplyr::pull(data_bio4, "path_file"))

  testthat::expect_type(res, "character")
  testthat::expect_length(res, 1L)
  testthat::expect_equal(
    base::basename(res),
    stringr::str_c(
      "CHELSA_bio_04_2026-11-23__", rlang::hash(data_cells), "__.qs"
    )
  )
  testthat::expect_equal(qs2::qs_read(file = res), data_cells)
})

testthat::test_that("save_cell_table_export() saves four batches", {
  list_cell_tables <-
    build_export_cell_tables_fixture(dir_root = withr::local_tempdir())

  contract_palaeo <-
    load_producer_contract(
      path_contract = here::here(
        "Data", "Contracts", "chelsa_trace21k_palaeoclimate.yml"
      )
    )

  data_bio12 <-
    purrr::chuck(list_cell_tables, "palaeo") |>
    dplyr::filter(.data[["variable"]] == "bio12")

  res <-
    save_cell_table_export(
      data_files = data_bio12,
      contract = contract_palaeo,
      dir_output = withr::local_tempdir(),
      date_save = base::as.Date("2026-11-23")
    )

  testthat::expect_length(res, 4L)
  # Batch k is the k-th path
  testthat::expect_true(
    base::all(
      stringr::str_detect(
        base::basename(res),
        stringr::str_c(
          "^bio12_batch_", 1:4, "_2026-11-23__[0-9a-f]{32}__[.]qs$"
        )
      )
    )
  )

  list_batches <-
    purrr::map(.x = res, .f = ~ qs2::qs_read(file = .x))

  testthat::expect_equal(
    purrr::map_int(.x = list_batches, .f = ~ base::nrow(.x)),
    c(4L, 4L, 4L, 3L)
  )

  # Age 0 (time_id 20) comes first, although the files list it second
  data_expected <-
    dplyr::bind_rows(
      qs2::qs_read(
        file = data_bio12 |>
          dplyr::filter(.data[["time_id"]] == 20L) |>
          dplyr::pull("path_file")
      ),
      qs2::qs_read(
        file = data_bio12 |>
          dplyr::filter(.data[["time_id"]] == 15L) |>
          dplyr::pull("path_file")
      )
    )

  testthat::expect_equal(dplyr::bind_rows(list_batches), data_expected)
})

testthat::test_that("save_cell_table_export() saves the soil file", {
  list_cell_tables <-
    build_export_cell_tables_fixture(dir_root = withr::local_tempdir())

  contract_soil <-
    load_producer_contract(
      path_contract = here::here("Data", "Contracts", "hwsd_soil.yml")
    )

  data_hwsd <-
    purrr::chuck(list_cell_tables, "hwsd")

  res <-
    save_cell_table_export(
      data_files = data_hwsd,
      contract = contract_soil,
      dir_output = withr::local_tempdir(),
      date_save = base::as.Date("2026-11-23")
    )

  testthat::expect_length(res, 1L)
  testthat::expect_match(
    base::basename(res),
    "^hwsd_data_2026-11-23__[0-9a-f]{32}__[.]qs$"
  )
  testthat::expect_equal(
    qs2::qs_read(file = res),
    qs2::qs_read(file = dplyr::pull(data_hwsd, "path_file"))
  )
})


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("save_cell_table_export() passes the replace flag on", {
  list_cell_tables <-
    build_export_cell_tables_fixture(dir_root = withr::local_tempdir())

  contract_soil <-
    load_producer_contract(
      path_contract = here::here("Data", "Contracts", "hwsd_soil.yml")
    )

  dir_output <-
    withr::local_tempdir()

  res_earlier <-
    save_export_artefact(
      data_export = tibble::tibble(long = 1, lat = 2, value = 3),
      contract = contract_soil,
      dir_output = dir_output,
      date_save = base::as.Date("2026-11-22")
    )

  testthat::expect_error(
    save_cell_table_export(
      data_files = purrr::chuck(list_cell_tables, "hwsd"),
      contract = contract_soil,
      dir_output = dir_output,
      date_save = base::as.Date("2026-11-23")
    ),
    regexp = "earlier version"
  )

  res <-
    save_cell_table_export(
      data_files = purrr::chuck(list_cell_tables, "hwsd"),
      contract = contract_soil,
      dir_output = dir_output,
      date_save = base::as.Date("2026-11-23"),
      flag_replace_previous = TRUE
    )

  testthat::expect_false(base::file.exists(res_earlier))
  testthat::expect_equal(base::list.files(dir_output), base::basename(res))
})

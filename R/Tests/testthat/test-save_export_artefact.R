#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("save_export_artefact() validates its inputs", {
  contract_neo <-
    load_producer_contract(
      path_contract = here::here("Data", "Contracts", "chelsa_neoclimate.yml")
    )

  dir_output <-
    withr::local_tempdir()

  data_input <-
    tibble::tibble(long = 1, lat = 1, value = 1, var_name = "bio_01")

  testthat::expect_error(
    save_export_artefact(
      data_export = data_input,
      contract = base::list(),
      dir_output = dir_output,
      variable = "bio1"
    ),
    regexp = "contract"
  )
  testthat::expect_error(
    save_export_artefact(
      data_export = data_input,
      contract = contract_neo,
      dir_output = NA_character_,
      variable = "bio1"
    ),
    regexp = "dir_output"
  )
  testthat::expect_error(
    save_export_artefact(
      data_export = data_input,
      contract = contract_neo,
      dir_output = dir_output,
      variable = "bio1",
      date_save = "2026-11-23"
    ),
    regexp = "date_save"
  )
  testthat::expect_error(
    save_export_artefact(
      data_export = data_input,
      contract = contract_neo,
      dir_output = dir_output,
      variable = "bio1",
      flag_replace_previous = "yes"
    ),
    regexp = "flag_replace_previous"
  )
  testthat::expect_length(base::list.files(dir_output), 0L)
})

testthat::test_that("save_export_artefact() checks variable and partition", {
  contract_neo <-
    load_producer_contract(
      path_contract = here::here("Data", "Contracts", "chelsa_neoclimate.yml")
    )

  contract_palaeo <-
    load_producer_contract(
      path_contract = here::here(
        "Data", "Contracts", "chelsa_trace21k_palaeoclimate.yml"
      )
    )

  contract_soil <-
    load_producer_contract(
      path_contract = here::here("Data", "Contracts", "hwsd_soil.yml")
    )

  dir_output <-
    withr::local_tempdir()

  data_input <-
    tibble::tibble(long = 1, lat = 1, value = 1)

  testthat::expect_error(
    save_export_artefact(
      data_export = data_input,
      contract = contract_neo,
      dir_output = dir_output
    ),
    regexp = "variable"
  )
  testthat::expect_error(
    save_export_artefact(
      data_export = data_input,
      contract = contract_neo,
      dir_output = dir_output,
      variable = "bio99"
    ),
    regexp = "variable"
  )
  testthat::expect_error(
    save_export_artefact(
      data_export = data_input,
      contract = contract_soil,
      dir_output = dir_output,
      variable = "soil_group"
    ),
    regexp = "variable"
  )
  testthat::expect_error(
    save_export_artefact(
      data_export = data_input,
      contract = contract_palaeo,
      dir_output = dir_output,
      variable = "bio1"
    ),
    regexp = "partition"
  )
  testthat::expect_error(
    save_export_artefact(
      data_export = data_input,
      contract = contract_palaeo,
      dir_output = dir_output,
      variable = "bio1",
      partition = 0
    ),
    regexp = "partition"
  )
  testthat::expect_error(
    save_export_artefact(
      data_export = data_input,
      contract = contract_palaeo,
      dir_output = dir_output,
      variable = "bio1",
      partition = 1.5
    ),
    regexp = "partition"
  )
  testthat::expect_error(
    save_export_artefact(
      data_export = data_input,
      contract = contract_neo,
      dir_output = dir_output,
      variable = "bio1",
      partition = 1
    ),
    regexp = "partition"
  )
  testthat::expect_length(base::list.files(dir_output), 0L)
})


#----------------------------------------------------------#
# Outputs -----
#----------------------------------------------------------#

testthat::test_that("save_export_artefact() saves under the contract name", {
  contract_neo <-
    load_producer_contract(
      path_contract = here::here("Data", "Contracts", "chelsa_neoclimate.yml")
    )

  dir_output <-
    withr::local_tempdir()

  data_input <-
    tibble::tibble(
      long = c(1, 2),
      lat = c(3, 4),
      value = c(5, 6),
      var_name = "bio_04"
    )

  res <-
    save_export_artefact(
      data_export = data_input,
      contract = contract_neo,
      dir_output = dir_output,
      variable = "bio4",
      date_save = base::as.Date("2026-11-23")
    )

  testthat::expect_type(res, "character")
  testthat::expect_equal(
    res,
    base::file.path(
      dir_output,
      stringr::str_c(
        "CHELSA_bio_04_2026-11-23__", rlang::hash(data_input), "__.qs"
      )
    )
  )
  testthat::expect_true(base::file.exists(res))
  testthat::expect_equal(qs2::qs_read(file = res), data_input)
  testthat::expect_equal(base::list.files(dir_output), base::basename(res))
})

testthat::test_that("save_export_artefact() names batches and soil files", {
  contract_palaeo <-
    load_producer_contract(
      path_contract = here::here(
        "Data", "Contracts", "chelsa_trace21k_palaeoclimate.yml"
      )
    )

  contract_soil <-
    load_producer_contract(
      path_contract = here::here("Data", "Contracts", "hwsd_soil.yml")
    )

  dir_output <-
    withr::local_tempdir()

  data_input <-
    tibble::tibble(long = 1, lat = 2, time_id = 20L, value = 3)

  res_palaeo <-
    save_export_artefact(
      data_export = data_input,
      contract = contract_palaeo,
      dir_output = base::file.path(dir_output, "Palaoclimate"),
      variable = "bio12",
      partition = 2,
      date_save = base::as.Date("2026-11-23")
    )

  testthat::expect_equal(
    base::basename(res_palaeo),
    stringr::str_c(
      "bio12_batch_2_2026-11-23__", rlang::hash(data_input), "__.qs"
    )
  )
  testthat::expect_true(base::file.exists(res_palaeo))

  res_soil <-
    save_export_artefact(
      data_export = data_input,
      contract = contract_soil,
      dir_output = base::file.path(dir_output, "HWSD"),
      date_save = base::as.Date("2026-11-23")
    )

  testthat::expect_equal(
    base::basename(res_soil),
    stringr::str_c("hwsd_data_2026-11-23__", rlang::hash(data_input), "__.qs")
  )
  testthat::expect_true(base::file.exists(res_soil))
})


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("save_export_artefact() keeps an existing file", {
  contract_soil <-
    load_producer_contract(
      path_contract = here::here("Data", "Contracts", "hwsd_soil.yml")
    )

  dir_output <-
    withr::local_tempdir()

  data_input <-
    tibble::tibble(long = 1, lat = 2, value = 3, var_name = "HWSD2")

  res_first <-
    save_export_artefact(
      data_export = data_input,
      contract = contract_soil,
      dir_output = dir_output,
      date_save = base::as.Date("2026-11-23")
    )

  base::writeLines(text = "kept", con = res_first)

  res_second <-
    save_export_artefact(
      data_export = data_input,
      contract = contract_soil,
      dir_output = dir_output,
      date_save = base::as.Date("2026-11-23")
    )

  testthat::expect_equal(res_second, res_first)
  testthat::expect_equal(base::readLines(con = res_first), "kept")
})

testthat::test_that("save_export_artefact() stops on an earlier version", {
  contract_neo <-
    load_producer_contract(
      path_contract = here::here("Data", "Contracts", "chelsa_neoclimate.yml")
    )

  dir_output <-
    withr::local_tempdir()

  res_earlier <-
    save_export_artefact(
      data_export = tibble::tibble(long = 1, lat = 2, value = 3),
      contract = contract_neo,
      dir_output = dir_output,
      variable = "bio1",
      date_save = base::as.Date("2026-11-22")
    )

  testthat::expect_error(
    save_export_artefact(
      data_export = tibble::tibble(long = 1, lat = 2, value = 4),
      contract = contract_neo,
      dir_output = dir_output,
      variable = "bio1",
      date_save = base::as.Date("2026-11-23")
    ),
    regexp = "earlier version"
  )
  testthat::expect_equal(
    base::list.files(dir_output),
    base::basename(res_earlier)
  )
})

testthat::test_that("save_export_artefact() replaces an earlier version", {
  contract_neo <-
    load_producer_contract(
      path_contract = here::here("Data", "Contracts", "chelsa_neoclimate.yml")
    )

  dir_output <-
    withr::local_tempdir()

  res_bio1 <-
    save_export_artefact(
      data_export = tibble::tibble(long = 1, lat = 2, value = 3),
      contract = contract_neo,
      dir_output = dir_output,
      variable = "bio1",
      date_save = base::as.Date("2026-11-22")
    )

  res_bio4 <-
    save_export_artefact(
      data_export = tibble::tibble(long = 1, lat = 2, value = 3),
      contract = contract_neo,
      dir_output = dir_output,
      variable = "bio4",
      date_save = base::as.Date("2026-11-22")
    )

  base::writeLines(
    text = "other",
    con = base::file.path(dir_output, "notes.txt")
  )

  res_new <-
    save_export_artefact(
      data_export = tibble::tibble(long = 1, lat = 2, value = 4),
      contract = contract_neo,
      dir_output = dir_output,
      variable = "bio1",
      date_save = base::as.Date("2026-11-23"),
      flag_replace_previous = TRUE
    )

  # Only the earlier bio1 file goes; bio4 and a foreign file stay
  testthat::expect_false(base::file.exists(res_bio1))
  testthat::expect_true(base::file.exists(res_new))
  testthat::expect_true(base::file.exists(res_bio4))
  testthat::expect_true(
    base::file.exists(base::file.path(dir_output, "notes.txt"))
  )
})

testthat::test_that("save_export_artefact() keeps other batches", {
  contract_palaeo <-
    load_producer_contract(
      path_contract = here::here(
        "Data", "Contracts", "chelsa_trace21k_palaeoclimate.yml"
      )
    )

  dir_output <-
    withr::local_tempdir()

  vec_paths <-
    purrr::map_chr(
      .x = 1:2,
      .f = ~ save_export_artefact(
        data_export = tibble::tibble(long = 1, lat = 2, value = .x),
        contract = contract_palaeo,
        dir_output = dir_output,
        variable = "bio1",
        partition = .x,
        date_save = base::as.Date("2026-11-23")
      )
    )

  testthat::expect_true(base::all(base::file.exists(vec_paths)))
  testthat::expect_length(base::list.files(dir_output), 2L)
})

testthat::test_that("save_export_artefact() leaves no temporary file", {
  contract_soil <-
    load_producer_contract(
      path_contract = here::here("Data", "Contracts", "hwsd_soil.yml")
    )

  dir_output <-
    withr::local_tempdir()

  save_export_artefact(
    data_export = tibble::tibble(long = 1, lat = 2, value = 3),
    contract = contract_soil,
    dir_output = dir_output
  )

  testthat::expect_false(
    base::any(stringr::str_detect(base::list.files(dir_output), "[.]part$"))
  )
  testthat::expect_match(
    base::list.files(dir_output),
    stringr::str_c(
      "^hwsd_data_", base::format(base::Sys.Date(), "%Y-%m-%d"), "__"
    )
  )
})

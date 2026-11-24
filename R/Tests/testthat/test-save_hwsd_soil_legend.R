#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("save_hwsd_soil_legend() validates its inputs", {
  dir_root <-
    withr::local_tempdir()

  path_lookup <-
    base::file.path(dir_root, "d_wrb2.csv")

  readr::write_csv(
    x = tibble::tibble(CODE = c("AC", "CR"), Value = c("Acrisols", "Cryosols")),
    file = path_lookup
  )

  path_output <-
    base::file.path(dir_root, "legend.csv")

  testthat::expect_error(
    save_hwsd_soil_legend(
      list_soil_group_codes = base::list(1, 9),
      path_wrb_lookup = path_lookup,
      path_output = path_output
    ),
    regexp = "list_soil_group_codes"
  )
  testthat::expect_error(
    save_hwsd_soil_legend(
      list_soil_group_codes = base::list(AC = 1, CR = 9),
      path_wrb_lookup = base::file.path(dir_root, "missing.csv"),
      path_output = path_output
    ),
    regexp = "path_wrb_lookup"
  )
  testthat::expect_error(
    save_hwsd_soil_legend(
      list_soil_group_codes = base::list(AC = 1, CR = 9),
      path_wrb_lookup = path_lookup,
      path_output = 1
    ),
    regexp = "path_output"
  )
  testthat::expect_error(
    save_hwsd_soil_legend(
      list_soil_group_codes = base::list(AC = 1, CR = 9),
      path_wrb_lookup = path_lookup,
      path_output = path_output,
      list_names_missing = base::list("Durisols")
    ),
    regexp = "list_names_missing"
  )
  testthat::expect_error(
    save_hwsd_soil_legend(
      list_soil_group_codes = base::list(AC = 1, CR = 9),
      path_wrb_lookup = path_lookup,
      path_output = path_output,
      list_names_missing = base::list(DU = 10)
    ),
    regexp = "list_names_missing"
  )
  testthat::expect_false(base::file.exists(path_output))
})


#----------------------------------------------------------#
# Outputs -----
#----------------------------------------------------------#

testthat::test_that("save_hwsd_soil_legend() writes the sorted legend", {
  dir_root <-
    withr::local_tempdir()

  path_lookup <-
    base::file.path(dir_root, "d_wrb2.csv")

  readr::write_csv(
    x = tibble::tibble(
      CODE = c("LV", "GG", "AC", "CR"),
      Value = c("Luvisols", "Glaciers", "Acrisols", "Cryosols")
    ),
    file = path_lookup
  )

  path_output <-
    base::file.path(dir_root, "Tables", "hwsd_soil_group_legend.csv")

  res <-
    save_hwsd_soil_legend(
      list_soil_group_codes = base::list(LV = 19L, AC = 1L, CR = 9L),
      path_wrb_lookup = path_lookup,
      path_output = path_output
    )

  testthat::expect_equal(res, path_output)

  data_legend <-
    readr::read_csv(file = res, show_col_types = FALSE)

  testthat::expect_named(data_legend, c("value", "wrb_code", "soil_group"))
  testthat::expect_equal(dplyr::pull(data_legend, "value"), c(1, 9, 19))
  testthat::expect_equal(
    dplyr::pull(data_legend, "wrb_code"),
    c("AC", "CR", "LV")
  )
  testthat::expect_equal(
    dplyr::pull(data_legend, "soil_group"),
    c("Acrisols", "Cryosols", "Luvisols")
  )
})


testthat::test_that("save_hwsd_soil_legend() fills names the lookup lacks", {
  dir_root <-
    withr::local_tempdir()

  path_lookup <-
    base::file.path(dir_root, "d_wrb2.csv")

  readr::write_csv(
    x = tibble::tibble(CODE = c("CR", "AC"), Value = c("Cryosols", "Acrisols")),
    file = path_lookup
  )

  res <-
    save_hwsd_soil_legend(
      list_soil_group_codes = base::list(AC = 1, CR = 9, DU = 10),
      path_wrb_lookup = path_lookup,
      path_output = base::file.path(dir_root, "legend.csv"),
      list_names_missing = base::list(DU = "Durisols")
    )

  data_legend <-
    readr::read_csv(file = res, show_col_types = FALSE)

  testthat::expect_equal(dplyr::pull(data_legend, "value"), c(1, 9, 10))
  testthat::expect_equal(
    dplyr::pull(data_legend, "soil_group"),
    c("Acrisols", "Cryosols", "Durisols")
  )
})


#----------------------------------------------------------#
# Edge cases -----
#----------------------------------------------------------#

testthat::test_that("save_hwsd_soil_legend() never replaces an HWSD name", {
  dir_root <-
    withr::local_tempdir()

  path_lookup <-
    base::file.path(dir_root, "d_wrb2.csv")

  readr::write_csv(
    x = tibble::tibble(CODE = c("AC", "CR"), Value = c("Acrisols", "Cryosols")),
    file = path_lookup
  )

  path_output <-
    base::file.path(dir_root, "legend.csv")

  testthat::expect_error(
    save_hwsd_soil_legend(
      list_soil_group_codes = base::list(AC = 1, CR = 9),
      path_wrb_lookup = path_lookup,
      path_output = path_output,
      list_names_missing = base::list(CR = "Cryosols")
    ),
    regexp = "CR"
  )
  testthat::expect_false(base::file.exists(path_output))
})

testthat::test_that("save_hwsd_soil_legend() checks the codes and names", {
  dir_root <-
    withr::local_tempdir()

  path_lookup <-
    base::file.path(dir_root, "d_wrb2.csv")

  readr::write_csv(
    x = tibble::tibble(
      CODE = c("AC", "CR", "CR"),
      Value = c("Acrisols", "Cryosols", "Cryosols again")
    ),
    file = path_lookup
  )

  path_lookup_columns <-
    base::file.path(dir_root, "d_wrb2_columns.csv")

  readr::write_csv(
    x = tibble::tibble(code = "AC", name = "Acrisols"),
    file = path_lookup_columns
  )

  path_output <-
    base::file.path(dir_root, "legend.csv")

  testthat::expect_error(
    save_hwsd_soil_legend(
      list_soil_group_codes = base::list(AC = 1, XX = 2),
      path_wrb_lookup = path_lookup,
      path_output = path_output
    ),
    regexp = "XX"
  )
  testthat::expect_error(
    save_hwsd_soil_legend(
      list_soil_group_codes = base::list(AC = 1, CR = 9),
      path_wrb_lookup = path_lookup,
      path_output = path_output
    ),
    regexp = "CR"
  )
  testthat::expect_error(
    save_hwsd_soil_legend(
      list_soil_group_codes = base::list(AC = 1, LV = 1),
      path_wrb_lookup = path_lookup,
      path_output = path_output
    ),
    regexp = "repeated"
  )
  testthat::expect_error(
    save_hwsd_soil_legend(
      list_soil_group_codes = base::list(AC = 1.5),
      path_wrb_lookup = path_lookup,
      path_output = path_output
    ),
    regexp = "whole"
  )
  testthat::expect_error(
    save_hwsd_soil_legend(
      list_soil_group_codes = base::list(AC = 1),
      path_wrb_lookup = path_lookup_columns,
      path_output = path_output
    ),
    regexp = "CODE"
  )
  testthat::expect_false(base::file.exists(path_output))
})

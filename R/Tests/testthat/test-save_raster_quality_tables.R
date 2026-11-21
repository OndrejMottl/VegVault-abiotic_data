#----------------------------------------------------------#
# Fixtures -----
#----------------------------------------------------------#

data_bands_small <-
  tibble::tibble(
    file_name = "bio4.tif",
    band_south = c(10, 0),
    band_north = c(20, 10),
    share_zero = c(0, 1)
  )

data_findings_small <-
  tibble::tibble(
    product = "chelsa",
    variable = "bio4",
    age = 0,
    file_name = "bio4.tif",
    band_south = c(10, 0),
    band_north = c(20, 10),
    rule = "zero_share_max",
    value = c(0, 1),
    threshold = 0.5,
    detail = "",
    status = c("pass", "fail")
  )

data_summary_small <-
  tibble::tibble(
    product = "chelsa",
    variable = "bio4",
    age = 0,
    file_name = "bio4.tif",
    status = "fail",
    n_failures = 1,
    failing_rules = "zero_share_max",
    allowed_rules = NA_character_
  )


#----------------------------------------------------------#
# Output -----
#----------------------------------------------------------#

testthat::test_that("save_raster_quality_tables() writes the tables", {
  dir_output <-
    base::file.path(withr::local_tempdir(), "Tables")

  res <-
    save_raster_quality_tables(
      data_band_summary = data_bands_small,
      data_findings = data_findings_small,
      data_file_summary = data_summary_small,
      dir_output = dir_output,
      table_prefix = "raster_quality_test"
    )

  testthat::expect_named(res, c("bands", "findings", "summary"))
  testthat::expect_equal(
    base::basename(base::unname(res)),
    c(
      "raster_quality_test_bands.csv",
      "raster_quality_test_findings.csv",
      "raster_quality_test_summary.csv"
    )
  )
  testthat::expect_true(base::all(base::file.exists(res)))

  data_findings_read <-
    readr::read_csv(
      purrr::chuck(res, "findings"),
      show_col_types = FALSE
    )

  # Passing findings are left out
  testthat::expect_equal(base::nrow(data_findings_read), 1L)
  testthat::expect_equal(dplyr::pull(data_findings_read, "status"), "fail")

  data_bands_read <-
    readr::read_csv(
      purrr::chuck(res, "bands"),
      show_col_types = FALSE
    )

  testthat::expect_equal(base::nrow(data_bands_read), 2L)
})

testthat::test_that("save_raster_quality_tables() writes pair tables", {
  dir_output <-
    withr::local_tempdir()

  res <-
    save_raster_quality_tables(
      data_band_summary = data_bands_small,
      data_findings = data_findings_small,
      data_file_summary = data_summary_small,
      dir_output = dir_output,
      table_prefix = "raster_quality_test",
      data_pair_comparison = tibble::tibble(
        file_name = "bio18.tif",
        file_name_reference = "bio12.tif",
        n_exceeding = 0
      )
    )

  testthat::expect_named(res, c("bands", "findings", "summary", "pairs"))
  testthat::expect_true(base::file.exists(purrr::chuck(res, "pairs")))
})

testthat::test_that("save_raster_quality_tables() replaces old tables", {
  dir_output <-
    withr::local_tempdir()

  save_raster_quality_tables(
    data_band_summary = data_bands_small,
    data_findings = data_findings_small,
    data_file_summary = data_summary_small,
    dir_output = dir_output,
    table_prefix = "raster_quality_test"
  )

  res <-
    save_raster_quality_tables(
      data_band_summary = data_bands_small[1, ],
      data_findings = data_findings_small,
      data_file_summary = data_summary_small,
      dir_output = dir_output,
      table_prefix = "raster_quality_test"
    )

  data_bands_read <-
    readr::read_csv(
      purrr::chuck(res, "bands"),
      show_col_types = FALSE
    )

  testthat::expect_equal(base::nrow(data_bands_read), 1L)
  testthat::expect_length(base::list.files(dir_output), 3)
})


#----------------------------------------------------------#
# Machine paths -----
#----------------------------------------------------------#

testthat::test_that("save_raster_quality_tables() refuses file paths", {
  dir_output <-
    withr::local_tempdir()

  testthat::expect_error(
    save_raster_quality_tables(
      data_band_summary = dplyr::mutate(
        data_bands_small,
        file_name = "D:/data/bio4.tif"
      ),
      data_findings = data_findings_small,
      data_file_summary = data_summary_small,
      dir_output = dir_output,
      table_prefix = "raster_quality_test"
    ),
    regexp = "path"
  )

  testthat::expect_length(base::list.files(dir_output), 0)
})


#----------------------------------------------------------#
# Input validation -----
#----------------------------------------------------------#

testthat::test_that("save_raster_quality_tables() validates the inputs", {
  dir_output <-
    withr::local_tempdir()

  testthat::expect_error(
    save_raster_quality_tables(
      data_band_summary = data_bands_small,
      data_findings = data_findings_small,
      data_file_summary = data_summary_small,
      dir_output = dir_output,
      table_prefix = "Raster quality"
    ),
    regexp = "table_prefix"
  )

  testthat::expect_error(
    save_raster_quality_tables(
      data_band_summary = data_bands_small,
      data_findings = dplyr::select(data_findings_small, -"status"),
      data_file_summary = data_summary_small,
      dir_output = dir_output,
      table_prefix = "raster_quality_test"
    ),
    regexp = "data_findings"
  )

  testthat::expect_error(
    save_raster_quality_tables(
      data_band_summary = data_bands_small,
      data_findings = data_findings_small,
      data_file_summary = data_summary_small,
      dir_output = 1,
      table_prefix = "raster_quality_test"
    ),
    regexp = "dir_output"
  )
})

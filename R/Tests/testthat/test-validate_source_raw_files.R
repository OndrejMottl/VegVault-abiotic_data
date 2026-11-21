#----------------------------------------------------------#
# Fixtures -----
#----------------------------------------------------------#

data_expected_valid <-
  tibble::tibble(
    product = "prod_neo",
    variable = c("bio1", "bio12"),
    age = 0,
    time_id = NA_integer_,
    file_name = c("neo_bio1.tif", "neo_bio12.tif"),
    path_file = c("Data/neo_bio1.tif", "Data/neo_bio12.tif"),
    url = c("https://example.org/bio1", "https://example.org/bio12"),
    expected_size_bytes = c(100, 200)
  )

data_checks_valid <-
  tibble::tibble(
    file_name = c("neo_bio1.tif", "neo_bio12.tif"),
    path_file = c("Data/neo_bio1.tif", "Data/neo_bio12.tif"),
    flag_present = TRUE,
    size_bytes = c(100, 200),
    flag_size_ok = TRUE,
    flag_content_ok = TRUE,
    flag_complete = TRUE,
    detail = ""
  )

data_records_valid <-
  tibble::tibble(
    product = "prod_neo",
    file_name = c("neo_bio1.tif", "neo_bio1.tif", "neo_bio12.tif"),
    url = c(
      "https://example.org/bio1", "https://example.org/bio1",
      "https://example.org/bio12"
    ),
    size_bytes = c(100, 100, 200),
    sha256 = c("old", "new", "twelve"),
    downloaded_utc = c(
      "2026-11-20T13:00:00Z", NA_character_, "2026-11-20T13:01:00Z"
    ),
    recorded_utc = c(
      "2026-11-20T13:00:00Z", "2026-11-21T09:00:00Z", "2026-11-20T13:01:00Z"
    ),
    method = c("download", "checksum", "download")
  )


#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("validate_source_raw_files() validates its arguments", {
  testthat::expect_error(
    validate_source_raw_files(
      data_expected_files = dplyr::select(data_expected_valid, -"file_name"),
      data_file_checks = data_checks_valid,
      data_download_records = data_records_valid,
      sel_source = "neo"
    ),
    regexp = "data_expected_files"
  )

  testthat::expect_error(
    validate_source_raw_files(
      data_expected_files = data_expected_valid,
      data_file_checks = dplyr::select(data_checks_valid, -"flag_complete"),
      data_download_records = data_records_valid,
      sel_source = "neo"
    ),
    regexp = "data_file_checks"
  )

  testthat::expect_error(
    validate_source_raw_files(
      data_expected_files = data_expected_valid,
      data_file_checks = data_checks_valid,
      data_download_records = dplyr::select(data_records_valid, -"sha256"),
      sel_source = "neo"
    ),
    regexp = "data_download_records"
  )

  testthat::expect_error(
    validate_source_raw_files(
      data_expected_files = data_expected_valid,
      data_file_checks = data_checks_valid,
      data_download_records = data_records_valid,
      sel_source = 1
    ),
    regexp = "sel_source"
  )
})


#----------------------------------------------------------#
# Outputs -----
#----------------------------------------------------------#

testthat::test_that("validate_source_raw_files() returns the valid files", {
  res <-
    validate_source_raw_files(
      data_expected_files = data_expected_valid,
      data_file_checks = data_checks_valid,
      data_download_records = data_records_valid,
      sel_source = "neo"
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(
    res,
    c(
      "product", "variable", "age", "time_id", "file_name", "path_file",
      "url", "size_bytes", "sha256", "downloaded_utc", "method"
    )
  )
  testthat::expect_equal(
    dplyr::pull(res, "file_name"),
    c("neo_bio1.tif", "neo_bio12.tif")
  )
  # The latest record of bio1 is the checksum of 2026-11-21
  testthat::expect_equal(dplyr::pull(res, "sha256"), c("new", "twelve"))
  testthat::expect_equal(
    dplyr::pull(res, "method"),
    c("checksum", "download")
  )
})


#----------------------------------------------------------#
# Failing closed -----
#----------------------------------------------------------#

testthat::test_that("validate_source_raw_files() fails a missing file", {
  data_checks_missing <-
    data_checks_valid |>
    dplyr::mutate(
      flag_present = c(TRUE, FALSE),
      size_bytes = c(100, NA_real_),
      flag_size_ok = c(TRUE, NA),
      flag_content_ok = c(TRUE, NA),
      flag_complete = c(TRUE, FALSE),
      detail = c("", "The file is missing.")
    )

  testthat::expect_error(
    validate_source_raw_files(
      data_expected_files = data_expected_valid,
      data_file_checks = data_checks_missing,
      data_download_records = data_records_valid,
      sel_source = "neo"
    ),
    regexp = "neo_bio12.tif"
  )

  list_condition <-
    rlang::catch_cnd(
      expr = validate_source_raw_files(
        data_expected_files = data_expected_valid,
        data_file_checks = data_checks_missing,
        data_download_records = data_records_valid,
        sel_source = "neo"
      ),
      classes = "error"
    )

  text_message <-
    rlang::cnd_message(list_condition)

  testthat::expect_match(text_message, "flag_download")
  testthat::expect_match(text_message, "neo")
  testthat::expect_match(text_message, "01_Data_acquisition/00_run.R")
})

testthat::test_that("validate_source_raw_files() fails an incomplete file", {
  data_checks_incomplete <-
    data_checks_valid |>
    dplyr::mutate(
      flag_content_ok = c(FALSE, TRUE),
      flag_complete = c(FALSE, TRUE),
      detail = c("3 blocks end beyond the end of the file.", "")
    )

  list_condition <-
    rlang::catch_cnd(
      expr = validate_source_raw_files(
        data_expected_files = data_expected_valid,
        data_file_checks = data_checks_incomplete,
        data_download_records = data_records_valid,
        sel_source = "neo"
      ),
      classes = "error"
    )

  testthat::expect_s3_class(list_condition, "error")

  text_message <-
    rlang::cnd_message(list_condition)

  testthat::expect_match(text_message, "neo_bio1.tif")
  testthat::expect_match(text_message, "beyond")
  testthat::expect_match(text_message, "never overwritten")
})

testthat::test_that("validate_source_raw_files() fails an unrecorded file", {
  list_condition <-
    rlang::catch_cnd(
      expr = validate_source_raw_files(
        data_expected_files = data_expected_valid,
        data_file_checks = data_checks_valid,
        data_download_records = dplyr::filter(
          data_records_valid,
          .data[["file_name"]] == "neo_bio1.tif"
        ),
        sel_source = "neo"
      ),
      classes = "error"
    )

  testthat::expect_s3_class(list_condition, "error")

  text_message <-
    rlang::cnd_message(list_condition)

  testthat::expect_match(text_message, "neo_bio12.tif")
  testthat::expect_match(text_message, "flag_record_checksums")
})

testthat::test_that("validate_source_raw_files() needs the current size", {
  testthat::expect_error(
    validate_source_raw_files(
      data_expected_files = data_expected_valid,
      data_file_checks = data_checks_valid,
      data_download_records = dplyr::mutate(
        data_records_valid,
        size_bytes = c(100, 100, 199)
      ),
      sel_source = "neo"
    ),
    regexp = "neo_bio12.tif"
  )
})

testthat::test_that("validate_source_raw_files() fails a file not checked", {
  testthat::expect_error(
    validate_source_raw_files(
      data_expected_files = data_expected_valid,
      data_file_checks = dplyr::slice(data_checks_valid, 1),
      data_download_records = data_records_valid,
      sel_source = "neo"
    ),
    regexp = "neo_bio12.tif"
  )
})

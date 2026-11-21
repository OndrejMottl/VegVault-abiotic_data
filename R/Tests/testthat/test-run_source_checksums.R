#----------------------------------------------------------#
# Fixtures -----
#----------------------------------------------------------#

vec_record_columns <-
  c(
    "product", "file_name", "url", "size_bytes", "sha256",
    "downloaded_utc", "recorded_utc", "method"
  )


#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("run_source_checksums() validates its arguments", {
  dir_root <-
    withr::local_tempdir()

  list_server <-
    build_raw_source_fixture(dir_root = dir_root)

  data_expected <-
    build_downloads_expected_files(
      list_server = list_server,
      dir_data = list_server[["dir_server"]]
    )

  data_checks <-
    purrr::map(
      .x = dplyr::pull(data_expected, "path_file"),
      .f = ~ diagnose_raw_file(
        path_file = .x,
        expected_size_bytes = NA_real_,
        file_check = "geotiff"
      )
    ) |>
    purrr::list_rbind()

  testthat::expect_error(
    run_source_checksums(
      data_file_checks = dplyr::select(data_checks, -"flag_complete"),
      data_expected_files = data_expected,
      dir_records = dir_root,
      flag_record_checksums = TRUE,
      verbose = FALSE
    ),
    regexp = "data_file_checks"
  )

  testthat::expect_error(
    run_source_checksums(
      data_file_checks = data_checks,
      data_expected_files = data_expected,
      dir_records = dir_root,
      flag_record_checksums = "yes",
      verbose = FALSE
    ),
    regexp = "flag_record_checksums"
  )
})


#----------------------------------------------------------#
# Flag off -----
#----------------------------------------------------------#

testthat::test_that("run_source_checksums() does nothing without the flag", {
  dir_root <-
    withr::local_tempdir()

  list_server <-
    build_raw_source_fixture(dir_root = dir_root)

  data_expected <-
    build_downloads_expected_files(
      list_server = list_server,
      dir_data = list_server[["dir_server"]]
    )

  data_checks <-
    purrr::map(
      .x = dplyr::pull(data_expected, "path_file"),
      .f = ~ diagnose_raw_file(
        path_file = .x,
        expected_size_bytes = NA_real_,
        file_check = "geotiff"
      )
    ) |>
    purrr::list_rbind()

  dir_records <-
    base::file.path(dir_root, "Records")

  res <-
    run_source_checksums(
      data_file_checks = data_checks,
      data_expected_files = data_expected,
      dir_records = dir_records,
      flag_record_checksums = FALSE,
      verbose = FALSE
    )

  testthat::expect_named(res, vec_record_columns)
  testthat::expect_equal(base::nrow(res), 0L)
  testthat::expect_false(base::dir.exists(dir_records))
})


#----------------------------------------------------------#
# Flag on -----
#----------------------------------------------------------#

testthat::test_that("run_source_checksums() records unrecorded files only", {
  dir_root <-
    withr::local_tempdir()

  list_server <-
    build_raw_source_fixture(dir_root = dir_root)

  data_expected <-
    build_downloads_expected_files(
      list_server = list_server,
      dir_data = list_server[["dir_server"]]
    )

  data_checks <-
    purrr::map(
      .x = dplyr::pull(data_expected, "path_file"),
      .f = ~ diagnose_raw_file(
        path_file = .x,
        expected_size_bytes = NA_real_,
        file_check = "geotiff"
      )
    ) |>
    purrr::list_rbind()

  dir_records <-
    base::file.path(dir_root, "Records")

  base::dir.create(dir_records)

  readr::write_csv(
    x = tibble::tibble(
      product = "prod_neo",
      file_name = "neo_bio1.tif",
      url = list_server[["url_bio1"]],
      size_bytes = list_server[["size_bio1"]],
      sha256 = "recorded",
      downloaded_utc = "2026-11-20T13:00:00Z",
      recorded_utc = "2026-11-20T13:00:00Z",
      method = "download"
    ),
    file = base::file.path(dir_records, "download_record_seed.csv")
  )

  res <-
    run_source_checksums(
      data_file_checks = data_checks,
      data_expected_files = data_expected,
      dir_records = dir_records,
      flag_record_checksums = TRUE,
      verbose = FALSE
    )

  testthat::expect_named(res, vec_record_columns)
  testthat::expect_equal(dplyr::pull(res, "file_name"), "neo_bio12.tif")
  testthat::expect_equal(
    dplyr::pull(res, "sha256"),
    secretbase::sha256(file = list_server[["path_bio12"]])
  )
  testthat::expect_equal(dplyr::pull(res, "method"), "checksum")
  testthat::expect_true(base::is.na(dplyr::pull(res, "downloaded_utc")))
  testthat::expect_equal(dplyr::pull(res, "url"), list_server[["url_bio12"]])
  testthat::expect_equal(
    dplyr::pull(res, "size_bytes"),
    list_server[["size_bio12"]]
  )

  vec_records_new <-
    base::setdiff(base::list.files(dir_records), "download_record_seed.csv")

  testthat::expect_length(vec_records_new, 1L)
  testthat::expect_match(vec_records_new, "_checksum[.]csv$")
})

testthat::test_that("run_source_checksums() records a changed size again", {
  dir_root <-
    withr::local_tempdir()

  list_server <-
    build_raw_source_fixture(dir_root = dir_root)

  data_expected <-
    build_downloads_expected_files(
      list_server = list_server,
      dir_data = list_server[["dir_server"]]
    ) |>
    dplyr::slice(1)

  data_checks <-
    diagnose_raw_file(
      path_file = list_server[["path_bio1"]],
      expected_size_bytes = NA_real_,
      file_check = "geotiff"
    )

  dir_records <-
    withr::local_tempdir()

  readr::write_csv(
    x = tibble::tibble(
      product = "prod_neo",
      file_name = "neo_bio1.tif",
      url = list_server[["url_bio1"]],
      size_bytes = 1,
      sha256 = "old",
      downloaded_utc = NA_character_,
      recorded_utc = "2026-11-20T13:00:00Z",
      method = "checksum"
    ),
    file = base::file.path(dir_records, "download_record_seed.csv"),
    na = ""
  )

  res <-
    run_source_checksums(
      data_file_checks = data_checks,
      data_expected_files = data_expected,
      dir_records = dir_records,
      flag_record_checksums = TRUE,
      verbose = FALSE
    )

  testthat::expect_equal(dplyr::pull(res, "file_name"), "neo_bio1.tif")
})

testthat::test_that("run_source_checksums() never records incomplete files", {
  dir_root <-
    withr::local_tempdir()

  list_server <-
    build_raw_source_fixture(dir_root = dir_root)

  data_expected <-
    tibble::tibble(
      product = "prod_neo",
      file_name = "neo_truncated.tif",
      url = list_server[["url_truncated"]]
    )

  data_checks <-
    diagnose_raw_file(
      path_file = list_server[["path_truncated"]],
      expected_size_bytes = NA_real_,
      file_check = "geotiff"
    )

  dir_records <-
    withr::local_tempdir()

  res <-
    run_source_checksums(
      data_file_checks = data_checks,
      data_expected_files = data_expected,
      dir_records = dir_records,
      flag_record_checksums = TRUE,
      verbose = FALSE
    )

  testthat::expect_equal(base::nrow(res), 0L)
  testthat::expect_length(base::list.files(dir_records), 0L)
})

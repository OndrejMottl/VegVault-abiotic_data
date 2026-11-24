#----------------------------------------------------------#
# Fixtures -----
#----------------------------------------------------------#

vec_record_columns <-
  c(
    "product", "file_name", "url", "size_bytes", "sha256",
    "downloaded_utc", "recorded_utc", "method"
  )

data_record_first <-
  tibble::tibble(
    product = "prod_neo",
    file_name = c("neo_bio12.tif", "neo_bio1.tif"),
    url = "https://example.org/neo",
    size_bytes = c(200, 100),
    sha256 = c("bbb", "aaa"),
    downloaded_utc = "2026-11-20T13:00:00Z",
    recorded_utc = "2026-11-20T13:00:00Z",
    method = "download"
  )

data_record_second <-
  tibble::tibble(
    product = "prod_neo",
    file_name = "neo_bio1.tif",
    url = "https://example.org/neo",
    size_bytes = 101,
    sha256 = "ccc",
    downloaded_utc = NA_character_,
    recorded_utc = "2026-11-21T09:00:00Z",
    method = "checksum"
  )


#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("load_download_records() needs an existing folder", {
  testthat::expect_error(
    load_download_records(
      dir_records = base::file.path(withr::local_tempdir(), "none")
    ),
    regexp = "dir_records"
  )

  testthat::expect_error(
    load_download_records(dir_records = 1),
    regexp = "dir_records"
  )
})


#----------------------------------------------------------#
# Outputs -----
#----------------------------------------------------------#

testthat::test_that("load_download_records() returns no rows when empty", {
  res <-
    load_download_records(dir_records = withr::local_tempdir())

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(res, vec_record_columns)
  testthat::expect_equal(base::nrow(res), 0L)
  testthat::expect_type(dplyr::pull(res, "size_bytes"), "double")
  testthat::expect_type(dplyr::pull(res, "sha256"), "character")
})


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("load_download_records() binds and sorts records", {
  dir_records <-
    withr::local_tempdir()

  readr::write_csv(
    x = data_record_first,
    file = base::file.path(dir_records, "download_record_a.csv"),
    na = ""
  )

  readr::write_csv(
    x = data_record_second,
    file = base::file.path(dir_records, "download_record_b.csv"),
    na = ""
  )

  base::writeLines(
    text = "not a record",
    con = base::file.path(dir_records, "README.txt")
  )

  res <-
    load_download_records(dir_records = dir_records)

  testthat::expect_named(res, vec_record_columns)
  testthat::expect_equal(base::nrow(res), 3L)
  testthat::expect_equal(
    dplyr::pull(res, "file_name"),
    c("neo_bio1.tif", "neo_bio1.tif", "neo_bio12.tif")
  )
  testthat::expect_equal(dplyr::pull(res, "sha256"), c("aaa", "ccc", "bbb"))
  testthat::expect_equal(dplyr::pull(res, "size_bytes"), c(100, 101, 200))
  testthat::expect_true(
    base::is.na(dplyr::pull(res, "downloaded_utc")[2])
  )
  testthat::expect_type(dplyr::pull(res, "downloaded_utc"), "character")
})


#----------------------------------------------------------#
# Edge cases -----
#----------------------------------------------------------#

testthat::test_that("load_download_records() needs every column", {
  dir_records <-
    withr::local_tempdir()

  readr::write_csv(
    x = dplyr::select(data_record_first, -"sha256"),
    file = base::file.path(dir_records, "download_record_a.csv")
  )

  testthat::expect_error(
    load_download_records(dir_records = dir_records),
    regexp = "download_record_a.csv"
  )
})

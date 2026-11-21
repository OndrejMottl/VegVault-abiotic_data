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

testthat::test_that("run_source_downloads() validates its arguments", {
  dir_root <-
    withr::local_tempdir()

  list_server <-
    build_raw_source_fixture(dir_root = dir_root)

  data_expected <-
    build_downloads_expected_files(
      list_server = list_server,
      dir_data = base::file.path(dir_root, "Data")
    )

  testthat::expect_error(
    run_source_downloads(
      data_expected_files = dplyr::select(data_expected, -"url"),
      flag_download = FALSE,
      file_check = "geotiff",
      dir_download = base::file.path(dir_root, "Downloads"),
      dir_records = base::file.path(dir_root, "Records"),
      verbose = FALSE
    ),
    regexp = "data_expected_files"
  )

  testthat::expect_error(
    run_source_downloads(
      data_expected_files = data_expected,
      flag_download = NA,
      file_check = "geotiff",
      dir_download = base::file.path(dir_root, "Downloads"),
      dir_records = base::file.path(dir_root, "Records"),
      verbose = FALSE
    ),
    regexp = "flag_download"
  )
})


#----------------------------------------------------------#
# Flag off -----
#----------------------------------------------------------#

testthat::test_that("run_source_downloads() does nothing without the flag", {
  dir_root <-
    withr::local_tempdir()

  list_server <-
    build_raw_source_fixture(dir_root = dir_root)

  data_expected <-
    build_downloads_expected_files(
      list_server = list_server,
      dir_data = base::file.path(dir_root, "Data")
    ) |>
    dplyr::mutate(url = "https://invalid.example/none.tif")

  res <-
    run_source_downloads(
      data_expected_files = data_expected,
      flag_download = FALSE,
      file_check = "geotiff",
      dir_download = base::file.path(dir_root, "Downloads"),
      dir_records = base::file.path(dir_root, "Records"),
      verbose = FALSE
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(res, vec_record_columns)
  testthat::expect_equal(base::nrow(res), 0L)
  testthat::expect_false(base::dir.exists(base::file.path(dir_root, "Data")))
  testthat::expect_false(
    base::dir.exists(base::file.path(dir_root, "Records"))
  )
})


#----------------------------------------------------------#
# Flag on -----
#----------------------------------------------------------#

testthat::test_that("run_source_downloads() downloads and records", {
  dir_root <-
    withr::local_tempdir()

  list_server <-
    build_raw_source_fixture(dir_root = dir_root)

  dir_records <-
    base::file.path(dir_root, "Records")

  data_expected <-
    build_downloads_expected_files(
      list_server = list_server,
      dir_data = base::file.path(dir_root, "Data")
    )

  res <-
    run_source_downloads(
      data_expected_files = data_expected,
      flag_download = TRUE,
      file_check = "geotiff",
      dir_download = base::file.path(dir_root, "Downloads"),
      dir_records = dir_records,
      verbose = FALSE
    )

  testthat::expect_named(res, vec_record_columns)
  testthat::expect_equal(
    dplyr::pull(res, "file_name"),
    c("neo_bio1.tif", "neo_bio12.tif")
  )
  testthat::expect_true(
    base::all(base::file.exists(dplyr::pull(data_expected, "path_file")))
  )
  testthat::expect_true(base::all(dplyr::pull(res, "method") == "download"))
  testthat::expect_true(base::all(dplyr::pull(res, "product") == "prod_neo"))

  vec_records <-
    base::list.files(dir_records)

  testthat::expect_length(vec_records, 1L)
  testthat::expect_match(
    vec_records,
    "^download_record_prod_neo_[0-9-]{10}_[0-9]{6}_download[.]csv$"
  )

  data_record <-
    readr::read_csv(
      file = base::file.path(dir_records, vec_records),
      col_types = readr::cols(.default = readr::col_character())
    )

  testthat::expect_equal(
    dplyr::pull(data_record, "sha256"),
    dplyr::pull(res, "sha256")
  )
})

testthat::test_that("run_source_downloads() skips complete files", {
  dir_root <-
    withr::local_tempdir()

  list_server <-
    build_raw_source_fixture(dir_root = dir_root)

  dir_records <-
    base::file.path(dir_root, "Records")

  data_expected <-
    build_downloads_expected_files(
      list_server = list_server,
      dir_data = base::file.path(dir_root, "Data")
    )

  run_source_downloads(
    data_expected_files = data_expected,
    flag_download = TRUE,
    file_check = "geotiff",
    dir_download = base::file.path(dir_root, "Downloads"),
    dir_records = dir_records,
    verbose = FALSE
  )

  res <-
    run_source_downloads(
      data_expected_files = data_expected,
      flag_download = TRUE,
      file_check = "geotiff",
      dir_download = base::file.path(dir_root, "Downloads"),
      dir_records = dir_records,
      verbose = FALSE
    )

  testthat::expect_equal(base::nrow(res), 0L)
  testthat::expect_length(base::list.files(dir_records), 1L)
})


#----------------------------------------------------------#
# Failing closed -----
#----------------------------------------------------------#

testthat::test_that("run_source_downloads() keeps an incomplete file", {
  dir_root <-
    withr::local_tempdir()

  list_server <-
    build_raw_source_fixture(dir_root = dir_root)

  dir_data <-
    base::file.path(dir_root, "Data")

  base::dir.create(dir_data)

  base::writeLines(
    text = "cut",
    con = base::file.path(dir_data, "neo_bio12.tif")
  )

  testthat::expect_error(
    run_source_downloads(
      data_expected_files = build_downloads_expected_files(
        list_server = list_server,
        dir_data = dir_data
      ),
      flag_download = TRUE,
      file_check = "geotiff",
      dir_download = base::file.path(dir_root, "Downloads"),
      dir_records = base::file.path(dir_root, "Records"),
      verbose = FALSE
    ),
    regexp = "neo_bio12.tif"
  )

  testthat::expect_equal(
    base::readLines(base::file.path(dir_data, "neo_bio12.tif")),
    "cut"
  )
  testthat::expect_false(
    base::file.exists(base::file.path(dir_data, "neo_bio1.tif"))
  )
})

testthat::test_that("run_source_downloads() needs an address", {
  dir_root <-
    withr::local_tempdir()

  list_server <-
    build_raw_source_fixture(dir_root = dir_root)

  testthat::expect_error(
    run_source_downloads(
      data_expected_files = build_downloads_expected_files(
        list_server = list_server,
        dir_data = base::file.path(dir_root, "Data")
      ) |>
        dplyr::mutate(url = NA_character_),
      flag_download = TRUE,
      file_check = "geotiff",
      dir_download = base::file.path(dir_root, "Downloads"),
      dir_records = base::file.path(dir_root, "Records"),
      verbose = FALSE
    ),
    regexp = "address"
  )
})

testthat::test_that("run_source_downloads() records files before a failure", {
  dir_root <-
    withr::local_tempdir()

  list_server <-
    build_raw_source_fixture(dir_root = dir_root)

  dir_records <-
    base::file.path(dir_root, "Records")

  data_expected <-
    build_downloads_expected_files(
      list_server = list_server,
      dir_data = base::file.path(dir_root, "Data")
    ) |>
    dplyr::mutate(
      url = c(list_server[["url_bio1"]], list_server[["url_truncated"]]),
      expected_size_bytes = c(list_server[["size_bio1"]], NA_real_)
    )

  testthat::expect_error(
    run_source_downloads(
      data_expected_files = data_expected,
      flag_download = TRUE,
      file_check = "geotiff",
      dir_download = base::file.path(dir_root, "Downloads"),
      dir_records = dir_records,
      verbose = FALSE
    ),
    regexp = "neo_bio12.tif"
  )

  vec_records <-
    base::list.files(dir_records, full.names = TRUE)

  testthat::expect_length(vec_records, 1L)

  data_record <-
    readr::read_csv(
      file = vec_records,
      col_types = readr::cols(.default = readr::col_character())
    )

  testthat::expect_equal(dplyr::pull(data_record, "file_name"), "neo_bio1.tif")
})

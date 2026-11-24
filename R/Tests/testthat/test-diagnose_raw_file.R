#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("diagnose_raw_file() validates its arguments", {
  testthat::expect_error(
    diagnose_raw_file(
      path_file = 1,
      expected_size_bytes = NA_real_,
      file_check = "zip"
    ),
    regexp = "path_file"
  )

  testthat::expect_error(
    diagnose_raw_file(
      path_file = "a.zip",
      expected_size_bytes = "100",
      file_check = "zip"
    ),
    regexp = "expected_size_bytes"
  )

  testthat::expect_error(
    diagnose_raw_file(
      path_file = "a.zip",
      expected_size_bytes = NA_real_,
      file_check = "netcdf"
    ),
    regexp = "file_check"
  )
})


#----------------------------------------------------------#
# Outputs -----
#----------------------------------------------------------#

testthat::test_that("diagnose_raw_file() returns one row with columns", {
  list_server <-
    build_raw_source_fixture(dir_root = withr::local_tempdir())

  res <-
    diagnose_raw_file(
      path_file = list_server[["path_bio1"]],
      expected_size_bytes = list_server[["size_bio1"]],
      file_check = "geotiff"
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_equal(base::nrow(res), 1L)
  testthat::expect_named(
    res,
    c(
      "file_name", "path_file", "flag_present", "size_bytes",
      "flag_size_ok", "flag_content_ok", "flag_complete", "detail"
    )
  )
  testthat::expect_equal(dplyr::pull(res, "file_name"), "neo_bio1.tif")
  testthat::expect_equal(
    dplyr::pull(res, "path_file"),
    list_server[["path_bio1"]]
  )
})


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("diagnose_raw_file() passes a complete GeoTIFF", {
  list_server <-
    build_raw_source_fixture(dir_root = withr::local_tempdir())

  res <-
    diagnose_raw_file(
      path_file = list_server[["path_bio1"]],
      expected_size_bytes = list_server[["size_bio1"]],
      file_check = "geotiff"
    )

  testthat::expect_true(dplyr::pull(res, "flag_present"))
  testthat::expect_equal(
    dplyr::pull(res, "size_bytes"),
    list_server[["size_bio1"]]
  )
  testthat::expect_true(dplyr::pull(res, "flag_size_ok"))
  testthat::expect_true(dplyr::pull(res, "flag_content_ok"))
  testthat::expect_true(dplyr::pull(res, "flag_complete"))
  testthat::expect_equal(dplyr::pull(res, "detail"), "")
})

testthat::test_that("diagnose_raw_file() fails a wrong size", {
  list_server <-
    build_raw_source_fixture(dir_root = withr::local_tempdir())

  res <-
    diagnose_raw_file(
      path_file = list_server[["path_bio1"]],
      expected_size_bytes = list_server[["size_bio1"]] + 10,
      file_check = "geotiff"
    )

  testthat::expect_false(dplyr::pull(res, "flag_size_ok"))
  testthat::expect_true(dplyr::pull(res, "flag_content_ok"))
  testthat::expect_false(dplyr::pull(res, "flag_complete"))
  testthat::expect_match(dplyr::pull(res, "detail"), "bytes")
})

testthat::test_that("diagnose_raw_file() skips the size rule without size", {
  list_server <-
    build_raw_source_fixture(dir_root = withr::local_tempdir())

  res <-
    diagnose_raw_file(
      path_file = list_server[["path_bio1"]],
      expected_size_bytes = NA_real_,
      file_check = "geotiff"
    )

  testthat::expect_true(base::is.na(dplyr::pull(res, "flag_size_ok")))
  testthat::expect_true(dplyr::pull(res, "flag_complete"))
})

testthat::test_that("diagnose_raw_file() fails a truncated GeoTIFF", {
  list_server <-
    build_raw_source_fixture(dir_root = withr::local_tempdir())

  res <-
    diagnose_raw_file(
      path_file = list_server[["path_truncated"]],
      expected_size_bytes = NA_real_,
      file_check = "geotiff"
    )

  testthat::expect_true(dplyr::pull(res, "flag_present"))
  testthat::expect_false(dplyr::pull(res, "flag_content_ok"))
  testthat::expect_false(dplyr::pull(res, "flag_complete"))
  testthat::expect_match(dplyr::pull(res, "detail"), "beyond")
})

testthat::test_that("diagnose_raw_file() reports a missing file", {
  res <-
    diagnose_raw_file(
      path_file = base::file.path(withr::local_tempdir(), "none.tif"),
      expected_size_bytes = 100,
      file_check = "geotiff"
    )

  testthat::expect_false(dplyr::pull(res, "flag_present"))
  testthat::expect_true(base::is.na(dplyr::pull(res, "size_bytes")))
  testthat::expect_true(base::is.na(dplyr::pull(res, "flag_size_ok")))
  testthat::expect_true(base::is.na(dplyr::pull(res, "flag_content_ok")))
  testthat::expect_false(dplyr::pull(res, "flag_complete"))
  testthat::expect_match(dplyr::pull(res, "detail"), "missing")
})

testthat::test_that("diagnose_raw_file() checks zip archives", {
  dir_root <-
    withr::local_tempdir()

  list_server <-
    build_raw_source_fixture(dir_root = dir_root)

  res_ok <-
    diagnose_raw_file(
      path_file = list_server[["path_archive"]],
      expected_size_bytes = NA_real_,
      file_check = "zip"
    )

  testthat::expect_true(dplyr::pull(res_ok, "flag_content_ok"))
  testthat::expect_true(dplyr::pull(res_ok, "flag_complete"))

  # A zip cut short loses its central directory at the end of the file
  path_cut <-
    base::file.path(dir_root, "cut.zip")

  base::writeBin(
    object = base::readBin(
      con = list_server[["path_archive"]],
      what = "raw",
      n = list_server[["size_archive"]] - 30
    ),
    con = path_cut
  )

  res_cut <-
    diagnose_raw_file(
      path_file = path_cut,
      expected_size_bytes = NA_real_,
      file_check = "zip"
    )

  testthat::expect_false(dplyr::pull(res_cut, "flag_content_ok"))
  testthat::expect_false(dplyr::pull(res_cut, "flag_complete"))
  testthat::expect_match(dplyr::pull(res_cut, "detail"), "archive")

  res_not_zip <-
    diagnose_raw_file(
      path_file = list_server[["path_bio1"]],
      expected_size_bytes = NA_real_,
      file_check = "zip"
    )

  testthat::expect_false(dplyr::pull(res_not_zip, "flag_content_ok"))
})

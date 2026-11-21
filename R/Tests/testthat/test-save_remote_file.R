#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("save_remote_file() validates its arguments", {
  dir_root <-
    withr::local_tempdir()

  list_server <-
    build_raw_source_fixture(dir_root = dir_root)

  testthat::expect_error(
    save_remote_file(
      url = 1,
      path_file = base::file.path(dir_root, "a.tif"),
      expected_size_bytes = NA_real_,
      file_check = "geotiff",
      dir_download = base::file.path(dir_root, "Downloads"),
      verbose = FALSE
    ),
    regexp = "url"
  )

  testthat::expect_error(
    save_remote_file(
      url = list_server[["url_bio1"]],
      path_file = base::file.path(dir_root, "a.tif"),
      expected_size_bytes = NA_real_,
      file_check = "netcdf",
      dir_download = base::file.path(dir_root, "Downloads"),
      verbose = FALSE
    ),
    regexp = "file_check"
  )

  testthat::expect_error(
    save_remote_file(
      url = list_server[["url_bio1"]],
      path_file = base::file.path(dir_root, "a.tif"),
      expected_size_bytes = NA_real_,
      file_check = "geotiff",
      dir_download = 1,
      verbose = FALSE
    ),
    regexp = "dir_download"
  )
})

testthat::test_that("save_remote_file() never overwrites a file", {
  dir_root <-
    withr::local_tempdir()

  list_server <-
    build_raw_source_fixture(dir_root = dir_root)

  path_existing <-
    base::file.path(dir_root, "existing.tif")

  base::writeLines(text = "old", con = path_existing)

  testthat::expect_error(
    save_remote_file(
      url = list_server[["url_bio1"]],
      path_file = path_existing,
      expected_size_bytes = list_server[["size_bio1"]],
      file_check = "geotiff",
      dir_download = base::file.path(dir_root, "Downloads"),
      verbose = FALSE
    ),
    regexp = "exists"
  )

  testthat::expect_equal(base::readLines(path_existing), "old")
})


#----------------------------------------------------------#
# Outputs -----
#----------------------------------------------------------#

testthat::test_that("save_remote_file() saves a complete file", {
  dir_root <-
    withr::local_tempdir()

  list_server <-
    build_raw_source_fixture(dir_root = dir_root)

  path_target <-
    base::file.path(dir_root, "Input", "Neo", "neo_bio1.tif")

  dir_download <-
    base::file.path(dir_root, "Downloads")

  res <-
    save_remote_file(
      url = list_server[["url_bio1"]],
      path_file = path_target,
      expected_size_bytes = list_server[["size_bio1"]],
      file_check = "geotiff",
      dir_download = dir_download,
      verbose = FALSE
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(
    res,
    c("file_name", "url", "size_bytes", "sha256", "downloaded_utc")
  )
  testthat::expect_true(base::file.exists(path_target))
  testthat::expect_equal(
    base::file.size(path_target),
    list_server[["size_bio1"]]
  )
  testthat::expect_equal(dplyr::pull(res, "file_name"), "neo_bio1.tif")
  testthat::expect_equal(dplyr::pull(res, "url"), list_server[["url_bio1"]])
  testthat::expect_equal(
    dplyr::pull(res, "sha256"),
    secretbase::sha256(file = list_server[["path_bio1"]])
  )
  testthat::expect_match(
    dplyr::pull(res, "downloaded_utc"),
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}Z$"
  )
  testthat::expect_length(base::list.files(dir_download), 0L)
})

testthat::test_that("save_remote_file() replaces a leftover part file", {
  dir_root <-
    withr::local_tempdir()

  list_server <-
    build_raw_source_fixture(dir_root = dir_root)

  dir_download <-
    base::file.path(dir_root, "Downloads")

  base::dir.create(dir_download)

  base::writeLines(
    text = "interrupted",
    con = base::file.path(dir_download, "neo_bio1.tif.part")
  )

  path_target <-
    base::file.path(dir_root, "neo_bio1.tif")

  save_remote_file(
    url = list_server[["url_bio1"]],
    path_file = path_target,
    expected_size_bytes = list_server[["size_bio1"]],
    file_check = "geotiff",
    dir_download = dir_download,
    verbose = FALSE
  )

  testthat::expect_equal(
    base::file.size(path_target),
    list_server[["size_bio1"]]
  )
})


#----------------------------------------------------------#
# Failing closed -----
#----------------------------------------------------------#

testthat::test_that("save_remote_file() rejects a wrong size", {
  dir_root <-
    withr::local_tempdir()

  list_server <-
    build_raw_source_fixture(dir_root = dir_root)

  path_target <-
    base::file.path(dir_root, "neo_bio1.tif")

  dir_download <-
    base::file.path(dir_root, "Downloads")

  testthat::expect_error(
    save_remote_file(
      url = list_server[["url_bio1"]],
      path_file = path_target,
      expected_size_bytes = list_server[["size_bio1"]] + 1,
      file_check = "geotiff",
      dir_download = dir_download,
      verbose = FALSE
    ),
    regexp = "incomplete"
  )

  testthat::expect_false(base::file.exists(path_target))
  testthat::expect_length(base::list.files(dir_download), 0L)
})

testthat::test_that("save_remote_file() rejects a truncated file", {
  dir_root <-
    withr::local_tempdir()

  list_server <-
    build_raw_source_fixture(dir_root = dir_root)

  path_target <-
    base::file.path(dir_root, "neo_truncated.tif")

  testthat::expect_error(
    save_remote_file(
      url = list_server[["url_truncated"]],
      path_file = path_target,
      expected_size_bytes = NA_real_,
      file_check = "geotiff",
      dir_download = base::file.path(dir_root, "Downloads"),
      verbose = FALSE
    ),
    regexp = "incomplete"
  )

  testthat::expect_false(base::file.exists(path_target))
})

testthat::test_that("save_remote_file() fails on an unreachable address", {
  dir_root <-
    withr::local_tempdir()

  path_target <-
    base::file.path(dir_root, "none.tif")

  testthat::expect_error(
    save_remote_file(
      url = stringr::str_c(
        "file:///",
        stringr::str_remove(
          base::normalizePath(dir_root, winslash = "/"),
          "^/"
        ),
        "/Server/none.tif"
      ),
      path_file = path_target,
      expected_size_bytes = 100,
      file_check = "geotiff",
      dir_download = base::file.path(dir_root, "Downloads"),
      verbose = FALSE
    ),
    regexp = "none.tif"
  )

  testthat::expect_false(base::file.exists(path_target))
})


#----------------------------------------------------------#
# Messages -----
#----------------------------------------------------------#

testthat::test_that("save_remote_file() reports progress when verbose", {
  dir_root <-
    withr::local_tempdir()

  list_server <-
    build_raw_source_fixture(dir_root = dir_root)

  testthat::expect_message(
    save_remote_file(
      url = list_server[["url_bio1"]],
      path_file = base::file.path(dir_root, "a", "neo_bio1.tif"),
      expected_size_bytes = list_server[["size_bio1"]],
      file_check = "geotiff",
      dir_download = base::file.path(dir_root, "Downloads"),
      verbose = TRUE
    ),
    regexp = "neo_bio1.tif"
  )

  testthat::expect_no_message(
    save_remote_file(
      url = list_server[["url_bio1"]],
      path_file = base::file.path(dir_root, "b", "neo_bio1.tif"),
      expected_size_bytes = list_server[["size_bio1"]],
      file_check = "geotiff",
      dir_download = base::file.path(dir_root, "Downloads"),
      verbose = FALSE
    )
  )
})

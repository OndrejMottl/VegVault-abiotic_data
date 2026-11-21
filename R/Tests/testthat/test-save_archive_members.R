#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("save_archive_members() validates its arguments", {
  dir_root <-
    withr::local_tempdir()

  list_server <-
    build_raw_source_fixture(dir_root = dir_root)

  data_raw_files <-
    tibble::tibble(
      file_name = "archive.zip",
      path_file = list_server[["path_archive"]]
    )

  testthat::expect_error(
    save_archive_members(
      data_raw_files = dplyr::select(data_raw_files, -"path_file"),
      list_archive_members = base::list(archive.zip = "db.mdb"),
      dir_extract = base::file.path(dir_root, "Extract"),
      verbose = FALSE
    ),
    regexp = "data_raw_files"
  )

  testthat::expect_error(
    save_archive_members(
      data_raw_files = data_raw_files,
      list_archive_members = base::list("db.mdb"),
      dir_extract = base::file.path(dir_root, "Extract"),
      verbose = FALSE
    ),
    regexp = "list_archive_members"
  )

  testthat::expect_error(
    save_archive_members(
      data_raw_files = data_raw_files,
      list_archive_members = base::list(other.zip = "db.mdb"),
      dir_extract = base::file.path(dir_root, "Extract"),
      verbose = FALSE
    ),
    regexp = "other.zip"
  )
})


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("save_archive_members() extracts the listed members", {
  dir_root <-
    withr::local_tempdir()

  list_server <-
    build_raw_source_fixture(dir_root = dir_root)

  dir_extract <-
    base::file.path(dir_root, "Extract")

  res <-
    save_archive_members(
      data_raw_files = tibble::tibble(
        file_name = "archive.zip",
        path_file = list_server[["path_archive"]]
      ),
      list_archive_members = base::list(
        archive.zip = c("raster.bil", "db.mdb")
      ),
      dir_extract = dir_extract,
      verbose = FALSE
    )

  testthat::expect_type(res, "character")
  testthat::expect_equal(
    res,
    base::file.path(dir_extract, c("raster.bil", "db.mdb"))
  )
  testthat::expect_equal(
    base::file.size(res),
    list_server[["vec_member_sizes"]][c(2, 1)]
  )
})

testthat::test_that("save_archive_members() keeps a member of right size", {
  dir_root <-
    withr::local_tempdir()

  list_server <-
    build_raw_source_fixture(dir_root = dir_root)

  dir_extract <-
    base::file.path(dir_root, "Extract")

  data_raw_files <-
    tibble::tibble(
      file_name = "archive.zip",
      path_file = list_server[["path_archive"]]
    )

  save_archive_members(
    data_raw_files = data_raw_files,
    list_archive_members = base::list(archive.zip = "db.mdb"),
    dir_extract = dir_extract,
    verbose = FALSE
  )

  path_member <-
    base::file.path(dir_extract, "db.mdb")

  base::Sys.setFileTime(path = path_member, time = "2020-01-01 00:00:00")

  time_before <-
    base::file.mtime(path_member)

  res <-
    save_archive_members(
      data_raw_files = data_raw_files,
      list_archive_members = base::list(archive.zip = "db.mdb"),
      dir_extract = dir_extract,
      verbose = FALSE
    )

  testthat::expect_equal(res, path_member)
  testthat::expect_equal(base::file.mtime(path_member), time_before)
})


#----------------------------------------------------------#
# Failing closed -----
#----------------------------------------------------------#

testthat::test_that("save_archive_members() never overwrites a member", {
  dir_root <-
    withr::local_tempdir()

  list_server <-
    build_raw_source_fixture(dir_root = dir_root)

  dir_extract <-
    base::file.path(dir_root, "Extract")

  base::dir.create(dir_extract)

  base::writeLines(
    text = "different content of another length",
    con = base::file.path(dir_extract, "db.mdb")
  )

  testthat::expect_error(
    save_archive_members(
      data_raw_files = tibble::tibble(
        file_name = "archive.zip",
        path_file = list_server[["path_archive"]]
      ),
      list_archive_members = base::list(archive.zip = "db.mdb"),
      dir_extract = dir_extract,
      verbose = FALSE
    ),
    regexp = "db.mdb"
  )

  testthat::expect_equal(
    base::readLines(base::file.path(dir_extract, "db.mdb")),
    "different content of another length"
  )
})

testthat::test_that("save_archive_members() needs listed members", {
  dir_root <-
    withr::local_tempdir()

  list_server <-
    build_raw_source_fixture(dir_root = dir_root)

  testthat::expect_error(
    save_archive_members(
      data_raw_files = tibble::tibble(
        file_name = "archive.zip",
        path_file = list_server[["path_archive"]]
      ),
      list_archive_members = base::list(archive.zip = "none.txt"),
      dir_extract = base::file.path(dir_root, "Extract"),
      verbose = FALSE
    ),
    regexp = "none.txt"
  )
})

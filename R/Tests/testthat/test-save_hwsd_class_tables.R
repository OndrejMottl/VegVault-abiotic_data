#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("save_hwsd_class_tables() validates its arguments", {
  dir_root <-
    withr::local_tempdir()

  testthat::expect_error(
    save_hwsd_class_tables(
      vec_extracted_files = base::file.path(dir_root, "HWSD2.2.mdb"),
      dir_tables = base::file.path(dir_root, "Tables"),
      flag_read_access_database = NA,
      verbose = FALSE
    ),
    regexp = "flag_read_access_database"
  )

  testthat::expect_error(
    save_hwsd_class_tables(
      vec_extracted_files = base::file.path(dir_root, "HWSD2.2.mdb"),
      dir_tables = 1,
      flag_read_access_database = FALSE,
      verbose = FALSE
    ),
    regexp = "dir_tables"
  )
})


#----------------------------------------------------------#
# Flag off -----
#----------------------------------------------------------#

testthat::test_that("save_hwsd_class_tables() does nothing without the flag", {
  dir_root <-
    withr::local_tempdir()

  dir_tables <-
    base::file.path(dir_root, "Tables")

  res <-
    save_hwsd_class_tables(
      vec_extracted_files = base::file.path(dir_root, "HWSD2.bil"),
      dir_tables = dir_tables,
      flag_read_access_database = FALSE,
      verbose = FALSE
    )

  testthat::expect_type(res, "character")
  testthat::expect_length(res, 0L)
  testthat::expect_false(base::dir.exists(dir_tables))
})


#----------------------------------------------------------#
# Failing closed -----
#----------------------------------------------------------#

testthat::test_that("save_hwsd_class_tables() needs the database file", {
  dir_root <-
    withr::local_tempdir()

  testthat::expect_error(
    save_hwsd_class_tables(
      vec_extracted_files = base::file.path(dir_root, "HWSD2.bil"),
      dir_tables = base::file.path(dir_root, "Tables"),
      flag_read_access_database = TRUE,
      verbose = FALSE
    ),
    regexp = "[.]mdb"
  )
})

testthat::test_that("save_hwsd_class_tables() never overwrites a table", {
  dir_root <-
    withr::local_tempdir()

  path_database <-
    base::file.path(dir_root, "HWSD2.2.mdb")

  base::writeLines(text = "not a database", con = path_database)

  dir_tables <-
    base::file.path(dir_root, "Tables")

  base::dir.create(dir_tables)

  base::writeLines(
    text = "kept",
    con = base::file.path(dir_tables, "d_wrb2.csv")
  )

  testthat::expect_error(
    save_hwsd_class_tables(
      vec_extracted_files = path_database,
      dir_tables = dir_tables,
      flag_read_access_database = TRUE,
      verbose = FALSE
    ),
    regexp = "d_wrb2.csv"
  )

  testthat::expect_equal(
    base::readLines(base::file.path(dir_tables, "d_wrb2.csv")),
    "kept"
  )
})

testthat::test_that("save_hwsd_class_tables() names the driver on failure", {
  dir_root <-
    withr::local_tempdir()

  path_database <-
    base::file.path(dir_root, "HWSD2.2.mdb")

  base::writeLines(text = "not a database", con = path_database)

  testthat::expect_error(
    save_hwsd_class_tables(
      vec_extracted_files = path_database,
      dir_tables = base::file.path(dir_root, "Tables"),
      flag_read_access_database = TRUE,
      verbose = FALSE
    ),
    regexp = "Access"
  )
})

#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("validate_hwsd_class_tables() validates its arguments", {
  testthat::expect_error(
    validate_hwsd_class_tables(dir_tables = 1),
    regexp = "dir_tables"
  )

  testthat::expect_error(
    validate_hwsd_class_tables(
      dir_tables = withr::local_tempdir(),
      vec_file_names = base::character(0)
    ),
    regexp = "vec_file_names"
  )
})


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("validate_hwsd_class_tables() returns the table paths", {
  dir_tables <-
    withr::local_tempdir()

  vec_file_names <-
    c("hwsd2_smu.csv", "d_wrb2.csv", "d_wrb2code.csv")

  purrr::walk(
    .x = vec_file_names,
    .f = ~ base::writeLines(
      text = "x",
      con = base::file.path(dir_tables, .x)
    )
  )

  res <-
    validate_hwsd_class_tables(dir_tables = dir_tables)

  testthat::expect_type(res, "character")
  testthat::expect_equal(res, base::file.path(dir_tables, vec_file_names))
})


#----------------------------------------------------------#
# Failing closed -----
#----------------------------------------------------------#

testthat::test_that("validate_hwsd_class_tables() names the flag", {
  dir_tables <-
    withr::local_tempdir()

  base::writeLines(
    text = "x",
    con = base::file.path(dir_tables, "hwsd2_smu.csv")
  )

  list_condition <-
    rlang::catch_cnd(
      expr = validate_hwsd_class_tables(dir_tables = dir_tables),
      classes = "error"
    )

  testthat::expect_s3_class(list_condition, "error")

  text_message <-
    rlang::cnd_message(list_condition)

  testthat::expect_match(text_message, "d_wrb2.csv")
  testthat::expect_match(text_message, "flag_read_access_database")
  testthat::expect_match(text_message, "Access")

  testthat::expect_error(
    validate_hwsd_class_tables(
      dir_tables = base::file.path(dir_tables, "none")
    ),
    regexp = "flag_read_access_database"
  )
})

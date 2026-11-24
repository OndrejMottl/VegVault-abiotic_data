#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("validate_export_folder() validates its inputs", {
  dir_output <-
    withr::local_tempdir()

  dir_other <-
    withr::local_tempdir()

  vec_paths <-
    base::file.path(dir_output, c("a.qs", "b.qs"))

  base::file.create(vec_paths)
  base::file.create(base::file.path(dir_other, "c.qs"))

  testthat::expect_error(
    validate_export_folder(vec_paths_export = 1),
    regexp = "vec_paths_export"
  )
  testthat::expect_error(
    validate_export_folder(vec_paths_export = base::character(0)),
    regexp = "vec_paths_export"
  )
  testthat::expect_error(
    validate_export_folder(vec_paths_export = c(vec_paths, NA)),
    regexp = "vec_paths_export"
  )
  testthat::expect_error(
    validate_export_folder(vec_paths_export = c(vec_paths, vec_paths[1])),
    regexp = "vec_paths_export"
  )
  testthat::expect_error(
    validate_export_folder(
      vec_paths_export = c(vec_paths, base::file.path(dir_other, "c.qs"))
    ),
    regexp = "one folder"
  )
  testthat::expect_error(
    validate_export_folder(
      vec_paths_export = base::file.path(dir_output, "Missing", "a.qs")
    ),
    regexp = "folder"
  )
})


#----------------------------------------------------------#
# Outputs -----
#----------------------------------------------------------#

testthat::test_that("validate_export_folder() passes an exact folder", {
  dir_output <-
    withr::local_tempdir()

  vec_paths <-
    base::file.path(dir_output, c("a.qs", "b.qs"))

  base::file.create(vec_paths)

  res <-
    validate_export_folder(vec_paths_export = vec_paths)

  testthat::expect_equal(res, vec_paths)
})


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("validate_export_folder() names extra files", {
  dir_output <-
    withr::local_tempdir()

  vec_paths <-
    base::file.path(dir_output, c("a.qs", "b.qs"))

  base::file.create(vec_paths)
  base::file.create(base::file.path(dir_output, "old_batch_5.qs"))

  testthat::expect_error(
    validate_export_folder(vec_paths_export = vec_paths),
    regexp = "old_batch_5.qs"
  )
})

testthat::test_that("validate_export_folder() counts every entry", {
  dir_output <-
    withr::local_tempdir()

  vec_paths <-
    base::file.path(dir_output, "a.qs")

  base::file.create(vec_paths)
  base::file.create(base::file.path(dir_output, "a.qs.part"))

  testthat::expect_error(
    validate_export_folder(vec_paths_export = vec_paths),
    regexp = "a.qs.part"
  )

  base::file.remove(base::file.path(dir_output, "a.qs.part"))
  base::file.create(base::file.path(dir_output, ".hidden"))

  testthat::expect_error(
    validate_export_folder(vec_paths_export = vec_paths),
    regexp = ".hidden"
  )

  base::file.remove(base::file.path(dir_output, ".hidden"))
  base::dir.create(base::file.path(dir_output, "Sub"))

  testthat::expect_error(
    validate_export_folder(vec_paths_export = vec_paths),
    regexp = "Sub"
  )
})

testthat::test_that("validate_export_folder() names missing files", {
  dir_output <-
    withr::local_tempdir()

  vec_paths <-
    base::file.path(dir_output, c("a.qs", "b.qs"))

  base::file.create(vec_paths[1])

  testthat::expect_error(
    validate_export_folder(vec_paths_export = vec_paths),
    regexp = "b.qs"
  )
})

testthat::test_that("validate_export_folder() removes nothing", {
  dir_output <-
    withr::local_tempdir()

  vec_paths <-
    base::file.path(dir_output, "a.qs")

  base::file.create(vec_paths)
  base::file.create(base::file.path(dir_output, "extra.qs"))

  testthat::expect_error(
    validate_export_folder(vec_paths_export = vec_paths)
  )
  testthat::expect_setequal(
    base::list.files(dir_output),
    c("a.qs", "extra.qs")
  )
})

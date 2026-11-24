#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("build_crew_controller() validates n_workers", {
  testthat::expect_error(
    build_crew_controller(n_workers = 0),
    regexp = "n_workers"
  )

  testthat::expect_error(
    build_crew_controller(n_workers = 2.5),
    regexp = "n_workers"
  )

  testthat::expect_error(
    build_crew_controller(n_workers = NA_real_),
    regexp = "n_workers"
  )

  testthat::expect_error(
    build_crew_controller(n_workers = c(1, 2)),
    regexp = "n_workers"
  )

  testthat::expect_error(
    build_crew_controller(n_workers = "8"),
    regexp = "n_workers"
  )
})

testthat::test_that("build_crew_controller() validates seconds_idle", {
  testthat::expect_error(
    build_crew_controller(n_workers = 2, seconds_idle = 0),
    regexp = "seconds_idle"
  )

  testthat::expect_error(
    build_crew_controller(n_workers = 2, seconds_idle = NA_real_),
    regexp = "seconds_idle"
  )
})


#----------------------------------------------------------#
# Outputs -----
#----------------------------------------------------------#

testthat::test_that("build_crew_controller() is NULL for one worker", {
  testthat::expect_null(build_crew_controller(n_workers = 1))

  testthat::expect_null(build_crew_controller(n_workers = 1L))
})

testthat::test_that("build_crew_controller() builds a local controller", {
  res <-
    build_crew_controller(
      n_workers = 3,
      seconds_idle = 10
    )

  testthat::expect_s3_class(res, "crew_class_controller")

  testthat::expect_equal(
    res[["launcher"]][["workers"]],
    3L
  )

  testthat::expect_equal(
    res[["launcher"]][["seconds_idle"]],
    10
  )

  testthat::expect_true(res[["garbage_collection"]])

  testthat::expect_false(res[["started"]]())
})

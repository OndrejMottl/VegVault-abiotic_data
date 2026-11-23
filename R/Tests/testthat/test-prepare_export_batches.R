#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("prepare_export_batches() validates its inputs", {
  data_input <-
    tibble::tibble(x = 1:8)

  testthat::expect_error(
    prepare_export_batches(data_export = 1:8, n_batches = 4),
    regexp = "data frame"
  )
  testthat::expect_error(
    prepare_export_batches(data_export = data_input, n_batches = 0),
    regexp = "n_batches"
  )
  testthat::expect_error(
    prepare_export_batches(data_export = data_input, n_batches = 1.5),
    regexp = "n_batches"
  )
  testthat::expect_error(
    prepare_export_batches(data_export = data_input, n_batches = NA),
    regexp = "n_batches"
  )
  testthat::expect_error(
    prepare_export_batches(data_export = data_input, n_batches = "4"),
    regexp = "n_batches"
  )
  testthat::expect_error(
    prepare_export_batches(data_export = data_input, n_batches = c(2, 4)),
    regexp = "n_batches"
  )
})


#----------------------------------------------------------#
# Outputs -----
#----------------------------------------------------------#

testthat::test_that("prepare_export_batches() returns a list of tables", {
  res <-
    prepare_export_batches(
      data_export = tibble::tibble(x = 1:8),
      n_batches = 4
    )

  testthat::expect_type(res, "list")
  testthat::expect_length(res, 4L)
  testthat::expect_true(
    base::all(purrr::map_lgl(.x = res, .f = ~ base::is.data.frame(.x)))
  )
})


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("prepare_export_batches() splits rows evenly", {
  res <-
    prepare_export_batches(
      data_export = tibble::tibble(x = 1:8),
      n_batches = 4
    )

  testthat::expect_equal(
    purrr::map(.x = res, .f = ~ dplyr::pull(.x, "x")),
    base::list(1:2, 3:4, 5:6, 7:8)
  )
})

testthat::test_that("prepare_export_batches() puts the rest in the last", {
  data_input <-
    tibble::tibble(x = 1:10, y = base::letters[1:10])

  res <-
    prepare_export_batches(data_export = data_input, n_batches = 4)

  testthat::expect_equal(
    purrr::map_int(.x = res, .f = ~ base::nrow(.x)),
    c(3L, 3L, 3L, 1L)
  )
  testthat::expect_equal(dplyr::bind_rows(res), data_input)
})

testthat::test_that("prepare_export_batches() keeps one batch whole", {
  data_input <-
    tibble::tibble(x = 1:5)

  res <-
    prepare_export_batches(data_export = data_input, n_batches = 1)

  testthat::expect_length(res, 1L)
  testthat::expect_equal(res[[1]], data_input)
})

testthat::test_that("prepare_export_batches() keeps order in large tables", {
  data_input <-
    tibble::tibble(x = base::seq_len(10001))

  res <-
    prepare_export_batches(data_export = data_input, n_batches = 4)

  testthat::expect_equal(
    purrr::map_int(.x = res, .f = ~ base::nrow(.x)),
    c(2501L, 2501L, 2501L, 2498L)
  )
  testthat::expect_equal(dplyr::bind_rows(res), data_input)
})


#----------------------------------------------------------#
# Edge cases -----
#----------------------------------------------------------#

testthat::test_that("prepare_export_batches() stops on an empty batch", {
  testthat::expect_error(
    prepare_export_batches(
      data_export = tibble::tibble(x = 1:5),
      n_batches = 4
    ),
    regexp = "empty"
  )
  testthat::expect_error(
    prepare_export_batches(
      data_export = tibble::tibble(x = base::integer(0)),
      n_batches = 1
    ),
    regexp = "empty"
  )
})

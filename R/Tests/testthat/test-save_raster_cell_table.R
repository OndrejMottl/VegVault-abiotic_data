#----------------------------------------------------------#
# Fixtures -----
#----------------------------------------------------------#

mat_cell_values <-
  base::matrix(
    c(1, NA, 3, 4, NA, 6, 7, NA),
    nrow = 2,
    ncol = 4,
    byrow = TRUE
  )


#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("save_raster_cell_table() validates its arguments", {
  dir_root <-
    withr::local_tempdir()

  path_input <-
    base::file.path(dir_root, "values.tif")

  build_quality_raster_fixture(
    path_file = path_input,
    mat_values = mat_cell_values
  )

  path_output <-
    base::file.path(dir_root, "values.qs2")

  testthat::expect_error(
    save_raster_cell_table(
      path_input = base::file.path(dir_root, "none.tif"),
      path_output = path_output,
      data_constants = tibble::tibble(time_id = 20L),
      vec_columns = c("long", "lat", "time_id", "value")
    ),
    regexp = "path_input"
  )

  testthat::expect_error(
    save_raster_cell_table(
      path_input = path_input,
      path_output = path_input,
      data_constants = tibble::tibble(time_id = 20L),
      vec_columns = c("long", "lat", "time_id", "value")
    ),
    regexp = "path_output"
  )

  testthat::expect_error(
    save_raster_cell_table(
      path_input = path_input,
      path_output = path_output,
      data_constants = tibble::tibble(time_id = c(20L, 15L)),
      vec_columns = c("long", "lat", "time_id", "value")
    ),
    regexp = "data_constants"
  )

  testthat::expect_error(
    save_raster_cell_table(
      path_input = path_input,
      path_output = path_output,
      data_constants = tibble::tibble(time_id = NA_integer_),
      vec_columns = c("long", "lat", "time_id", "value")
    ),
    regexp = "data_constants"
  )

  testthat::expect_error(
    save_raster_cell_table(
      path_input = path_input,
      path_output = path_output,
      data_constants = tibble::tibble(value = 1),
      vec_columns = c("long", "lat", "value")
    ),
    regexp = "data_constants"
  )

  testthat::expect_error(
    save_raster_cell_table(
      path_input = path_input,
      path_output = path_output,
      data_constants = tibble::tibble(time_id = 20L),
      vec_columns = c("long", "lat", "value")
    ),
    regexp = "vec_columns"
  )

  testthat::expect_error(
    save_raster_cell_table(
      path_input = path_input,
      path_output = path_output,
      data_constants = tibble::tibble(time_id = 20L),
      vec_columns = c("long", "lat", "time_id", "value", "value")
    ),
    regexp = "vec_columns"
  )

  testthat::expect_false(base::file.exists(path_output))
})


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("save_raster_cell_table() saves the ordered columns", {
  dir_root <-
    withr::local_tempdir()

  path_input <-
    base::file.path(dir_root, "values.tif")

  build_quality_raster_fixture(
    path_file = path_input,
    mat_values = mat_cell_values
  )

  # The folder of the output does not exist yet
  path_output <-
    base::file.path(dir_root, "Cell_tables", "values.qs2")

  res <-
    save_raster_cell_table(
      path_input = path_input,
      path_output = path_output,
      data_constants = tibble::tibble(time_id = 15L),
      vec_columns = c("long", "lat", "time_id", "value")
    )

  testthat::expect_equal(res, path_output)
  testthat::expect_true(base::file.exists(path_output))
  testthat::expect_false(
    base::file.exists(stringr::str_c(path_output, ".part"))
  )

  data_saved <-
    qs2::qs_read(file = path_output)

  testthat::expect_s3_class(data_saved, "tbl_df")
  testthat::expect_named(data_saved, c("long", "lat", "time_id", "value"))
  testthat::expect_type(dplyr::pull(data_saved, "time_id"), "integer")
  testthat::expect_equal(dplyr::pull(data_saved, "time_id"), base::rep(15L, 5))
  testthat::expect_equal(
    dplyr::pull(data_saved, "long"),
    c(-135, 45, 135, -45, 45)
  )
  testthat::expect_equal(
    dplyr::pull(data_saved, "lat"),
    c(45, 45, 45, -45, -45)
  )
  testthat::expect_equal(dplyr::pull(data_saved, "value"), c(1, 3, 4, 6, 7))
})

testthat::test_that("save_raster_cell_table() adds a variable label", {
  dir_root <-
    withr::local_tempdir()

  path_input <-
    base::file.path(dir_root, "values.tif")

  build_quality_raster_fixture(
    path_file = path_input,
    mat_values = mat_cell_values
  )

  path_output <-
    save_raster_cell_table(
      path_input = path_input,
      path_output = base::file.path(dir_root, "values.qs2"),
      data_constants = tibble::tibble(var_name = "bio_01"),
      vec_columns = c("long", "lat", "value", "var_name")
    )

  data_saved <-
    qs2::qs_read(file = path_output)

  testthat::expect_named(data_saved, c("long", "lat", "value", "var_name"))
  testthat::expect_equal(
    dplyr::pull(data_saved, "var_name"),
    base::rep("bio_01", 5)
  )
})

testthat::test_that("save_raster_cell_table() works without constants", {
  dir_root <-
    withr::local_tempdir()

  path_input <-
    base::file.path(dir_root, "values.tif")

  build_quality_raster_fixture(
    path_file = path_input,
    mat_values = mat_cell_values
  )

  path_output <-
    save_raster_cell_table(
      path_input = path_input,
      path_output = base::file.path(dir_root, "values.qs2"),
      data_constants = tibble::tibble(.rows = 1L),
      vec_columns = c("value", "long", "lat")
    )

  data_saved <-
    qs2::qs_read(file = path_output)

  testthat::expect_named(data_saved, c("value", "long", "lat"))
  testthat::expect_equal(base::nrow(data_saved), 5L)
})

testthat::test_that("save_raster_cell_table() replaces its own output", {
  dir_root <-
    withr::local_tempdir()

  path_input <-
    base::file.path(dir_root, "values.tif")

  path_output <-
    base::file.path(dir_root, "values.qs2")

  build_quality_raster_fixture(
    path_file = path_input,
    mat_values = mat_cell_values
  )

  save_raster_cell_table(
    path_input = path_input,
    path_output = path_output,
    data_constants = tibble::tibble(time_id = 20L),
    vec_columns = c("long", "lat", "time_id", "value")
  )

  build_quality_raster_fixture(
    path_file = path_input,
    mat_values = base::matrix(2, nrow = 2, ncol = 4)
  )

  save_raster_cell_table(
    path_input = path_input,
    path_output = path_output,
    data_constants = tibble::tibble(time_id = 20L),
    vec_columns = c("long", "lat", "time_id", "value")
  )

  data_saved <-
    qs2::qs_read(file = path_output)

  testthat::expect_equal(dplyr::pull(data_saved, "value"), base::rep(2, 8))
})


#----------------------------------------------------------#
# Failing closed -----
#----------------------------------------------------------#

testthat::test_that("save_raster_cell_table() never saves an empty table", {
  dir_root <-
    withr::local_tempdir()

  path_input <-
    base::file.path(dir_root, "empty.tif")

  path_output <-
    base::file.path(dir_root, "empty.qs2")

  build_quality_raster_fixture(
    path_file = path_input,
    mat_values = base::matrix(NA_real_, nrow = 2, ncol = 4)
  )

  testthat::expect_error(
    save_raster_cell_table(
      path_input = path_input,
      path_output = path_output,
      data_constants = tibble::tibble(time_id = 20L),
      vec_columns = c("long", "lat", "time_id", "value")
    ),
    regexp = "no cell with a value"
  )

  testthat::expect_false(base::file.exists(path_output))
  testthat::expect_false(
    base::file.exists(stringr::str_c(path_output, ".part"))
  )
})

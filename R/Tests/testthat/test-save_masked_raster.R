#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("save_masked_raster() validates its arguments", {
  dir_root <-
    withr::local_tempdir()

  path_input <-
    base::file.path(dir_root, "in.tif")

  build_quality_raster_fixture(
    path_file = path_input,
    mat_values = base::matrix(1, nrow = 4, ncol = 8)
  )

  testthat::expect_error(
    save_masked_raster(
      path_input = path_input,
      path_output = path_input,
      vec_cells = 1L
    ),
    regexp = "path_output"
  )

  testthat::expect_error(
    save_masked_raster(
      path_input = path_input,
      path_output = base::file.path(dir_root, "out.tif"),
      vec_cells = "1"
    ),
    regexp = "vec_cells"
  )

  testthat::expect_error(
    save_masked_raster(
      path_input = path_input,
      path_output = base::file.path(dir_root, "out.tif"),
      vec_cells = 33L
    ),
    regexp = "33"
  )
})


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("save_masked_raster() sets the listed cells missing", {
  dir_root <-
    withr::local_tempdir()

  path_input <-
    base::file.path(dir_root, "in.tif")

  vec_values <-
    base::seq_len(32)

  build_quality_raster_fixture(
    path_file = path_input,
    mat_values = base::matrix(vec_values, nrow = 4, ncol = 8, byrow = TRUE)
  )

  path_output <-
    base::file.path(dir_root, "Final", "out.tif")

  res <-
    save_masked_raster(
      path_input = path_input,
      path_output = path_output,
      vec_cells = c(11L, 12L)
    )

  testthat::expect_equal(res, path_output)

  vec_out <-
    terra::values(terra::rast(path_output), mat = FALSE)

  testthat::expect_true(base::all(base::is.na(vec_out[c(11, 12)])))
  testthat::expect_equal(vec_out[-c(11, 12)], vec_values[-c(11, 12)])
  testthat::expect_equal(terra::datatype(terra::rast(path_output)), "FLT4S")
})

testthat::test_that("save_masked_raster() copies without cells", {
  dir_root <-
    withr::local_tempdir()

  path_input <-
    base::file.path(dir_root, "in.tif")

  build_quality_raster_fixture(
    path_file = path_input,
    mat_values = base::matrix(base::seq_len(32), nrow = 4, ncol = 8)
  )

  path_output <-
    save_masked_raster(
      path_input = path_input,
      path_output = base::file.path(dir_root, "out.tif"),
      vec_cells = base::integer(0)
    )

  testthat::expect_equal(
    terra::values(terra::rast(path_output), mat = FALSE),
    terra::values(terra::rast(path_input), mat = FALSE)
  )
  testthat::expect_equal(
    base::as.vector(terra::ext(terra::rast(path_output))),
    base::as.vector(terra::ext(terra::rast(path_input)))
  )
})

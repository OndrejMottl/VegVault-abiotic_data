#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("save_aggregated_raster() validates its arguments", {
  dir_root <-
    withr::local_tempdir()

  path_input <-
    base::file.path(dir_root, "in.tif")

  build_quality_raster_fixture(
    path_file = path_input,
    mat_values = base::matrix(1, nrow = 4, ncol = 8)
  )

  testthat::expect_error(
    save_aggregated_raster(
      path_input = path_input,
      path_output = path_input,
      aggregation_factor = 2,
      aggregation_fun = "median"
    ),
    regexp = "path_output"
  )

  testthat::expect_error(
    save_aggregated_raster(
      path_input = path_input,
      path_output = base::file.path(dir_root, "out.tif"),
      aggregation_factor = 1.5,
      aggregation_fun = "median"
    ),
    regexp = "aggregation_factor"
  )

  testthat::expect_error(
    save_aggregated_raster(
      path_input = path_input,
      path_output = base::file.path(dir_root, "out.tif"),
      aggregation_factor = 2,
      aggregation_fun = "mean"
    ),
    regexp = "aggregation_fun"
  )

  testthat::expect_error(
    save_aggregated_raster(
      path_input = base::file.path(dir_root, "none.tif"),
      path_output = base::file.path(dir_root, "out.tif"),
      aggregation_factor = 2,
      aggregation_fun = "median"
    ),
    regexp = "path_input"
  )
})


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("save_aggregated_raster() takes the block median", {
  dir_root <-
    withr::local_tempdir()

  path_input <-
    base::file.path(dir_root, "in.tif")

  # Blocks of 2 by 2: (1, 2, 3, 10), (4, 4, 4, 4), (NA, 1, 1, 1), (5, 6, 7, 8)
  build_quality_raster_fixture(
    path_file = path_input,
    mat_values = base::rbind(
      c(1, 2, 4, 4),
      c(3, 10, 4, 4),
      c(NA, 1, 5, 6),
      c(1, 1, 7, 8)
    )
  )

  path_output <-
    base::file.path(dir_root, "Sub", "out.tif")

  res <-
    save_aggregated_raster(
      path_input = path_input,
      path_output = path_output,
      aggregation_factor = 2,
      aggregation_fun = "median"
    )

  testthat::expect_equal(res, path_output)

  rast_out <-
    terra::rast(path_output)

  testthat::expect_equal(base::dim(rast_out)[1:2], c(2, 2))
  testthat::expect_equal(terra::datatype(rast_out), "FLT4S")
  testthat::expect_equal(
    terra::values(rast_out, mat = FALSE),
    c(2.5, 4, NA, 6.5)
  )
  testthat::expect_length(
    base::setdiff(base::list.files(base::dirname(path_output)), "out.tif"),
    0L
  )
})

testthat::test_that("save_aggregated_raster() pads blocks with missing", {
  dir_root <-
    withr::local_tempdir()

  path_input <-
    base::file.path(dir_root, "in.tif")

  build_quality_raster_fixture(
    path_file = path_input,
    mat_values = base::matrix(1:6, nrow = 3, ncol = 2, byrow = TRUE)
  )

  path_output <-
    save_aggregated_raster(
      path_input = path_input,
      path_output = base::file.path(dir_root, "out.tif"),
      aggregation_factor = 2,
      aggregation_fun = "median"
    )

  rast_out <-
    terra::rast(path_output)

  testthat::expect_equal(terra::nrow(rast_out), 2)
  testthat::expect_equal(terra::values(rast_out, mat = FALSE), c(2.5, NA))
})

testthat::test_that("save_aggregated_raster() takes the mode", {
  dir_root <-
    withr::local_tempdir()

  path_input <-
    base::file.path(dir_root, "in.tif")

  # Blocks: (5, 3, 3, 5) is a tie, (7, 7, 9, 7) has the mode 7
  build_quality_raster_fixture(
    path_file = path_input,
    mat_values = base::rbind(c(5, 3, 7, 7), c(3, 5, 9, 7))
  )

  path_output <-
    save_aggregated_raster(
      path_input = path_input,
      path_output = base::file.path(dir_root, "out.tif"),
      aggregation_factor = 2,
      aggregation_fun = "modal",
      datatype = "INT1U"
    )

  rast_out <-
    terra::rast(path_output)

  testthat::expect_equal(terra::datatype(rast_out), "INT1U")
  testthat::expect_equal(terra::values(rast_out, mat = FALSE), c(3, 7))
})

testthat::test_that("save_aggregated_raster() replaces its own output", {
  dir_root <-
    withr::local_tempdir()

  path_input <-
    base::file.path(dir_root, "in.tif")

  path_output <-
    base::file.path(dir_root, "out.tif")

  build_quality_raster_fixture(
    path_file = path_input,
    mat_values = base::matrix(1, nrow = 4, ncol = 8)
  )

  save_aggregated_raster(
    path_input = path_input,
    path_output = path_output,
    aggregation_factor = 2,
    aggregation_fun = "median"
  )

  build_quality_raster_fixture(
    path_file = path_input,
    mat_values = base::matrix(3, nrow = 4, ncol = 8)
  )

  save_aggregated_raster(
    path_input = path_input,
    path_output = path_output,
    aggregation_factor = 2,
    aggregation_fun = "median"
  )

  testthat::expect_true(
    base::all(terra::values(terra::rast(path_output), mat = FALSE) == 3)
  )
})

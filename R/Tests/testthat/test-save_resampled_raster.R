#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("save_resampled_raster() validates its arguments", {
  dir_root <-
    withr::local_tempdir()

  path_input <-
    base::file.path(dir_root, "in.tif")

  build_quality_raster_fixture(
    path_file = path_input,
    mat_values = base::matrix(1, nrow = 18, ncol = 36)
  )

  testthat::expect_error(
    save_resampled_raster(
      path_input = path_input,
      path_template = path_input,
      path_output = path_input,
      datatype = "INT1U"
    ),
    regexp = "path_output"
  )

  testthat::expect_error(
    save_resampled_raster(
      path_input = path_input,
      path_template = base::file.path(dir_root, "none.tif"),
      path_output = base::file.path(dir_root, "out.tif"),
      datatype = "INT1U"
    ),
    regexp = "path_template"
  )
})


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("save_resampled_raster() takes the template grid", {
  dir_root <-
    withr::local_tempdir()

  path_input <-
    base::file.path(dir_root, "in.tif")

  # Every row holds its own row number, so the cropped rows can be seen
  build_quality_raster_fixture(
    path_file = path_input,
    mat_values = base::matrix(1:18, nrow = 18, ncol = 36)
  )

  path_template <-
    base::file.path(dir_root, "template.tif")

  terra::writeRaster(
    x = terra::rast(
      nrows = 16,
      ncols = 36,
      xmin = -180,
      xmax = 180,
      ymin = -90,
      ymax = 70,
      crs = "EPSG:4326",
      vals = 0
    ),
    filename = path_template
  )

  path_output <-
    base::file.path(dir_root, "out.tif")

  res <-
    save_resampled_raster(
      path_input = path_input,
      path_template = path_template,
      path_output = path_output,
      datatype = "INT1U"
    )

  testthat::expect_equal(res, path_output)

  rast_out <-
    terra::rast(path_output)

  rast_template <-
    terra::rast(path_template)

  testthat::expect_equal(base::dim(rast_out), base::dim(rast_template))
  testthat::expect_equal(
    base::as.vector(terra::ext(rast_out)),
    base::as.vector(terra::ext(rast_template))
  )
  testthat::expect_equal(terra::datatype(rast_out), "INT1U")

  mat_out <-
    terra::as.matrix(rast_out, wide = TRUE)

  # The two input rows north of 70 N are dropped
  testthat::expect_equal(base::unique(mat_out[, 1]), 3:18)
})

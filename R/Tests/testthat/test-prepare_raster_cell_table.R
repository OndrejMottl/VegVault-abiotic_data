#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("prepare_raster_cell_table() validates its argument", {
  testthat::expect_error(
    prepare_raster_cell_table(path_file = "none.tif"),
    regexp = "path_file"
  )

  testthat::expect_error(
    prepare_raster_cell_table(path_file = c("a.tif", "b.tif")),
    regexp = "path_file"
  )
})

testthat::test_that("prepare_raster_cell_table() needs one layer", {
  dir_root <-
    withr::local_tempdir()

  path_file <-
    base::file.path(dir_root, "two_layers.tif")

  rast_two_layers <-
    terra::rast(
      nrows = 2,
      ncols = 4,
      nlyrs = 2,
      xmin = -180,
      xmax = 180,
      ymin = -90,
      ymax = 90,
      crs = "EPSG:4326",
      vals = 1
    )

  terra::writeRaster(x = rast_two_layers, filename = path_file)

  testthat::expect_error(
    prepare_raster_cell_table(path_file = path_file),
    regexp = "layer"
  )
})


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("prepare_raster_cell_table() keeps cells with a value", {
  dir_root <-
    withr::local_tempdir()

  path_file <-
    base::file.path(dir_root, "values.tif")

  # Cells of 90 degrees; cells 2, 5 and 8 are missing
  build_quality_raster_fixture(
    path_file = path_file,
    mat_values = base::matrix(
      c(1, NA, 3, 4, NA, 6, 7, NA),
      nrow = 2,
      ncol = 4,
      byrow = TRUE
    )
  )

  res <-
    prepare_raster_cell_table(path_file = path_file)

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(res, c("long", "lat", "value"))
  testthat::expect_type(dplyr::pull(res, "long"), "double")
  testthat::expect_type(dplyr::pull(res, "lat"), "double")
  testthat::expect_type(dplyr::pull(res, "value"), "double")
  # Cell order: row by row from the north-west corner, at cell centres
  testthat::expect_equal(
    dplyr::pull(res, "long"),
    c(-135, 45, 135, -45, 45)
  )
  testthat::expect_equal(dplyr::pull(res, "lat"), c(45, 45, 45, -45, -45))
  testthat::expect_equal(dplyr::pull(res, "value"), c(1, 3, 4, 6, 7))
})

testthat::test_that("prepare_raster_cell_table() reads integers as double", {
  dir_root <-
    withr::local_tempdir()

  path_file <-
    base::file.path(dir_root, "codes.tif")

  rast_codes <-
    terra::rast(
      nrows = 2,
      ncols = 4,
      xmin = -180,
      xmax = 180,
      ymin = -90,
      ymax = 90,
      crs = "EPSG:4326",
      vals = c(1, 19, NA, 1, NA, 19, 19, 32)
    )

  terra::writeRaster(x = rast_codes, filename = path_file, datatype = "INT1U")

  res <-
    prepare_raster_cell_table(path_file = path_file)

  testthat::expect_type(dplyr::pull(res, "value"), "double")
  testthat::expect_equal(dplyr::pull(res, "value"), c(1, 19, 1, 19, 19, 32))
})


#----------------------------------------------------------#
# Edge cases -----
#----------------------------------------------------------#

testthat::test_that("prepare_raster_cell_table() gives no rows when empty", {
  dir_root <-
    withr::local_tempdir()

  path_file <-
    base::file.path(dir_root, "empty.tif")

  build_quality_raster_fixture(
    path_file = path_file,
    mat_values = base::matrix(NA_real_, nrow = 2, ncol = 4)
  )

  res <-
    prepare_raster_cell_table(path_file = path_file)

  testthat::expect_named(res, c("long", "lat", "value"))
  testthat::expect_equal(base::nrow(res), 0L)
})

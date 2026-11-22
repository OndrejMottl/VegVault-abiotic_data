#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("save_classified_raster() validates its arguments", {
  dir_root <-
    withr::local_tempdir()

  path_input <-
    base::file.path(dir_root, "in.tif")

  build_quality_raster_fixture(
    path_file = path_input,
    mat_values = base::matrix(2, nrow = 4, ncol = 8)
  )

  testthat::expect_error(
    save_classified_raster(
      path_input = path_input,
      path_output = path_input,
      data_classes = tibble::tibble(from = 2, to = 1),
      datatype = "INT1U"
    ),
    regexp = "path_output"
  )

  testthat::expect_error(
    save_classified_raster(
      path_input = path_input,
      path_output = base::file.path(dir_root, "out.tif"),
      data_classes = tibble::tibble(from = c(2, 2), to = c(1, 3)),
      datatype = "INT1U"
    ),
    regexp = "data_classes"
  )
})


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("save_classified_raster() replaces values by class", {
  dir_root <-
    withr::local_tempdir()

  path_input <-
    base::file.path(dir_root, "in.tif")

  # Unit 9 has no class and becomes missing; the missing cell stays missing
  build_quality_raster_fixture(
    path_file = path_input,
    mat_values = base::rbind(c(2, 3, 4, 9), c(NA, 2, 3, 4))
  )

  path_output <-
    base::file.path(dir_root, "Sub", "out.tif")

  res <-
    save_classified_raster(
      path_input = path_input,
      path_output = path_output,
      data_classes = tibble::tibble(from = c(2, 3, 4), to = c(1, 34, 37)),
      datatype = "INT1U"
    )

  testthat::expect_equal(res, path_output)

  rast_out <-
    terra::rast(path_output)

  testthat::expect_equal(terra::datatype(rast_out), "INT1U")
  testthat::expect_equal(
    terra::values(rast_out, mat = FALSE),
    c(1, 34, 37, NA, NA, 1, 34, 37)
  )
})

#----------------------------------------------------------#
# Fixtures -----
#----------------------------------------------------------#

list_mask_valid <-
  base::list(
    detector = "bio12",
    ages = 500,
    region = base::list(
      lon_min = -90, lon_max = 0, lat_min = -90, lat_max = 45
    ),
    ratio_min = 2,
    increase_min = 100
  )


#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("build_artefact_mask_cells() validates its arguments", {
  testthat::expect_error(
    build_artefact_mask_cells(
      data_processing_files = tibble::tibble(variable = "bio12"),
      list_artefact_mask = list_mask_valid
    ),
    regexp = "data_processing_files"
  )

  testthat::expect_error(
    build_artefact_mask_cells(
      data_processing_files = tibble::tibble(
        variable = "bio12",
        age = 0,
        path_aggregated = "a.tif"
      ),
      list_artefact_mask = base::replace(list_mask_valid, "ratio_min", -1)
    ),
    regexp = "ratio_min"
  )

  testthat::expect_error(
    build_artefact_mask_cells(
      data_processing_files = tibble::tibble(
        variable = "bio12",
        age = 0,
        path_aggregated = "a.tif"
      ),
      list_artefact_mask = base::replace(
        list_mask_valid,
        "region",
        base::list(base::list(lon_min = 0))
      )
    ),
    regexp = "region"
  )
})


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("build_artefact_mask_cells() flags inflated cells", {
  dir_root <-
    withr::local_tempdir()

  # Coarse rasters of 4 by 8 cells of 45 degrees, as aggregated
  vec_ages <-
    c(0, 500, 1000)

  data_files <-
    tibble::tibble(
      variable = "bio12",
      age = vec_ages,
      path_aggregated = base::file.path(
        dir_root,
        stringr::str_c("bio12_", vec_ages, ".tif")
      )
    )

  mat_base <-
    base::matrix(100, nrow = 4, ncol = 8)

  build_quality_raster_fixture(
    path_file = dplyr::pull(data_files, "path_aggregated")[1],
    mat_values = mat_base
  )

  # Age 500: cells 11 and 12 inside the region, cell 15 outside it
  build_quality_raster_fixture(
    path_file = dplyr::pull(data_files, "path_aggregated")[2],
    mat_values = mat_base |>
      base::replace(list = base::cbind(c(2, 2, 2), c(3, 4, 7)), values = 600)
  )

  # Age 1000, not a listed age: cell 19 inflated
  build_quality_raster_fixture(
    path_file = dplyr::pull(data_files, "path_aggregated")[3],
    mat_values = mat_base |>
      base::replace(list = base::cbind(3, 3), values = 600)
  )

  res <-
    build_artefact_mask_cells(
      data_processing_files = data_files,
      list_artefact_mask = list_mask_valid
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(
    res,
    c("age", "cell", "long", "lat", "value", "median_all_ages")
  )
  testthat::expect_equal(dplyr::pull(res, "cell"), c(11L, 12L))
  testthat::expect_equal(dplyr::pull(res, "age"), c(500, 500))
  testthat::expect_equal(dplyr::pull(res, "long"), c(-67.5, -22.5))
  testthat::expect_equal(dplyr::pull(res, "lat"), c(22.5, 22.5))
  testthat::expect_equal(dplyr::pull(res, "value"), c(600, 600))
  testthat::expect_equal(dplyr::pull(res, "median_all_ages"), c(100, 100))
})

testthat::test_that("build_artefact_mask_cells() needs a minimum increase", {
  dir_root <-
    withr::local_tempdir()

  vec_ages <-
    c(0, 500, 1000)

  data_files <-
    tibble::tibble(
      variable = "bio12",
      age = vec_ages,
      path_aggregated = base::file.path(
        dir_root,
        stringr::str_c("bio12_", vec_ages, ".tif")
      )
    )

  purrr::walk2(
    .x = dplyr::pull(data_files, "path_aggregated"),
    .y = c(10, 30, 10),
    .f = ~ build_quality_raster_fixture(
      path_file = .x,
      mat_values = base::matrix(.y, nrow = 4, ncol = 8)
    )
  )

  res <-
    build_artefact_mask_cells(
      data_processing_files = data_files,
      list_artefact_mask = list_mask_valid
    )

  testthat::expect_equal(base::nrow(res), 0L)
})


#----------------------------------------------------------#
# Failing closed -----
#----------------------------------------------------------#

testthat::test_that("build_artefact_mask_cells() needs every listed age", {
  dir_root <-
    withr::local_tempdir()

  path_file <-
    base::file.path(dir_root, "bio12_0.tif")

  build_quality_raster_fixture(
    path_file = path_file,
    mat_values = base::matrix(100, nrow = 4, ncol = 8)
  )

  testthat::expect_error(
    build_artefact_mask_cells(
      data_processing_files = tibble::tibble(
        variable = "bio12",
        age = 0,
        path_aggregated = path_file
      ),
      list_artefact_mask = list_mask_valid
    ),
    regexp = "500"
  )
})

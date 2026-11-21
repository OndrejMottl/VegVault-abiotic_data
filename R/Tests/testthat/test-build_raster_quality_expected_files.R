#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("build_raster_quality_expected_files() needs set fields", {
  list_set_valid <-
    base::list(
      product = "prod_neo",
      dir = "Input/Neo",
      file_name_template = "neo_{file_variable}.tif",
      variables = base::list(bio1 = "bio_01"),
      ages = base::list(from = 0, to = 0, by = 1)
    )

  list_grid_valid <-
    base::list(
      n_col = 36, n_row = 18,
      x_min = -180, x_max = 180, y_min = -90, y_max = 90
    )

  testthat::expect_error(
    build_raster_quality_expected_files(
      list_check_set = base::replace(list_set_valid, "dir", base::list(1)),
      list_grid = list_grid_valid,
      path_data_storage = "Data"
    ),
    regexp = "dir"
  )

  testthat::expect_error(
    build_raster_quality_expected_files(
      list_check_set = base::replace(
        list_set_valid,
        "variables",
        base::list(base::list("bio_01"))
      ),
      list_grid = list_grid_valid,
      path_data_storage = "Data"
    ),
    regexp = "variables"
  )

  testthat::expect_error(
    build_raster_quality_expected_files(
      list_check_set = base::replace(
        list_set_valid,
        "ages",
        base::list(base::list(from = 0, to = 10))
      ),
      list_grid = list_grid_valid,
      path_data_storage = "Data"
    ),
    regexp = "ages"
  )

  testthat::expect_error(
    build_raster_quality_expected_files(
      list_check_set = list_set_valid,
      list_grid = base::replace(list_grid_valid, "n_row", base::list(NULL)),
      path_data_storage = "Data"
    ),
    regexp = "list_grid"
  )

  testthat::expect_error(
    build_raster_quality_expected_files(
      list_check_set = list_set_valid,
      list_grid = list_grid_valid,
      path_data_storage = NA_character_
    ),
    regexp = "path_data_storage"
  )

  testthat::expect_error(
    build_raster_quality_expected_files(
      list_check_set = list_set_valid,
      list_grid = list_grid_valid,
      path_data_storage = "Data",
      data_server_sizes = tibble::tibble(file_name = "neo_bio_01.tif")
    ),
    regexp = "data_server_sizes"
  )
})


#----------------------------------------------------------#
# Outputs -----
#----------------------------------------------------------#

testthat::test_that("build_raster_quality_expected_files() returns columns", {
  res <-
    build_raster_quality_expected_files(
      list_check_set = base::list(
        product = "prod_neo",
        dir = "Input/Neo",
        file_name_template = "neo_{file_variable}.tif",
        variables = base::list(bio1 = "bio_01"),
        ages = base::list(from = 0, to = 0, by = 1)
      ),
      list_grid = base::list(
        n_col = 36, n_row = 18,
        x_min = -180, x_max = 180, y_min = -90, y_max = 90
      ),
      path_data_storage = "Data"
    )

  testthat::expect_s3_class(res, "tbl_df")

  testthat::expect_named(
    res,
    c(
      "product", "variable", "age", "file_name", "path_file",
      "expected_size_bytes", "expected_n_col", "expected_n_row",
      "expected_x_min", "expected_x_max", "expected_y_min",
      "expected_y_max"
    )
  )

  testthat::expect_type(dplyr::pull(res, "age"), "double")

  testthat::expect_type(dplyr::pull(res, "expected_size_bytes"), "double")
})


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("build_raster_quality_expected_files() fills a neo set", {
  res <-
    build_raster_quality_expected_files(
      list_check_set = base::list(
        product = "prod_neo",
        dir = "Input/Neo",
        file_name_template = "neo_{file_variable}_V2.1.tif",
        variables = base::list(bio4 = "bio_04", bio1 = "bio_01"),
        ages = base::list(from = 0, to = 0, by = 1)
      ),
      list_grid = base::list(
        n_col = 36, n_row = 18,
        x_min = -180, x_max = 180, y_min = -90, y_max = 90
      ),
      path_data_storage = "D:/root/Data"
    )

  testthat::expect_equal(dplyr::pull(res, "variable"), c("bio4", "bio1"))

  testthat::expect_equal(
    dplyr::pull(res, "file_name"),
    c("neo_bio_04_V2.1.tif", "neo_bio_01_V2.1.tif")
  )

  testthat::expect_equal(
    dplyr::pull(res, "path_file"),
    c(
      "D:/root/Data/Input/Neo/neo_bio_04_V2.1.tif",
      "D:/root/Data/Input/Neo/neo_bio_01_V2.1.tif"
    )
  )

  testthat::expect_equal(dplyr::pull(res, "age"), c(0, 0))

  testthat::expect_equal(dplyr::pull(res, "product"), c("prod_neo", "prod_neo"))

  testthat::expect_true(
    base::all(base::is.na(dplyr::pull(res, "expected_size_bytes")))
  )

  testthat::expect_equal(dplyr::pull(res, "expected_n_col"), c(36, 36))

  testthat::expect_equal(dplyr::pull(res, "expected_y_min"), c(-90, -90))
})

testthat::test_that("build_raster_quality_expected_files() maps time ids", {
  res <-
    build_raster_quality_expected_files(
      list_check_set = base::list(
        product = "prod_pal",
        dir = "Input/Pal",
        file_name_template = "pal_{file_variable}_{time_id}_V1.0.tif",
        variables = c(bio1 = "bio01", bio12 = "bio12"),
        ages = base::list(from = 0, to = 3000, by = 1500),
        time_id = base::list(origin_age = 2000, step_years = 100)
      ),
      list_grid = base::list(
        n_col = 36, n_row = 18,
        x_min = -180, x_max = 180, y_min = -90, y_max = 90
      ),
      path_data_storage = "Data"
    )

  testthat::expect_equal(base::nrow(res), 6L)

  testthat::expect_equal(
    dplyr::pull(res, "variable"),
    c("bio1", "bio1", "bio1", "bio12", "bio12", "bio12")
  )

  testthat::expect_equal(
    dplyr::pull(res, "age"),
    c(0, 1500, 3000, 0, 1500, 3000)
  )

  testthat::expect_equal(
    dplyr::pull(res, "file_name"),
    c(
      "pal_bio01_20_V1.0.tif", "pal_bio01_5_V1.0.tif",
      "pal_bio01_-10_V1.0.tif", "pal_bio12_20_V1.0.tif",
      "pal_bio12_5_V1.0.tif", "pal_bio12_-10_V1.0.tif"
    )
  )
})

testthat::test_that("build_raster_quality_expected_files() takes sizes", {
  data_server_sizes <-
    tibble::tibble(
      product = c("prod_neo", "prod_neo", "prod_other"),
      file_name = c("neo_bio_01.tif", "neo_bio_04.tif", "neo_bio_01.tif"),
      size_bytes = c(1000, 2000, 5)
    )

  res <-
    build_raster_quality_expected_files(
      list_check_set = base::list(
        product = "prod_neo",
        dir = "Input/Neo",
        file_name_template = "neo_{file_variable}.tif",
        variables = base::list(bio1 = "bio_01", bio4 = "bio_04"),
        ages = base::list(from = 0, to = 0, by = 1)
      ),
      list_grid = base::list(
        n_col = 36, n_row = 18,
        x_min = -180, x_max = 180, y_min = -90, y_max = 90
      ),
      path_data_storage = "Data",
      data_server_sizes = data_server_sizes
    )

  testthat::expect_equal(
    dplyr::pull(res, "expected_size_bytes"),
    c(1000, 2000)
  )
})


#----------------------------------------------------------#
# Edge cases -----
#----------------------------------------------------------#

testthat::test_that("build_raster_quality_expected_files() needs all sizes", {
  testthat::expect_error(
    build_raster_quality_expected_files(
      list_check_set = base::list(
        product = "prod_neo",
        dir = "Input/Neo",
        file_name_template = "neo_{file_variable}.tif",
        variables = base::list(bio1 = "bio_01", bio4 = "bio_04"),
        ages = base::list(from = 0, to = 0, by = 1)
      ),
      list_grid = base::list(
        n_col = 36, n_row = 18,
        x_min = -180, x_max = 180, y_min = -90, y_max = 90
      ),
      path_data_storage = "Data",
      data_server_sizes = tibble::tibble(
        product = "prod_neo",
        file_name = "neo_bio_01.tif",
        size_bytes = 1000
      )
    ),
    regexp = "neo_bio_04.tif"
  )
})

testthat::test_that("build_raster_quality_expected_files() checks time ids", {
  list_set_pal <-
    base::list(
      product = "prod_pal",
      dir = "Input/Pal",
      file_name_template = "pal_{file_variable}_{time_id}.tif",
      variables = base::list(bio1 = "bio01"),
      ages = base::list(from = 0, to = 500, by = 250),
      time_id = base::list(origin_age = 2000, step_years = 100)
    )

  list_grid <-
    base::list(
      n_col = 36, n_row = 18,
      x_min = -180, x_max = 180, y_min = -90, y_max = 90
    )

  testthat::expect_error(
    build_raster_quality_expected_files(
      list_check_set = list_set_pal,
      list_grid = list_grid,
      path_data_storage = "Data"
    ),
    regexp = "whole number"
  )

  testthat::expect_error(
    build_raster_quality_expected_files(
      list_check_set = base::replace(
        list_set_pal,
        "time_id",
        base::list(NULL)
      ),
      list_grid = list_grid,
      path_data_storage = "Data"
    ),
    regexp = "time_id"
  )
})

testthat::test_that("build_raster_quality_expected_files() needs unique files", {
  testthat::expect_error(
    build_raster_quality_expected_files(
      list_check_set = base::list(
        product = "prod_pal",
        dir = "Input/Pal",
        file_name_template = "pal_{file_variable}.tif",
        variables = base::list(bio1 = "bio01"),
        ages = base::list(from = 0, to = 500, by = 500)
      ),
      list_grid = base::list(
        n_col = 36, n_row = 18,
        x_min = -180, x_max = 180, y_min = -90, y_max = 90
      ),
      path_data_storage = "Data"
    ),
    regexp = "unique"
  )
})

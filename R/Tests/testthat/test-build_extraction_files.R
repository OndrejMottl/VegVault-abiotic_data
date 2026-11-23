#----------------------------------------------------------#
# Fixtures -----
#----------------------------------------------------------#

data_processed_valid <-
  tibble::tibble(
    product = "prod_palaeo",
    variable = c("bio1", "bio12"),
    age = c(0, 500),
    time_id = c(20L, 15L),
    file_name = c("palaeo_bio01_20_V1.0.tif", "palaeo_bio12_15_V1.0.tif"),
    path_file = c(
      final_a = "Processed/palaeo_bio01_20_V1.0.tif",
      final_b = "Processed/palaeo_bio12_15_V1.0.tif"
    ),
    path_final = "not kept"
  )


#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("build_extraction_files() validates its arguments", {
  testthat::expect_error(
    build_extraction_files(
      data_processed_files = dplyr::select(data_processed_valid, -"path_file"),
      dir_cell_tables = "Cell_tables"
    ),
    regexp = "data_processed_files"
  )

  testthat::expect_error(
    build_extraction_files(
      data_processed_files = dplyr::mutate(
        data_processed_valid,
        time_id = c(20, 15)
      ),
      dir_cell_tables = "Cell_tables"
    ),
    regexp = "time_id"
  )

  testthat::expect_error(
    build_extraction_files(
      data_processed_files = data_processed_valid,
      dir_cell_tables = c("a", "b")
    ),
    regexp = "dir_cell_tables"
  )

  testthat::expect_error(
    build_extraction_files(
      data_processed_files = data_processed_valid,
      dir_cell_tables = "Cell_tables",
      list_var_names = base::list("bio_01", "bio_12")
    ),
    regexp = "list_var_names"
  )
})


#----------------------------------------------------------#
# Outputs -----
#----------------------------------------------------------#

testthat::test_that("build_extraction_files() names the cell tables", {
  res <-
    build_extraction_files(
      data_processed_files = data_processed_valid,
      dir_cell_tables = "Processed/Cell_tables"
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(
    res,
    c(
      "product", "variable", "age", "time_id", "file_name", "path_raster",
      "path_cell_table"
    )
  )
  testthat::expect_equal(
    dplyr::pull(res, "path_cell_table"),
    c(
      "Processed/Cell_tables/palaeo_bio01_20_V1.0.qs2",
      "Processed/Cell_tables/palaeo_bio12_15_V1.0.qs2"
    )
  )
  # The branch names of the processing stage are not passed on
  testthat::expect_null(base::names(dplyr::pull(res, "path_raster")))
  testthat::expect_equal(
    dplyr::pull(res, "path_raster"),
    c(
      "Processed/palaeo_bio01_20_V1.0.tif",
      "Processed/palaeo_bio12_15_V1.0.tif"
    )
  )
  testthat::expect_equal(dplyr::pull(res, "time_id"), c(20L, 15L))
})

testthat::test_that("build_extraction_files() adds the variable labels", {
  res <-
    build_extraction_files(
      data_processed_files = data_processed_valid,
      dir_cell_tables = "Cell_tables",
      list_var_names = base::list(bio12 = "bio_12", bio1 = "bio_01")
    )

  testthat::expect_named(
    res,
    c(
      "product", "variable", "age", "time_id", "file_name", "var_name",
      "path_raster", "path_cell_table"
    )
  )
  testthat::expect_equal(dplyr::pull(res, "var_name"), c("bio_01", "bio_12"))
})


#----------------------------------------------------------#
# Failing closed -----
#----------------------------------------------------------#

testthat::test_that("build_extraction_files() needs a label per variable", {
  testthat::expect_error(
    build_extraction_files(
      data_processed_files = data_processed_valid,
      dir_cell_tables = "Cell_tables",
      list_var_names = base::list(bio1 = "bio_01")
    ),
    regexp = "bio12"
  )
})

testthat::test_that("build_extraction_files() rejects repeated file names", {
  testthat::expect_error(
    build_extraction_files(
      data_processed_files = dplyr::mutate(
        data_processed_valid,
        file_name = "same.tif"
      ),
      dir_cell_tables = "Cell_tables"
    ),
    regexp = "same.tif"
  )
})

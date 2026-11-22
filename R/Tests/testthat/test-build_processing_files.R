#----------------------------------------------------------#
# Fixtures -----
#----------------------------------------------------------#

data_raw_valid <-
  tibble::tibble(
    product = "prod_palaeo",
    variable = c("bio12", "bio12"),
    age = c(0, 500),
    time_id = c(20L, 15L),
    file_name = c("palaeo_bio12_20.tif", "palaeo_bio12_15.tif"),
    path_file = c("Data/Input/palaeo_bio12_20.tif", "Data/Input/p15.tif"),
    sha256 = c("a", "b")
  )


#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("build_processing_files() validates its arguments", {
  testthat::expect_error(
    build_processing_files(
      data_raw_files = dplyr::select(data_raw_valid, -"path_file"),
      dir_processed = "Data/Processed/Palaeo"
    ),
    regexp = "data_raw_files"
  )

  testthat::expect_error(
    build_processing_files(
      data_raw_files = dplyr::select(data_raw_valid, -"sha256"),
      dir_processed = "Data/Processed/Palaeo"
    ),
    regexp = "sha256"
  )

  testthat::expect_error(
    build_processing_files(
      data_raw_files = data_raw_valid,
      dir_processed = NA_character_
    ),
    regexp = "dir_processed"
  )
})


#----------------------------------------------------------#
# Outputs -----
#----------------------------------------------------------#

testthat::test_that("build_processing_files() names the processed files", {
  res <-
    build_processing_files(
      data_raw_files = data_raw_valid,
      dir_processed = "Data/Processed/Palaeo"
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(
    res,
    c(
      "product", "variable", "age", "time_id", "file_name", "path_raw",
      "sha256_raw", "path_aggregated", "path_final"
    )
  )
  testthat::expect_equal(dplyr::pull(res, "sha256_raw"), c("a", "b"))
  testthat::expect_equal(
    dplyr::pull(res, "path_raw"),
    dplyr::pull(data_raw_valid, "path_file")
  )
  testthat::expect_equal(
    dplyr::pull(res, "path_aggregated"),
    c(
      "Data/Processed/Palaeo/Aggregated/palaeo_bio12_20.tif",
      "Data/Processed/Palaeo/Aggregated/palaeo_bio12_15.tif"
    )
  )
  testthat::expect_equal(
    dplyr::pull(res, "path_final"),
    c(
      "Data/Processed/Palaeo/palaeo_bio12_20.tif",
      "Data/Processed/Palaeo/palaeo_bio12_15.tif"
    )
  )
  testthat::expect_equal(dplyr::pull(res, "time_id"), c(20L, 15L))
})


#----------------------------------------------------------#
# Edge cases -----
#----------------------------------------------------------#

testthat::test_that("build_processing_files() needs unique file names", {
  testthat::expect_error(
    build_processing_files(
      data_raw_files = dplyr::mutate(data_raw_valid, file_name = "same.tif"),
      dir_processed = "Data/Processed/Palaeo"
    ),
    regexp = "same.tif"
  )
})

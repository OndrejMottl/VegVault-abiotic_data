#----------------------------------------------------------#
# Fixtures -----
#----------------------------------------------------------#

data_files_valid <-
  tibble::tibble(
    product = "prod_palaeo",
    variable = c("bio12", "bio12", "bio15", "bio15", "bio1"),
    age = c(0, 500, 0, 500, 500),
    time_id = c(20L, 15L, 20L, 15L, 15L),
    file_name = c("b12_20.tif", "b12_15.tif", "b15_20.tif", "b15_15.tif",
      "b1_15.tif"),
    path_raw = "raw.tif",
    path_aggregated = "agg.tif",
    path_final = "final.tif"
  )

vec_mask_columns <-
  c("file_name", "variable", "age", "cell", "mask")


#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("prepare_processing_mask_cells() validates arguments", {
  testthat::expect_error(
    prepare_processing_mask_cells(
      data_processing_files = dplyr::select(data_files_valid, -"age"),
      data_fill_cells = NULL,
      data_artefact_cells = NULL,
      vec_artefact_variables = base::character(0)
    ),
    regexp = "data_processing_files"
  )

  testthat::expect_error(
    prepare_processing_mask_cells(
      data_processing_files = data_files_valid,
      data_fill_cells = tibble::tibble(file_name = "b12_20.tif"),
      data_artefact_cells = NULL,
      vec_artefact_variables = base::character(0)
    ),
    regexp = "data_fill_cells"
  )
})


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("prepare_processing_mask_cells() returns no row empty", {
  res <-
    prepare_processing_mask_cells(
      data_processing_files = data_files_valid,
      data_fill_cells = NULL,
      data_artefact_cells = NULL,
      vec_artefact_variables = base::character(0)
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(res, vec_mask_columns)
  testthat::expect_equal(base::nrow(res), 0L)
  testthat::expect_type(dplyr::pull(res, "cell"), "integer")
})

testthat::test_that("prepare_processing_mask_cells() combines both masks", {
  res <-
    prepare_processing_mask_cells(
      data_processing_files = data_files_valid,
      data_fill_cells = tibble::tibble(
        file_name = "b12_20.tif",
        cell = c(25L, 29L)
      ),
      data_artefact_cells = tibble::tibble(
        age = 500,
        cell = c(11L, 12L)
      ),
      vec_artefact_variables = c("bio12", "bio15")
    )

  testthat::expect_named(res, vec_mask_columns)
  testthat::expect_equal(base::nrow(res), 6L)

  data_artefact <-
    dplyr::filter(res, .data[["mask"]] == "artefact")

  testthat::expect_setequal(
    dplyr::pull(data_artefact, "file_name"),
    c("b12_15.tif", "b15_15.tif")
  )
  testthat::expect_false("b1_15.tif" %in% dplyr::pull(res, "file_name"))
  testthat::expect_equal(
    dplyr::pull(dplyr::filter(res, .data[["mask"]] == "fill_value"), "cell"),
    c(25L, 29L)
  )
  testthat::expect_equal(
    dplyr::pull(res, "file_name"),
    base::sort(dplyr::pull(res, "file_name"))
  )
})


#----------------------------------------------------------#
# Failing closed -----
#----------------------------------------------------------#

testthat::test_that("prepare_processing_mask_cells() checks the files", {
  testthat::expect_error(
    prepare_processing_mask_cells(
      data_processing_files = data_files_valid,
      data_fill_cells = tibble::tibble(file_name = "none.tif", cell = 1L),
      data_artefact_cells = NULL,
      vec_artefact_variables = base::character(0)
    ),
    regexp = "none.tif"
  )

  testthat::expect_error(
    prepare_processing_mask_cells(
      data_processing_files = data_files_valid,
      data_fill_cells = NULL,
      data_artefact_cells = tibble::tibble(age = 500, cell = 1L),
      vec_artefact_variables = c("bio12", "bio19")
    ),
    regexp = "bio19"
  )
})

#----------------------------------------------------------#
# Fixtures -----
#----------------------------------------------------------#

vec_count_columns <-
  c(
    "product", "variable", "age", "time_id", "file_name", "mask", "n_cells",
    "n_cells_with_value"
  )


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("save_masked_cell_counts() counts the masked cells", {
  dir_root <-
    withr::local_tempdir()

  path_aggregated <-
    base::file.path(dir_root, "agg.tif")

  # Cell 25 holds a value, cell 26 is already missing
  build_quality_raster_fixture(
    path_file = path_aggregated,
    mat_values = base::matrix(1, nrow = 4, ncol = 8) |>
      base::replace(list = base::cbind(4, 2), values = NA)
  )

  data_files <-
    tibble::tibble(
      product = "prod_neo",
      variable = "bio12",
      age = 0,
      time_id = NA_integer_,
      file_name = "neo_bio12.tif",
      path_raw = "raw.tif",
      path_aggregated = path_aggregated,
      path_final = "final.tif"
    )

  path_output <-
    base::file.path(dir_root, "Tables", "counts.csv")

  res <-
    save_masked_cell_counts(
      data_mask_cells = tibble::tibble(
        file_name = "neo_bio12.tif",
        variable = "bio12",
        age = 0,
        cell = c(25L, 26L),
        mask = "fill_value"
      ),
      data_processing_files = data_files,
      path_output = path_output
    )

  testthat::expect_equal(res, path_output)

  data_counts <-
    readr::read_csv(file = path_output, show_col_types = FALSE)

  testthat::expect_named(data_counts, vec_count_columns)
  testthat::expect_equal(base::nrow(data_counts), 1L)
  testthat::expect_equal(dplyr::pull(data_counts, "n_cells"), 2)
  testthat::expect_equal(dplyr::pull(data_counts, "n_cells_with_value"), 1)
  testthat::expect_equal(dplyr::pull(data_counts, "mask"), "fill_value")
})

testthat::test_that("save_masked_cell_counts() writes a header when empty", {
  dir_root <-
    withr::local_tempdir()

  path_output <-
    base::file.path(dir_root, "counts.csv")

  save_masked_cell_counts(
    data_mask_cells = tibble::tibble(
      file_name = base::character(0),
      variable = base::character(0),
      age = base::numeric(0),
      cell = base::integer(0),
      mask = base::character(0)
    ),
    data_processing_files = tibble::tibble(
      product = "prod_neo",
      variable = "bio1",
      age = 0,
      time_id = NA_integer_,
      file_name = "neo_bio1.tif",
      path_raw = "raw.tif",
      path_aggregated = "agg.tif",
      path_final = "final.tif"
    ),
    path_output = path_output
  )

  data_counts <-
    readr::read_csv(file = path_output, show_col_types = FALSE)

  testthat::expect_named(data_counts, vec_count_columns)
  testthat::expect_equal(base::nrow(data_counts), 0L)
})

testthat::test_that("save_masked_cell_counts() validates its arguments", {
  testthat::expect_error(
    save_masked_cell_counts(
      data_mask_cells = tibble::tibble(file_name = "a.tif"),
      data_processing_files = tibble::tibble(file_name = "a.tif"),
      path_output = "x.csv"
    ),
    regexp = "data_mask_cells"
  )
})

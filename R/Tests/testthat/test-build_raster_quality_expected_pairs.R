#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("build_raster_quality_expected_pairs() checks columns", {
  testthat::expect_error(
    build_raster_quality_expected_pairs(
      data_expected_files = tibble::tibble(file_name = "a.tif"),
      list_pairs = base::list()
    ),
    regexp = "data_expected_files"
  )

  testthat::expect_error(
    build_raster_quality_expected_pairs(
      data_expected_files = "a.tif",
      list_pairs = base::list()
    ),
    regexp = "data_expected_files"
  )
})

testthat::test_that("build_raster_quality_expected_pairs() checks pairs", {
  data_files <-
    tibble::tibble(
      product = "prod_a",
      variable = c("bio12", "bio18"),
      age = 0,
      file_name = c("bio12.tif", "bio18.tif"),
      path_file = c("D/bio12.tif", "D/bio18.tif")
    )

  testthat::expect_error(
    build_raster_quality_expected_pairs(
      data_expected_files = data_files,
      list_pairs = "bio18"
    ),
    regexp = "list_pairs"
  )

  testthat::expect_error(
    build_raster_quality_expected_pairs(
      data_expected_files = data_files,
      list_pairs = base::list(base::list(variable = "bio18"))
    ),
    regexp = "list_pairs"
  )

  testthat::expect_error(
    build_raster_quality_expected_pairs(
      data_expected_files = data_files,
      list_pairs = base::list(
        base::list(variable = "bio19", reference = "bio12")
      )
    ),
    regexp = "bio19"
  )

  testthat::expect_error(
    build_raster_quality_expected_pairs(
      data_expected_files = data_files,
      list_pairs = base::list(
        base::list(variable = "bio18", reference = "bio18")
      )
    ),
    regexp = "itself"
  )

  testthat::expect_error(
    build_raster_quality_expected_pairs(
      data_expected_files = data_files,
      list_pairs = base::list(
        base::list(variable = "bio18", reference = "bio12"),
        base::list(variable = "bio18", reference = "bio12")
      )
    ),
    regexp = "twice"
  )
})


#----------------------------------------------------------#
# Outputs -----
#----------------------------------------------------------#

testthat::test_that("build_raster_quality_expected_pairs() returns columns", {
  data_files <-
    tibble::tibble(
      product = "prod_a",
      variable = c("bio12", "bio18"),
      age = 0,
      file_name = c("bio12.tif", "bio18.tif"),
      path_file = c("D/bio12.tif", "D/bio18.tif"),
      observed_size_bytes = c(10, 20)
    )

  res <-
    build_raster_quality_expected_pairs(
      data_expected_files = data_files,
      list_pairs = base::list(
        base::list(variable = "bio18", reference = "bio12")
      )
    )

  testthat::expect_s3_class(res, "tbl_df")

  testthat::expect_named(
    res,
    c(
      "product", "variable", "age", "file_name", "path_file",
      "observed_size_bytes", "variable_reference", "file_name_reference",
      "path_file_reference", "observed_size_bytes_reference"
    )
  )
})

testthat::test_that("build_raster_quality_expected_pairs() allows no pair", {
  data_files <-
    tibble::tibble(
      product = "prod_a",
      variable = "bio12",
      age = 0,
      file_name = "bio12.tif",
      path_file = "D/bio12.tif"
    )

  res <-
    build_raster_quality_expected_pairs(
      data_expected_files = data_files,
      list_pairs = base::list()
    )

  testthat::expect_equal(base::nrow(res), 0L)

  testthat::expect_named(
    res,
    c(
      "product", "variable", "age", "file_name", "path_file",
      "variable_reference", "file_name_reference", "path_file_reference"
    )
  )
})


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("build_raster_quality_expected_pairs() pairs by age", {
  data_files <-
    tibble::tibble(
      product = "prod_a",
      variable = c(
        "bio12", "bio12", "bio18", "bio18", "bio19", "bio19"
      ),
      age = c(0, 500, 0, 500, 0, 500),
      file_name = c(
        "bio12_0.tif", "bio12_5.tif", "bio18_0.tif",
        "bio18_5.tif", "bio19_0.tif", "bio19_5.tif"
      ),
      path_file = c(
        "D/bio12_0.tif", "D/bio12_5.tif", "D/bio18_0.tif",
        "D/bio18_5.tif", "D/bio19_0.tif", "D/bio19_5.tif"
      ),
      observed_modified_time = c("t1", "t2", "t3", "t4", "t5", "t6")
    )

  res <-
    build_raster_quality_expected_pairs(
      data_expected_files = data_files,
      list_pairs = base::list(
        base::list(variable = "bio18", reference = "bio12"),
        base::list(variable = "bio19", reference = "bio12")
      )
    )

  testthat::expect_equal(base::nrow(res), 4L)

  testthat::expect_equal(
    dplyr::pull(res, "file_name"),
    c("bio18_0.tif", "bio18_5.tif", "bio19_0.tif", "bio19_5.tif")
  )

  testthat::expect_equal(
    dplyr::pull(res, "file_name_reference"),
    c("bio12_0.tif", "bio12_5.tif", "bio12_0.tif", "bio12_5.tif")
  )

  testthat::expect_equal(
    dplyr::pull(res, "path_file_reference"),
    c("D/bio12_0.tif", "D/bio12_5.tif", "D/bio12_0.tif", "D/bio12_5.tif")
  )

  testthat::expect_equal(
    dplyr::pull(res, "observed_modified_time_reference"),
    c("t1", "t2", "t1", "t2")
  )

  testthat::expect_equal(
    dplyr::pull(res, "variable_reference"),
    base::rep("bio12", 4)
  )
})


#----------------------------------------------------------#
# Edge cases -----
#----------------------------------------------------------#

testthat::test_that("build_raster_quality_expected_pairs() needs references", {
  data_files <-
    tibble::tibble(
      product = "prod_a",
      variable = c("bio12", "bio18", "bio18"),
      age = c(0, 0, 500),
      file_name = c("bio12_0.tif", "bio18_0.tif", "bio18_5.tif"),
      path_file = c("D/bio12_0.tif", "D/bio18_0.tif", "D/bio18_5.tif")
    )

  testthat::expect_error(
    build_raster_quality_expected_pairs(
      data_expected_files = data_files,
      list_pairs = base::list(
        base::list(variable = "bio18", reference = "bio12")
      )
    ),
    regexp = "bio18_5.tif"
  )
})

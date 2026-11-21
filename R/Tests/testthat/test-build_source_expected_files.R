#----------------------------------------------------------#
# Fixtures -----
#----------------------------------------------------------#

list_source_neo <-
  base::list(
    product = "prod_neo",
    dir = "Input/Neo",
    file_name_template = "neo_{file_variable}.tif",
    url_template = "https://example.org/neo/{file_name}",
    variables = base::list(bio1 = "bio_01", bio12 = "bio_12"),
    ages = base::list(from = 0, to = 0, by = 1)
  )

list_source_palaeo <-
  base::list(
    product = "prod_palaeo",
    dir = "Input/Palaeo",
    file_name_template = "palaeo_{file_variable}_{time_id}.tif",
    variables = base::list(bio1 = "bio01"),
    ages = base::list(from = 0, to = 1000, by = 500),
    time_id = base::list(origin_age = 2000, step_years = 100)
  )

list_source_fixed <-
  base::list(
    product = "prod_arch",
    dir = "Input/Arch",
    files = base::list(
      b_raster.zip = "https://example.org/arch/raster",
      a_db.zip = "https://example.org/arch/db"
    )
  )


#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("build_source_expected_files() needs source fields", {
  testthat::expect_error(
    build_source_expected_files(
      list_source = "neo",
      path_data_storage = "Data"
    ),
    regexp = "list_source"
  )

  testthat::expect_error(
    build_source_expected_files(
      list_source = base::replace(list_source_neo, "product", base::list(1)),
      path_data_storage = "Data"
    ),
    regexp = "product"
  )

  testthat::expect_error(
    build_source_expected_files(
      list_source = base::replace(list_source_neo, "dir", base::list(1)),
      path_data_storage = "Data"
    ),
    regexp = "dir"
  )

  testthat::expect_error(
    build_source_expected_files(
      list_source = base::replace(
        list_source_neo,
        "variables",
        base::list(base::list("bio_01"))
      ),
      path_data_storage = "Data"
    ),
    regexp = "variables"
  )

  testthat::expect_error(
    build_source_expected_files(
      list_source = base::replace(
        list_source_neo,
        "ages",
        base::list(base::list(from = 0, to = 10))
      ),
      path_data_storage = "Data"
    ),
    regexp = "ages"
  )

  testthat::expect_error(
    build_source_expected_files(
      list_source = base::replace(
        list_source_neo,
        "url_template",
        base::list(1)
      ),
      path_data_storage = "Data"
    ),
    regexp = "url_template"
  )

  testthat::expect_error(
    build_source_expected_files(
      list_source = list_source_neo,
      path_data_storage = NA_character_
    ),
    regexp = "path_data_storage"
  )

  testthat::expect_error(
    build_source_expected_files(
      list_source = list_source_neo,
      path_data_storage = "Data",
      data_server_sizes = tibble::tibble(file_name = "neo_bio_01.tif")
    ),
    regexp = "data_server_sizes"
  )
})

testthat::test_that("build_source_expected_files() needs one file list", {
  testthat::expect_error(
    build_source_expected_files(
      list_source = c(
        list_source_neo,
        base::list(files = base::list(a.zip = "https://example.org/a"))
      ),
      path_data_storage = "Data"
    ),
    regexp = "file_name_template"
  )

  testthat::expect_error(
    build_source_expected_files(
      list_source = base::list(product = "prod_neo", dir = "Input/Neo"),
      path_data_storage = "Data"
    ),
    regexp = "files"
  )

  testthat::expect_error(
    build_source_expected_files(
      list_source = base::replace(
        list_source_fixed,
        "files",
        base::list(base::list("https://example.org/a"))
      ),
      path_data_storage = "Data"
    ),
    regexp = "files"
  )
})


#----------------------------------------------------------#
# Outputs -----
#----------------------------------------------------------#

testthat::test_that("build_source_expected_files() returns columns", {
  res <-
    build_source_expected_files(
      list_source = list_source_neo,
      path_data_storage = "Data"
    )

  testthat::expect_s3_class(res, "tbl_df")

  testthat::expect_named(
    res,
    c(
      "product", "variable", "age", "time_id", "file_name", "path_file",
      "url", "expected_size_bytes"
    )
  )

  testthat::expect_type(dplyr::pull(res, "age"), "double")
  testthat::expect_type(dplyr::pull(res, "time_id"), "integer")
  testthat::expect_type(dplyr::pull(res, "url"), "character")
  testthat::expect_type(dplyr::pull(res, "expected_size_bytes"), "double")
})


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("build_source_expected_files() fills a neo source", {
  res <-
    build_source_expected_files(
      list_source = list_source_neo,
      path_data_storage = "Data"
    )

  testthat::expect_equal(dplyr::pull(res, "variable"), c("bio1", "bio12"))
  testthat::expect_equal(
    dplyr::pull(res, "file_name"),
    c("neo_bio_01.tif", "neo_bio_12.tif")
  )
  testthat::expect_equal(
    dplyr::pull(res, "path_file"),
    c("Data/Input/Neo/neo_bio_01.tif", "Data/Input/Neo/neo_bio_12.tif")
  )
  testthat::expect_equal(
    dplyr::pull(res, "url"),
    c(
      "https://example.org/neo/neo_bio_01.tif",
      "https://example.org/neo/neo_bio_12.tif"
    )
  )
  testthat::expect_true(base::all(base::is.na(dplyr::pull(res, "time_id"))))
  testthat::expect_true(
    base::all(base::is.na(dplyr::pull(res, "expected_size_bytes")))
  )
})

testthat::test_that("build_source_expected_files() maps time ids", {
  res <-
    build_source_expected_files(
      list_source = list_source_palaeo,
      path_data_storage = "Data"
    )

  testthat::expect_equal(dplyr::pull(res, "age"), c(0, 500, 1000))
  testthat::expect_equal(dplyr::pull(res, "time_id"), c(20L, 15L, 10L))
  testthat::expect_equal(
    dplyr::pull(res, "file_name"),
    c("palaeo_bio01_20.tif", "palaeo_bio01_15.tif", "palaeo_bio01_10.tif")
  )
  testthat::expect_true(base::all(base::is.na(dplyr::pull(res, "url"))))
})

testthat::test_that("build_source_expected_files() lists a fixed source", {
  res <-
    build_source_expected_files(
      list_source = list_source_fixed,
      path_data_storage = "Data"
    )

  testthat::expect_equal(
    dplyr::pull(res, "file_name"),
    c("b_raster.zip", "a_db.zip")
  )
  testthat::expect_equal(
    dplyr::pull(res, "url"),
    c("https://example.org/arch/raster", "https://example.org/arch/db")
  )
  testthat::expect_equal(
    dplyr::pull(res, "path_file"),
    c("Data/Input/Arch/b_raster.zip", "Data/Input/Arch/a_db.zip")
  )
  testthat::expect_true(base::all(base::is.na(dplyr::pull(res, "variable"))))
  testthat::expect_true(base::all(base::is.na(dplyr::pull(res, "age"))))
  testthat::expect_true(base::all(base::is.na(dplyr::pull(res, "time_id"))))
})

testthat::test_that("build_source_expected_files() takes server sizes", {
  data_sizes <-
    tibble::tibble(
      product = c("prod_neo", "prod_neo", "prod_other"),
      file_name = c("neo_bio_01.tif", "neo_bio_12.tif", "neo_bio_01.tif"),
      size_bytes = c(100, 200, 999)
    )

  res <-
    build_source_expected_files(
      list_source = list_source_neo,
      path_data_storage = "Data",
      data_server_sizes = data_sizes
    )

  testthat::expect_equal(dplyr::pull(res, "expected_size_bytes"), c(100, 200))
})


#----------------------------------------------------------#
# Edge cases -----
#----------------------------------------------------------#

testthat::test_that("build_source_expected_files() needs every size", {
  testthat::expect_error(
    build_source_expected_files(
      list_source = list_source_neo,
      path_data_storage = "Data",
      data_server_sizes = tibble::tibble(
        product = "prod_neo",
        file_name = "neo_bio_01.tif",
        size_bytes = 100
      )
    ),
    regexp = "neo_bio_12.tif"
  )
})

testthat::test_that("build_source_expected_files() checks time ids", {
  testthat::expect_error(
    build_source_expected_files(
      list_source = base::replace(
        list_source_palaeo,
        "ages",
        base::list(base::list(from = 0, to = 50, by = 50))
      ),
      path_data_storage = "Data"
    ),
    regexp = "whole number"
  )

  testthat::expect_error(
    build_source_expected_files(
      list_source = base::replace(list_source_palaeo, "time_id", NULL),
      path_data_storage = "Data"
    ),
    regexp = "time_id"
  )
})

testthat::test_that("build_source_expected_files() needs unique files", {
  testthat::expect_error(
    build_source_expected_files(
      list_source = base::replace(
        list_source_neo,
        "file_name_template",
        "neo.tif"
      ),
      path_data_storage = "Data"
    ),
    regexp = "unique"
  )
})

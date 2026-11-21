#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("load_raster_server_sizes() needs an existing file", {
  testthat::expect_error(
    load_raster_server_sizes(path_file = "missing_sizes.csv"),
    regexp = "existing CSV"
  )

  testthat::expect_error(
    load_raster_server_sizes(path_file = 1),
    regexp = "existing CSV"
  )
})

testthat::test_that("load_raster_server_sizes() needs the columns", {
  path_csv <-
    withr::local_tempfile(fileext = ".csv")

  base::writeLines(
    text = c("product,file_name", "prod_a,a.tif"),
    con = path_csv
  )

  testthat::expect_error(
    load_raster_server_sizes(path_file = path_csv),
    regexp = "size_bytes"
  )
})


#----------------------------------------------------------#
# Outputs -----
#----------------------------------------------------------#

testthat::test_that("load_raster_server_sizes() returns the columns", {
  path_csv <-
    withr::local_tempfile(fileext = ".csv")

  base::writeLines(
    text = c(
      "product,file_name,size_bytes,checked_on,url",
      "prod_a,a.tif,535254836,2026-11-20,https://example.org/a.tif",
      "prod_b,a.tif,1232526033,2026-11-20,https://example.org/b/a.tif"
    ),
    con = path_csv
  )

  res <-
    load_raster_server_sizes(path_file = path_csv)

  testthat::expect_s3_class(res, "tbl_df")

  testthat::expect_named(res, c("product", "file_name", "size_bytes"))

  testthat::expect_type(dplyr::pull(res, "size_bytes"), "double")

  testthat::expect_equal(
    dplyr::pull(res, "size_bytes"),
    c(535254836, 1232526033)
  )

  testthat::expect_equal(dplyr::pull(res, "product"), c("prod_a", "prod_b"))
})


#----------------------------------------------------------#
# Edge cases -----
#----------------------------------------------------------#

testthat::test_that("load_raster_server_sizes() rejects repeated files", {
  path_csv <-
    withr::local_tempfile(fileext = ".csv")

  base::writeLines(
    text = c(
      "product,file_name,size_bytes",
      "prod_a,a.tif,10",
      "prod_a,a.tif,11"
    ),
    con = path_csv
  )

  testthat::expect_error(
    load_raster_server_sizes(path_file = path_csv),
    regexp = "twice"
  )
})

testthat::test_that("load_raster_server_sizes() rejects bad sizes", {
  path_csv <-
    withr::local_tempfile(fileext = ".csv")

  base::writeLines(
    text = c(
      "product,file_name,size_bytes",
      "prod_a,a.tif,10",
      "prod_a,b.tif,"
    ),
    con = path_csv
  )

  testthat::expect_error(
    load_raster_server_sizes(path_file = path_csv),
    regexp = "positive"
  )

  path_csv_zero <-
    withr::local_tempfile(fileext = ".csv")

  base::writeLines(
    text = c("product,file_name,size_bytes", "prod_a,a.tif,0"),
    con = path_csv_zero
  )

  testthat::expect_error(
    load_raster_server_sizes(path_file = path_csv_zero),
    regexp = "positive"
  )
})

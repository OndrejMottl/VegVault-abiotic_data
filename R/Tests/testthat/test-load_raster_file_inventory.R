#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("load_raster_file_inventory() needs path_file", {
  testthat::expect_error(
    load_raster_file_inventory(
      data_expected_files = tibble::tibble(file_name = "a.tif")
    ),
    regexp = "path_file"
  )

  testthat::expect_error(
    load_raster_file_inventory(
      data_expected_files = tibble::tibble(path_file = NA_character_)
    ),
    regexp = "path_file"
  )

  testthat::expect_error(
    load_raster_file_inventory(data_expected_files = "a.tif"),
    regexp = "path_file"
  )
})


#----------------------------------------------------------#
# Outputs -----
#----------------------------------------------------------#

testthat::test_that("load_raster_file_inventory() adds the columns", {
  dir_root <-
    withr::local_tempdir()

  path_present <-
    base::file.path(dir_root, "present.tif")

  base::writeBin(
    object = base::as.raw(base::seq_len(10)),
    con = path_present
  )

  data_files <-
    tibble::tibble(
      file_name = c("present.tif", "missing.tif"),
      path_file = c(path_present, base::file.path(dir_root, "missing.tif"))
    )

  res <-
    load_raster_file_inventory(data_expected_files = data_files)

  testthat::expect_s3_class(res, "tbl_df")

  testthat::expect_named(
    res,
    c(
      "file_name", "path_file", "observed_size_bytes",
      "observed_modified_time"
    )
  )

  testthat::expect_type(dplyr::pull(res, "observed_size_bytes"), "double")

  testthat::expect_type(
    dplyr::pull(res, "observed_modified_time"),
    "character"
  )
})


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("load_raster_file_inventory() reads sizes and times", {
  dir_root <-
    withr::local_tempdir()

  path_present <-
    base::file.path(dir_root, "present.tif")

  base::writeBin(
    object = base::as.raw(base::seq_len(10)),
    con = path_present
  )

  base::Sys.setFileTime(
    path = path_present,
    time = base::as.POSIXct("2024-09-10 14:56:02", tz = "UTC")
  )

  data_files <-
    tibble::tibble(
      file_name = c("present.tif", "missing.tif"),
      path_file = c(path_present, base::file.path(dir_root, "missing.tif"))
    )

  res <-
    load_raster_file_inventory(data_expected_files = data_files)

  testthat::expect_equal(dplyr::pull(res, "observed_size_bytes"), c(10, NA))

  testthat::expect_equal(
    dplyr::pull(res, "observed_modified_time"),
    c("2024-09-10T14:56:02Z", NA)
  )

  testthat::expect_equal(
    dplyr::pull(res, "file_name"),
    c("present.tif", "missing.tif")
  )
})

testthat::test_that("load_raster_file_inventory() keeps zero rows", {
  res <-
    load_raster_file_inventory(
      data_expected_files = tibble::tibble(path_file = base::character(0))
    )

  testthat::expect_equal(base::nrow(res), 0L)

  testthat::expect_named(
    res,
    c("path_file", "observed_size_bytes", "observed_modified_time")
  )
})

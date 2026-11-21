#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("load_tiff_unsigned_values() reads every size", {
  path_file <-
    base::file.path(withr::local_tempdir(), "values.bin")

  base::writeBin(
    base::as.raw(
      c(
        0xff,
        0xff, 0xff,
        0xff, 0xff, 0xff, 0xff,
        0x05, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00
      )
    ),
    path_file
  )

  con <-
    base::file(path_file, open = "rb")

  withr::defer(base::close(con))

  res_byte <-
    load_tiff_unsigned_values(
      con = con,
      n_values = 1,
      n_bytes = 1,
      endian = "little"
    )

  res_short <-
    load_tiff_unsigned_values(
      con = con,
      n_values = 1,
      n_bytes = 2,
      endian = "little"
    )

  res_long <-
    load_tiff_unsigned_values(
      con = con,
      n_values = 1,
      n_bytes = 4,
      endian = "little"
    )

  res_long8 <-
    load_tiff_unsigned_values(
      con = con,
      n_values = 1,
      n_bytes = 8,
      endian = "little"
    )

  testthat::expect_equal(res_byte, 255)
  testthat::expect_equal(res_short, 65535)
  testthat::expect_equal(res_long, 4294967295)
  testthat::expect_equal(res_long8, 2^40 + 5)
})

testthat::test_that("load_tiff_unsigned_values() reads big-endian files", {
  path_file <-
    base::file.path(withr::local_tempdir(), "values.bin")

  base::writeBin(
    base::as.raw(
      c(
        0x01, 0x02,
        0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x07
      )
    ),
    path_file
  )

  con <-
    base::file(path_file, open = "rb")

  withr::defer(base::close(con))

  res_short <-
    load_tiff_unsigned_values(
      con = con,
      n_values = 1,
      n_bytes = 2,
      endian = "big"
    )

  res_long8 <-
    load_tiff_unsigned_values(
      con = con,
      n_values = 1,
      n_bytes = 8,
      endian = "big"
    )

  testthat::expect_equal(res_short, 258)
  testthat::expect_equal(res_long8, 2^40 + 7)
})

testthat::test_that("load_tiff_unsigned_values() reads several values", {
  path_file <-
    base::file.path(withr::local_tempdir(), "values.bin")

  base::writeBin(
    c(1L, 2L, 3L),
    path_file,
    size = 4,
    endian = "little"
  )

  con <-
    base::file(path_file, open = "rb")

  withr::defer(base::close(con))

  res <-
    load_tiff_unsigned_values(
      con = con,
      n_values = 3,
      n_bytes = 4,
      endian = "little"
    )

  testthat::expect_type(res, "double")
  testthat::expect_equal(res, c(1, 2, 3))
})

testthat::test_that("load_tiff_unsigned_values() reads zero values", {
  path_file <-
    base::file.path(withr::local_tempdir(), "values.bin")

  base::writeBin(1L, path_file, size = 4)

  con <-
    base::file(path_file, open = "rb")

  withr::defer(base::close(con))

  res <-
    load_tiff_unsigned_values(
      con = con,
      n_values = 0,
      n_bytes = 4,
      endian = "little"
    )

  testthat::expect_length(res, 0)
})


#----------------------------------------------------------#
# Errors -----
#----------------------------------------------------------#

testthat::test_that("load_tiff_unsigned_values() stops at the end of file", {
  path_file <-
    base::file.path(withr::local_tempdir(), "values.bin")

  base::writeBin(1L, path_file, size = 2)

  con <-
    base::file(path_file, open = "rb")

  withr::defer(base::close(con))

  testthat::expect_error(
    load_tiff_unsigned_values(
      con = con,
      n_values = 2,
      n_bytes = 4,
      endian = "little"
    ),
    regexp = "end of the file"
  )
})

testthat::test_that("load_tiff_unsigned_values() validates its arguments", {
  path_file <-
    base::file.path(withr::local_tempdir(), "values.bin")

  base::writeBin(1L, path_file, size = 4)

  con <-
    base::file(path_file, open = "rb")

  withr::defer(base::close(con))

  testthat::expect_error(
    load_tiff_unsigned_values(
      con = path_file,
      n_values = 1,
      n_bytes = 4,
      endian = "little"
    ),
    regexp = "con"
  )

  testthat::expect_error(
    load_tiff_unsigned_values(
      con = con,
      n_values = -1,
      n_bytes = 4,
      endian = "little"
    ),
    regexp = "n_values"
  )

  testthat::expect_error(
    load_tiff_unsigned_values(
      con = con,
      n_values = 1,
      n_bytes = 3,
      endian = "little"
    ),
    regexp = "n_bytes"
  )

  testthat::expect_error(
    load_tiff_unsigned_values(
      con = con,
      n_values = 1,
      n_bytes = 4,
      endian = "middle"
    ),
    regexp = "endian"
  )
})

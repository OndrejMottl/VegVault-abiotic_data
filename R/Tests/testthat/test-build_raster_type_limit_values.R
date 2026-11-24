#----------------------------------------------------------#
# Output -----
#----------------------------------------------------------#

testthat::test_that("build_raster_type_limit_values() returns sorted values", {
  res <-
    build_raster_type_limit_values(vec_scales = c(1, 0.1))

  testthat::expect_type(res, "double")
  testthat::expect_equal(res, base::sort(base::unique(res)))
})

testthat::test_that("build_raster_type_limit_values() scales type limits", {
  res <-
    build_raster_type_limit_values(vec_scales = c(1, 0.1, 0.01))

  vec_expected <-
    c(
      -2147483648, 2147483647, 4294967295,
      -32768, 32767, 65535,
      -128, 127, 255,
      -3276.8, 3276.7, 6553.5,
      -12.8, 12.7, 25.5,
      -327.68, 327.67, 655.35,
      -1.28, 1.27, 2.55
    )

  testthat::expect_true(
    base::all(
      purrr::map_lgl(
        .x = vec_expected,
        .f = ~ base::any(base::abs(res - .x) <= 1e-9 * base::abs(.x))
      )
    )
  )
})

testthat::test_that("build_raster_type_limit_values() leaves out zero", {
  res <-
    build_raster_type_limit_values(vec_scales = c(1, 0.1, 0.01))

  testthat::expect_false(base::any(res == 0))
})

testthat::test_that("build_raster_type_limit_values() applies offsets", {
  res <-
    build_raster_type_limit_values(
      vec_scales = 0.1,
      vec_offsets = c(0, -273.15),
      vec_integer_types = "INT2U"
    )

  testthat::expect_equal(
    res,
    c(-273.15, 6280.35, 6553.5),
    tolerance = 1e-9
  )
})

testthat::test_that("build_raster_type_limit_values() selects types", {
  res <-
    build_raster_type_limit_values(
      vec_scales = 1,
      vec_integer_types = c("INT1U", "INT2S")
    )

  testthat::expect_equal(res, c(-32768, 255, 32767))
})


#----------------------------------------------------------#
# Input validation -----
#----------------------------------------------------------#

testthat::test_that("build_raster_type_limit_values() validates scales", {
  testthat::expect_error(
    build_raster_type_limit_values(vec_scales = "0.1"),
    regexp = "vec_scales"
  )

  testthat::expect_error(
    build_raster_type_limit_values(vec_scales = c(0.1, 0)),
    regexp = "vec_scales"
  )

  testthat::expect_error(
    build_raster_type_limit_values(vec_scales = base::numeric(0)),
    regexp = "vec_scales"
  )
})

testthat::test_that("build_raster_type_limit_values() validates offsets", {
  testthat::expect_error(
    build_raster_type_limit_values(
      vec_scales = 1,
      vec_offsets = NA_real_
    ),
    regexp = "vec_offsets"
  )
})

testthat::test_that("build_raster_type_limit_values() validates types", {
  testthat::expect_error(
    build_raster_type_limit_values(
      vec_scales = 1,
      vec_integer_types = "FLT4S"
    ),
    regexp = "vec_integer_types"
  )
})

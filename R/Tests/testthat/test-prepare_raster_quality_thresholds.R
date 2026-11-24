#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("prepare_raster_quality_thresholds() rejects bad lists", {
  testthat::expect_error(
    prepare_raster_quality_thresholds(list_thresholds = "a"),
    regexp = "named list"
  )

  testthat::expect_error(
    prepare_raster_quality_thresholds(list_thresholds = base::list()),
    regexp = "named list"
  )

  testthat::expect_error(
    prepare_raster_quality_thresholds(
      list_thresholds = base::list(base::list(zero_share_max = 1))
    ),
    regexp = "named list"
  )

  testthat::expect_error(
    prepare_raster_quality_thresholds(
      list_thresholds = base::list(prod_a = base::list())
    ),
    regexp = "named list"
  )

  testthat::expect_error(
    prepare_raster_quality_thresholds(
      list_thresholds = base::list(
        prod_a = base::list(zero_share_max = base::list(0.1))
      )
    ),
    regexp = "named list"
  )
})

testthat::test_that("prepare_raster_quality_thresholds() rejects repeats", {
  testthat::expect_error(
    prepare_raster_quality_thresholds(
      list_thresholds = base::list(
        prod_a = base::list(
          zero_share_max = base::list(bio1 = 0.1, bio1 = 0.2)
        )
      )
    ),
    regexp = "repeated"
  )

  testthat::expect_error(
    prepare_raster_quality_thresholds(
      list_thresholds = base::list(
        prod_a = base::list(zero_share_max = base::list(bio1 = 0.1)),
        prod_a = base::list(zero_share_step = base::list(bio1 = 0.1))
      )
    ),
    regexp = "repeated"
  )
})

testthat::test_that("prepare_raster_quality_thresholds() rejects bad values", {
  testthat::expect_error(
    prepare_raster_quality_thresholds(
      list_thresholds = base::list(
        prod_a = base::list(zero_share_max = base::list(bio1 = "0.1"))
      )
    ),
    regexp = "finite number"
  )

  testthat::expect_error(
    prepare_raster_quality_thresholds(
      list_thresholds = base::list(
        prod_a = base::list(zero_share_max = base::list(bio1 = NA_real_))
      )
    ),
    regexp = "finite number"
  )

  testthat::expect_error(
    prepare_raster_quality_thresholds(
      list_thresholds = base::list(
        prod_a = base::list(zero_share_max = base::list(bio1 = Inf))
      )
    ),
    regexp = "finite number"
  )

  testthat::expect_error(
    prepare_raster_quality_thresholds(
      list_thresholds = base::list(
        prod_a = base::list(zero_share_max = base::list(bio1 = c(1, 2)))
      )
    ),
    regexp = "finite number"
  )
})


#----------------------------------------------------------#
# Outputs -----
#----------------------------------------------------------#

testthat::test_that("prepare_raster_quality_thresholds() returns the table", {
  res <-
    prepare_raster_quality_thresholds(
      list_thresholds = base::list(
        prod_a = base::list(zero_share_max = base::list(bio1 = 0.05))
      )
    )

  testthat::expect_s3_class(res, "tbl_df")

  testthat::expect_named(
    res,
    c("product", "variable", "rule", "threshold")
  )

  testthat::expect_type(dplyr::pull(res, "threshold"), "double")

  testthat::expect_type(dplyr::pull(res, "variable"), "character")
})


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("prepare_raster_quality_thresholds() flattens levels", {
  res <-
    prepare_raster_quality_thresholds(
      list_thresholds = base::list(
        prod_b = base::list(
          zero_share_step = base::list(bio4 = 0.2, bio1 = 0),
          pair_exceeding = c(bio18 = 0)
        ),
        prod_a = base::list(
          zero_share_max = base::list(bio1 = 0.131)
        )
      )
    )

  testthat::expect_equal(
    dplyr::pull(res, "product"),
    c("prod_a", "prod_b", "prod_b", "prod_b")
  )

  testthat::expect_equal(
    dplyr::pull(res, "rule"),
    c(
      "zero_share_max", "pair_exceeding",
      "zero_share_step", "zero_share_step"
    )
  )

  testthat::expect_equal(
    dplyr::pull(res, "variable"),
    c("bio1", "bio18", "bio1", "bio4")
  )

  testthat::expect_equal(
    dplyr::pull(res, "threshold"),
    c(0.131, 0, 0, 0.2)
  )
})

testthat::test_that("prepare_raster_quality_thresholds() keeps integers", {
  res <-
    prepare_raster_quality_thresholds(
      list_thresholds = base::list(
        prod_a = base::list(pair_missing_only = base::list(bio18 = 0L))
      )
    )

  testthat::expect_identical(dplyr::pull(res, "threshold"), 0)
})

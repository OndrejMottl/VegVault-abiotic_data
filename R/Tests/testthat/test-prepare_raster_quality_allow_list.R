#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("prepare_raster_quality_allow_list() rejects non-lists", {
  testthat::expect_error(
    prepare_raster_quality_allow_list(list_allow_list = "a"),
    regexp = "must be a list"
  )

  testthat::expect_error(
    prepare_raster_quality_allow_list(
      list_allow_list = base::list("entry")
    ),
    regexp = "must be a list"
  )
})

testthat::test_that("prepare_raster_quality_allow_list() needs the fields", {
  list_entry_valid <-
    base::list(
      product = "prod_a",
      variables = "bio1",
      rules = "type_limit",
      reason = "Known cap."
    )

  testthat::expect_error(
    prepare_raster_quality_allow_list(
      list_allow_list = base::list(
        base::replace(list_entry_valid, "product", base::list(NULL))
      )
    ),
    regexp = "product"
  )

  testthat::expect_error(
    prepare_raster_quality_allow_list(
      list_allow_list = base::list(
        base::replace(list_entry_valid, "variables", base::list(""))
      )
    ),
    regexp = "variables"
  )

  testthat::expect_error(
    prepare_raster_quality_allow_list(
      list_allow_list = base::list(
        base::replace(
          list_entry_valid,
          "rules",
          base::list(base::character(0))
        )
      )
    ),
    regexp = "rules"
  )

  testthat::expect_error(
    prepare_raster_quality_allow_list(
      list_allow_list = base::list(
        base::replace(list_entry_valid, "reason", base::list(""))
      )
    ),
    regexp = "reason"
  )

  testthat::expect_error(
    prepare_raster_quality_allow_list(
      list_allow_list = base::list(
        base::replace(list_entry_valid, "reason", base::list(c("a", "b")))
      )
    ),
    regexp = "reason"
  )
})

testthat::test_that("prepare_raster_quality_allow_list() checks latitudes", {
  list_entry_valid <-
    base::list(
      product = "prod_a",
      variables = "bio1",
      rules = "type_limit",
      reason = "Known cap."
    )

  testthat::expect_error(
    prepare_raster_quality_allow_list(
      list_allow_list = base::list(
        c(list_entry_valid, lat_min = "south")
      )
    ),
    regexp = "lat_min"
  )

  testthat::expect_error(
    prepare_raster_quality_allow_list(
      list_allow_list = base::list(
        c(list_entry_valid, lat_max = Inf)
      )
    ),
    regexp = "lat_max"
  )

  testthat::expect_error(
    prepare_raster_quality_allow_list(
      list_allow_list = base::list(
        c(list_entry_valid, lat_min = -70, lat_max = -80)
      )
    ),
    regexp = "below"
  )
})


#----------------------------------------------------------#
# Outputs -----
#----------------------------------------------------------#

testthat::test_that("prepare_raster_quality_allow_list() returns the table", {
  res <-
    prepare_raster_quality_allow_list(
      list_allow_list = base::list(
        base::list(
          product = "prod_a",
          variables = "bio1",
          rules = "type_limit",
          reason = "Known cap."
        )
      )
    )

  testthat::expect_s3_class(res, "tbl_df")

  testthat::expect_named(
    res,
    c("product", "variable", "rule", "reason", "lat_min", "lat_max")
  )

  testthat::expect_type(dplyr::pull(res, "lat_min"), "double")

  testthat::expect_type(dplyr::pull(res, "lat_max"), "double")
})

testthat::test_that("prepare_raster_quality_allow_list() handles no entry", {
  res <-
    prepare_raster_quality_allow_list(list_allow_list = base::list())

  testthat::expect_equal(base::nrow(res), 0L)

  testthat::expect_named(
    res,
    c("product", "variable", "rule", "reason", "lat_min", "lat_max")
  )

  testthat::expect_type(dplyr::pull(res, "reason"), "character")

  testthat::expect_type(dplyr::pull(res, "lat_min"), "double")
})


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("prepare_raster_quality_allow_list() expands entries", {
  res <-
    prepare_raster_quality_allow_list(
      list_allow_list = base::list(
        base::list(
          product = "prod_a",
          variables = c("bio12", "bio18"),
          rules = c("zero_share_max", "zero_share_step"),
          reason = "Antarctica.",
          lat_min = -91,
          lat_max = -74
        ),
        base::list(
          product = "prod_b",
          variables = "bio18",
          rules = "pair_exceeding",
          reason = "Source inconsistency."
        )
      )
    )

  testthat::expect_equal(base::nrow(res), 5L)

  testthat::expect_equal(
    dplyr::pull(res, "variable"),
    c("bio12", "bio12", "bio18", "bio18", "bio18")
  )

  testthat::expect_equal(
    dplyr::pull(res, "rule"),
    c(
      "zero_share_max", "zero_share_step",
      "zero_share_max", "zero_share_step",
      "pair_exceeding"
    )
  )

  testthat::expect_equal(
    dplyr::pull(res, "lat_min"),
    c(-91, -91, -91, -91, NA)
  )

  testthat::expect_equal(
    dplyr::pull(res, "lat_max"),
    c(-74, -74, -74, -74, NA)
  )

  testthat::expect_equal(
    dplyr::pull(res, "reason"),
    c(base::rep("Antarctica.", 4), "Source inconsistency.")
  )
})

testthat::test_that("prepare_raster_quality_allow_list() takes NULL limits", {
  res <-
    prepare_raster_quality_allow_list(
      list_allow_list = base::list(
        base::list(
          product = "prod_a",
          variables = "bio15",
          rules = "band_all_missing",
          reason = "No precipitation.",
          lat_min = NULL,
          lat_max = -80L
        )
      )
    )

  testthat::expect_equal(dplyr::pull(res, "lat_min"), NA_real_)

  testthat::expect_identical(dplyr::pull(res, "lat_max"), -80)
})

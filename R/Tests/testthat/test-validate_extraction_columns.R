#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("validate_extraction_columns() accepts valid columns", {
  testthat::expect_true(
    validate_extraction_columns(
      vec_columns = c("long", "lat", "time_id", "value")
    )
  )

  testthat::expect_true(
    validate_extraction_columns(
      vec_columns = c("long", "lat", "value", "var_name"),
      list_var_names = base::list(bio1 = "bio_01"),
      vec_variables = "bio1"
    )
  )

  testthat::expect_invisible(
    validate_extraction_columns(vec_columns = c("value", "lat", "long"))
  )
})


#----------------------------------------------------------#
# Failing closed -----
#----------------------------------------------------------#

testthat::test_that("validate_extraction_columns() rejects wrong columns", {
  testthat::expect_error(
    validate_extraction_columns(vec_columns = c("long", "lat", "time_id")),
    regexp = "columns"
  )

  testthat::expect_error(
    validate_extraction_columns(vec_columns = c("long", "lat", "value", "age")),
    regexp = "columns"
  )

  testthat::expect_error(
    validate_extraction_columns(
      vec_columns = c("long", "lat", "value", "value")
    ),
    regexp = "columns"
  )

  testthat::expect_error(
    validate_extraction_columns(vec_columns = NULL),
    regexp = "columns"
  )
})

testthat::test_that("validate_extraction_columns() needs the labels", {
  testthat::expect_error(
    validate_extraction_columns(
      vec_columns = c("long", "lat", "value", "var_name")
    ),
    regexp = "var_names"
  )

  testthat::expect_error(
    validate_extraction_columns(
      vec_columns = c("long", "lat", "value", "var_name"),
      list_var_names = base::list("bio_01")
    ),
    regexp = "var_names"
  )

  testthat::expect_error(
    validate_extraction_columns(
      vec_columns = c("long", "lat", "value", "var_name"),
      list_var_names = base::list(other = "HWSD2"),
      vec_variables = "soil_group",
      label = "`hwsd`"
    ),
    regexp = "`hwsd`.*var_names"
  )
})

#----------------------------------------------------------#
# Fixtures -----
#----------------------------------------------------------#

list_sources_valid <-
  base::list(
    neo = base::list(
      product = "prod_neo",
      dir = "Input/Neo",
      file_name_template = "neo_{file_variable}.tif",
      url_template = "https://example.org/{file_name}",
      variables = base::list(bio1 = "bio1"),
      ages = base::list(from = 0, to = 0, by = 1),
      file_check = "geotiff",
      flag_download = FALSE
    ),
    palaeo = base::list(
      product = "prod_palaeo",
      dir = "Input/Palaeo",
      file_name_template = "palaeo_{file_variable}_{time_id}.tif",
      variables = base::list(bio1 = "bio01"),
      ages = base::list(from = 0, to = 500, by = 500),
      time_id = base::list(origin_age = 2000, step_years = 100)
    ),
    arch = base::list(
      product = "prod_arch",
      dir = "Input/Arch",
      files = base::list(a.zip = "https://example.org/a.zip")
    )
  )

list_sets_valid <-
  base::list(
    neo_raw = base::list(
      source = "neo",
      required_pass = TRUE,
      grid = "raw"
    ),
    neo_legacy = base::list(
      product = "prod_neo",
      required_pass = FALSE,
      dir = "Input/Legacy",
      file_name_template = "legacy_{file_variable}.tif",
      variables = base::list(bio1 = "bio_01"),
      ages = base::list(from = 0, to = 0, by = 1),
      grid = "raw"
    ),
    palaeo_raw = base::list(
      source = "palaeo",
      required_pass = TRUE,
      grid = "raw"
    )
  )


#----------------------------------------------------------#
# Inputs -----
#----------------------------------------------------------#

testthat::test_that("prepare_quality_check_sets() validates its arguments", {
  testthat::expect_error(
    prepare_quality_check_sets(
      list_check_sets = base::list(1),
      list_data_sources = list_sources_valid
    ),
    regexp = "list_check_sets"
  )

  testthat::expect_error(
    prepare_quality_check_sets(
      list_check_sets = list_sets_valid,
      list_data_sources = "neo"
    ),
    regexp = "list_data_sources"
  )
})


#----------------------------------------------------------#
# Functionality -----
#----------------------------------------------------------#

testthat::test_that("prepare_quality_check_sets() fills sets from sources", {
  res <-
    prepare_quality_check_sets(
      list_check_sets = list_sets_valid,
      list_data_sources = list_sources_valid
    )

  testthat::expect_named(res, c("neo_raw", "neo_legacy", "palaeo_raw"))

  list_neo <-
    res[["neo_raw"]]

  testthat::expect_false("source" %in% base::names(list_neo))
  testthat::expect_equal(list_neo[["product"]], "prod_neo")
  testthat::expect_equal(list_neo[["dir"]], "Input/Neo")
  testthat::expect_equal(
    list_neo[["file_name_template"]],
    "neo_{file_variable}.tif"
  )
  testthat::expect_equal(list_neo[["variables"]], base::list(bio1 = "bio1"))
  testthat::expect_true(list_neo[["required_pass"]])
  testthat::expect_equal(list_neo[["grid"]], "raw")
  testthat::expect_false("flag_download" %in% base::names(list_neo))
  testthat::expect_false("time_id" %in% base::names(list_neo))

  testthat::expect_equal(
    res[["palaeo_raw"]][["time_id"]],
    base::list(origin_age = 2000, step_years = 100)
  )

  testthat::expect_identical(
    res[["neo_legacy"]],
    list_sets_valid[["neo_legacy"]]
  )
})


#----------------------------------------------------------#
# Failing closed -----
#----------------------------------------------------------#

testthat::test_that("prepare_quality_check_sets() needs a known source", {
  testthat::expect_error(
    prepare_quality_check_sets(
      list_check_sets = base::list(
        x_raw = base::list(source = "none", required_pass = TRUE)
      ),
      list_data_sources = list_sources_valid
    ),
    regexp = "none"
  )
})

testthat::test_that("prepare_quality_check_sets() needs a templated source", {
  testthat::expect_error(
    prepare_quality_check_sets(
      list_check_sets = base::list(
        arch_raw = base::list(source = "arch", required_pass = TRUE)
      ),
      list_data_sources = list_sources_valid
    ),
    regexp = "file_name_template"
  )
})

testthat::test_that("prepare_quality_check_sets() refuses two definitions", {
  testthat::expect_error(
    prepare_quality_check_sets(
      list_check_sets = base::list(
        neo_raw = base::list(
          source = "neo",
          variables = base::list(bio1 = "bio_01"),
          required_pass = TRUE
        )
      ),
      list_data_sources = list_sources_valid
    ),
    regexp = "variables"
  )
})

testthat::test_that("prepare_quality_check_sets() keeps an own folder", {
  res <-
    prepare_quality_check_sets(
      list_check_sets = base::list(
        neo_processed = base::list(
          source = "neo",
          dir = "Processed/Neo",
          required_pass = TRUE
        )
      ),
      list_data_sources = list_sources_valid
    )

  list_set <-
    res[["neo_processed"]]

  testthat::expect_equal(list_set[["dir"]], "Processed/Neo")
  testthat::expect_equal(
    list_set[["file_name_template"]],
    "neo_{file_variable}.tif"
  )
  testthat::expect_equal(base::sum(base::names(list_set) == "dir"), 1L)
})

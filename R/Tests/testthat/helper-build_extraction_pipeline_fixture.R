#' @title Build a small extraction-stage pipeline for tests
#' @description
#' Write a data root with small processed rasters, an upstream store that
#' stands in for the processing stage, the extraction configuration and a
#' pipeline script that declares `build_climate_extraction_targets()` for
#' two products and `build_hwsd_extraction_targets()`, so tests can run the
#' stage with `run_target_pipeline()`.
#' @param dir_root
#' Character scalar. An empty temporary folder to write into.
#' @param flag_upstream
#' Logical. Build the upstream store. Default `TRUE`; `FALSE` leaves it
#' missing.
#' @return
#' A list with `path_data_storage`, `path_config`, `path_script`,
#' `path_store`, `path_store_upstream`, `list_fixture` (the content of the
#' RDS file) and `list_paths` (the processed rasters: `neo_bio1`,
#' `neo_bio12`, `palaeo_bio12_20`, `palaeo_bio12_15` and `hwsd`).
#' @details
#' Rasters have 2 rows and 4 columns of 90 degrees over the globe, so the
#' cell centres are 135 W, 45 W, 45 E and 135 E at 45 N and 45 S. Product
#' `neo` has bio1 with the values 1 to 8 and bio12 with 10 to 80, missing in
#' cells 2 and 7. Product `palaeo` has bio12 at ages 0 (`time_id` 20, values
#' 100 to 800) and 500 (`time_id` 15, values 101 to 801, missing in cell 3).
#' The soil map holds the codes 1, 19, missing, 1, missing, 19, 19, 1 as
#' unsigned 8-bit integers. The upstream `path_file` columns carry branch
#' names, as the processing stage returns them.
build_extraction_pipeline_fixture <- function(
  dir_root,
  flag_upstream = TRUE
) {
  dir_root_normalised <-
    base::normalizePath(dir_root, winslash = "/")

  path_data_storage <-
    base::file.path(dir_root_normalised, "Data")

  dir_processed <-
    base::file.path(path_data_storage, "Processed")

  base::dir.create(path = dir_processed, recursive = TRUE)

  list_paths <-
    base::list(
      neo_bio1 = base::file.path(dir_processed, "neo_bio1.tif"),
      neo_bio12 = base::file.path(dir_processed, "neo_bio12.tif"),
      palaeo_bio12_20 = base::file.path(dir_processed, "palaeo_bio12_20.tif"),
      palaeo_bio12_15 = base::file.path(dir_processed, "palaeo_bio12_15.tif"),
      hwsd = base::file.path(dir_processed, "hwsd_soil_groups.tif")
    )

  build_quality_raster_fixture(
    path_file = list_paths[["neo_bio1"]],
    mat_values = base::matrix(1:8, nrow = 2, ncol = 4, byrow = TRUE)
  )

  build_quality_raster_fixture(
    path_file = list_paths[["neo_bio12"]],
    mat_values = base::matrix(
      c(10, NA, 30, 40, 50, 60, NA, 80),
      nrow = 2,
      ncol = 4,
      byrow = TRUE
    )
  )

  build_quality_raster_fixture(
    path_file = list_paths[["palaeo_bio12_20"]],
    mat_values = base::matrix(
      base::seq(from = 100, to = 800, by = 100),
      nrow = 2,
      ncol = 4,
      byrow = TRUE
    )
  )

  build_quality_raster_fixture(
    path_file = list_paths[["palaeo_bio12_15"]],
    mat_values = base::matrix(
      c(101, 201, NA, 401, 501, 601, 701, 801),
      nrow = 2,
      ncol = 4,
      byrow = TRUE
    )
  )

  # The soil map is stored as unsigned 8-bit integers, as in the stage
  rast_soil <-
    terra::rast(
      nrows = 2,
      ncols = 4,
      xmin = -180,
      xmax = 180,
      ymin = -90,
      ymax = 90,
      crs = "EPSG:4326",
      vals = c(1, 19, NA, 1, NA, 19, 19, 1)
    )

  terra::writeRaster(
    x = rast_soil,
    filename = list_paths[["hwsd"]],
    datatype = "INT1U"
  )

  # Upstream values the processing stage would provide
  list_upstream <-
    base::list(
      neo_processed_files = tibble::tibble(
        product = "prod_neo",
        variable = c("bio1", "bio12"),
        age = 0,
        time_id = NA_integer_,
        file_name = c("neo_bio1.tif", "neo_bio12.tif"),
        path_file = c(
          neo_final_a = list_paths[["neo_bio1"]],
          neo_final_b = list_paths[["neo_bio12"]]
        )
      ),
      palaeo_processed_files = tibble::tibble(
        product = "prod_palaeo",
        variable = "bio12",
        age = c(0, 500),
        time_id = c(20L, 15L),
        file_name = c("palaeo_bio12_20.tif", "palaeo_bio12_15.tif"),
        path_file = c(
          palaeo_final_a = list_paths[["palaeo_bio12_20"]],
          palaeo_final_b = list_paths[["palaeo_bio12_15"]]
        )
      ),
      hwsd_soil_raster = list_paths[["hwsd"]]
    )

  path_upstream_values <-
    base::file.path(dir_root_normalised, "upstream.rds")

  base::saveRDS(object = list_upstream, file = path_upstream_values)

  path_script_upstream <-
    base::file.path(dir_root_normalised, "upstream.R")

  base::writeLines(
    text = c(
      stringr::str_glue(
        "list_upstream <- base::readRDS('{path_upstream_values}')"
      ),
      "purrr::imap(",
      "  .x = list_upstream,",
      "  .f = ~ targets::tar_target_raw(",
      "    name = .y,",
      "    command = rlang::expr(base::identity(!!.x))",
      "  )",
      ")"
    ),
    con = path_script_upstream
  )

  path_store_upstream <-
    base::file.path(path_data_storage, "Targets_data", "upstream")

  if (
    isTRUE(flag_upstream)
  ) {
    run_target_pipeline(
      script = path_script_upstream,
      store = path_store_upstream,
      verbose = FALSE
    )
  }

  list_data_extraction <-
    base::list(
      climate_products = base::list(
        neo = base::list(
          store_processing = "upstream",
          dir = "Processed/Neo/Cell_tables",
          columns = c("long", "lat", "value", "var_name"),
          var_names = base::list(bio1 = "bio_01", bio12 = "bio_12")
        ),
        palaeo = base::list(
          store_processing = "upstream",
          dir = "Processed/Palaeo/Cell_tables",
          columns = c("long", "lat", "time_id", "value")
        )
      ),
      hwsd = base::list(
        store_processing = "upstream",
        product = "prod_hwsd",
        variable = "soil_group",
        dir = "Processed/Hwsd/Cell_tables",
        columns = c("long", "lat", "value", "var_name"),
        var_names = base::list(soil_group = "HWSD2")
      )
    )

  list_fixture <-
    base::list(list_data_extraction = list_data_extraction)

  path_config <-
    base::file.path(dir_root_normalised, "config.rds")

  base::saveRDS(object = list_fixture, file = path_config)

  vec_path_functions <-
    base::normalizePath(
      here::here("R", "Functions", c("Data_extraction", "Workflow")),
      winslash = "/"
    )

  text_path_functions <-
    stringr::str_c("'", vec_path_functions, "'", collapse = ", ")

  path_script <-
    base::file.path(dir_root_normalised, "pipeline.R")

  base::writeLines(
    text = c(
      "purrr::walk(",
      "  .x = base::list.files(",
      stringr::str_glue("    path = c({text_path_functions}),"),
      "    pattern = '[.]R$',",
      "    recursive = TRUE,",
      "    full.names = TRUE",
      "  ),",
      "  .f = ~ base::source(file = .x, encoding = 'UTF-8')",
      ")",
      stringr::str_glue("list_fixture <- base::readRDS('{path_config}')"),
      "list_extraction <- list_fixture[['list_data_extraction']]",
      "c(",
      "  purrr::map(",
      "    .x = c('neo', 'palaeo'),",
      "    .f = ~ build_climate_extraction_targets(",
      "      sel_product = .x,",
      "      list_data_extraction = list_extraction,",
      stringr::str_glue("      path_data_storage = '{path_data_storage}'"),
      "    )",
      "  ) |>",
      "    purrr::list_flatten(),",
      "  build_hwsd_extraction_targets(",
      "    list_data_extraction = list_extraction,",
      stringr::str_glue("    path_data_storage = '{path_data_storage}'"),
      "  )",
      ")"
    ),
    con = path_script
  )

  res <-
    base::list(
      path_data_storage = path_data_storage,
      path_config = path_config,
      path_script = path_script,
      path_store = base::file.path(
        path_data_storage, "Targets_data", "extraction"
      ),
      path_store_upstream = path_store_upstream,
      list_fixture = list_fixture,
      list_paths = list_paths
    )

  return(res)
}

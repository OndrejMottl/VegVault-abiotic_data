#' @title Build a small raster quality-check pipeline for tests
#' @description
#' Write a data root with three small global rasters, a server-size
#' record, the quality-check configuration and a pipeline script that
#' declares `build_raster_quality_targets()`, so tests can run the whole
#' check with `run_target_pipeline()`.
#' @param dir_root
#' Character scalar. An empty temporary folder to write into.
#' @return
#' A list with `path_data_storage`, `dir_tables`, `path_config` (an RDS
#' file holding `vec_check_sets` and `list_quality_checks`), `path_script`,
#' `path_store`, `path_sizes` (the server-size CSV), `path_bio12`,
#' `path_bio18`, `path_bio4` and `list_fixture` (the content of the RDS
#' file).
#' @details
#' The rasters have 18 rows and 36 columns over the globe, so with
#' `band_width = 10` every row is one band. Set `neo_new` (required, with
#' server sizes) has bio12 at 100 and bio18 at 30 everywhere and pairs
#' bio18 with bio12; it passes. Set `neo_legacy` (not required) has bio4
#' at 50 north of the equator and 0 south of it; it fails the zero-share
#' rules. The pipeline script sources the function files of
#' `R/Functions/Data_acquisition/`, `R/Functions/Quality_checks/` and
#' `R/Functions/Workflow/`, because `run_target_pipeline()` runs it in a
#' new R session.
build_quality_pipeline_fixture <- function(dir_root) {
  # Forward slashes, because the paths are written into the script text
  dir_root_normalised <-
    base::normalizePath(dir_root, winslash = "/")

  path_data_storage <-
    base::file.path(dir_root_normalised, "Data")

  base::dir.create(
    path = base::file.path(path_data_storage, "Input", "Neo_new"),
    recursive = TRUE
  )

  base::dir.create(
    path = base::file.path(path_data_storage, "Input", "Neo_legacy"),
    recursive = TRUE
  )

  path_bio12 <-
    base::file.path(path_data_storage, "Input", "Neo_new", "neo_bio12.tif")

  path_bio18 <-
    base::file.path(path_data_storage, "Input", "Neo_new", "neo_bio18.tif")

  path_bio4 <-
    base::file.path(
      path_data_storage, "Input", "Neo_legacy", "legacy_bio_04.tif"
    )

  build_quality_raster_fixture(
    path_file = path_bio12,
    mat_values = base::matrix(100, nrow = 18, ncol = 36)
  )

  build_quality_raster_fixture(
    path_file = path_bio18,
    mat_values = base::matrix(30, nrow = 18, ncol = 36)
  )

  build_quality_raster_fixture(
    path_file = path_bio4,
    mat_values = base::rbind(
      base::matrix(50, nrow = 9, ncol = 36),
      base::matrix(0, nrow = 9, ncol = 36)
    )
  )

  path_sizes <-
    base::file.path(path_data_storage, "Input", "server_sizes.csv")

  readr::write_csv(
    x = tibble::tibble(
      product = "prod_neo",
      file_name = c("neo_bio12.tif", "neo_bio18.tif"),
      size_bytes = base::file.size(c(path_bio12, path_bio18))
    ),
    file = path_sizes
  )

  list_quality_checks <-
    base::list(
      band_width = 10,
      n_rows_block = 4L,
      value_tolerance = 1e-6,
      extent_tolerance = 1e-6,
      type_limits = base::list(
        scales = 1,
        offsets = 0,
        integer_types = c("INT1U", "INT2U")
      ),
      server_sizes_file = "Input/server_sizes.csv",
      grids = base::list(
        global = base::list(
          n_col = 36, n_row = 18,
          x_min = -180, x_max = 180, y_min = -90, y_max = 90
        )
      ),
      thresholds = base::list(
        prod_neo = base::list(
          zero_share_max = base::list(
            bio4 = 0.05, bio12 = 0.05, bio18 = 0.05
          ),
          zero_share_step = base::list(
            bio4 = 0.05, bio12 = 0.05, bio18 = 0.05
          ),
          pair_exceeding = base::list(bio18 = 0),
          pair_missing_only = base::list(bio18 = 0)
        )
      ),
      allow_list = base::list(),
      check_sets = base::list(
        neo_new = base::list(
          product = "prod_neo",
          required_pass = TRUE,
          dir = "Input/Neo_new",
          file_name_template = "neo_{file_variable}.tif",
          variables = base::list(bio12 = "bio12", bio18 = "bio18"),
          ages = base::list(from = 0, to = 0, by = 1),
          grid = "global",
          check_server_size = TRUE,
          pairs = base::list(
            base::list(variable = "bio18", reference = "bio12")
          )
        ),
        neo_legacy = base::list(
          product = "prod_neo",
          required_pass = FALSE,
          dir = "Input/Neo_legacy",
          file_name_template = "legacy_{file_variable}.tif",
          variables = base::list(bio4 = "bio_04"),
          ages = base::list(from = 0, to = 0, by = 1),
          grid = "global",
          check_server_size = FALSE,
          pairs = base::list()
        )
      )
    )

  list_fixture <-
    base::list(
      vec_check_sets = c("neo_new", "neo_legacy"),
      list_quality_checks = list_quality_checks
    )

  path_config <-
    base::file.path(dir_root_normalised, "config.rds")

  base::saveRDS(
    object = list_fixture,
    file = path_config
  )

  dir_tables <-
    base::file.path(dir_root_normalised, "Tables")

  # Only the subfolders the pipeline needs: the copied contract functions
  #   in `R/Functions/Contracts/` make `{targets}` print code-analysis
  #   warnings. The quality checks build their file lists with
  #   `Data_acquisition` code
  vec_path_functions <-
    base::normalizePath(
      here::here(
        "R", "Functions", c("Data_acquisition", "Quality_checks", "Workflow")
      ),
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
      "build_raster_quality_targets(",
      "  vec_check_sets = list_fixture[['vec_check_sets']],",
      "  list_quality_checks = list_fixture[['list_quality_checks']],",
      stringr::str_glue("  path_data_storage = '{path_data_storage}',"),
      stringr::str_glue("  dir_tables = '{dir_tables}'"),
      ")"
    ),
    con = path_script
  )

  res <-
    base::list(
      path_data_storage = path_data_storage,
      dir_tables = dir_tables,
      path_config = path_config,
      path_script = path_script,
      path_store = base::file.path(dir_root_normalised, "store"),
      path_sizes = path_sizes,
      path_bio12 = path_bio12,
      path_bio18 = path_bio18,
      path_bio4 = path_bio4,
      list_fixture = list_fixture
    )

  return(res)
}

#' @title Build a small download-stage pipeline for tests
#' @description
#' Write a local server folder (see `build_raw_source_fixture()`), an
#' empty data root with a server-size record, the source configuration
#' and a pipeline script that declares `build_source_acquisition_targets()`
#' for two sources, so tests can run the stage with
#' `run_target_pipeline()`.
#' @param dir_root
#' Character scalar. An empty temporary folder to write into.
#' @return
#' A list with `path_data_storage`, `dir_records`, `path_config` (an RDS
#' file holding `list_data_sources` and `list_data_acquisition`),
#' `path_script`, `path_store`, `list_server` (the result of
#' `build_raw_source_fixture()`) and `list_fixture` (the content of the
#' RDS file).
#' @details
#' Source `neo` (GeoTIFF, product `prod_neo`) has the variables bio1 and
#' bio12, file names `neo_<variable>.tif` and the recorded server sizes.
#' Source `arch` (zip, product `prod_arch`) has the one file `archive.zip`
#' with the members `db.mdb` and `raster.bil`. All flags are `FALSE`, so
#' the first run finds no file; tests change the flags in the RDS file.
#' The script sources the function files of `R/Functions/Data_acquisition/`,
#' `R/Functions/Quality_checks/` and `R/Functions/Workflow/`, because
#' `run_target_pipeline()` runs it in a new R session.
build_acquisition_pipeline_fixture <- function(dir_root) {
  dir_root_normalised <-
    base::normalizePath(dir_root, winslash = "/")

  list_server <-
    build_raw_source_fixture(dir_root = dir_root_normalised)

  path_data_storage <-
    base::file.path(dir_root_normalised, "Data")

  base::dir.create(
    path = base::file.path(path_data_storage, "Input", "Download_records"),
    recursive = TRUE
  )

  readr::write_csv(
    x = tibble::tibble(
      product = "prod_neo",
      file_name = c("neo_bio1.tif", "neo_bio12.tif"),
      size_bytes = c(list_server[["size_bio1"]], list_server[["size_bio12"]])
    ),
    file = base::file.path(path_data_storage, "Input", "server_sizes.csv")
  )

  list_data_sources <-
    base::list(
      neo = base::list(
        product = "prod_neo",
        dir = "Input/Neo",
        file_name_template = "neo_{file_variable}.tif",
        url_template = stringr::str_c(
          "file:///",
          stringr::str_remove(list_server[["dir_server"]], "^/"),
          "/{file_name}"
        ),
        variables = base::list(bio1 = "bio1", bio12 = "bio12"),
        ages = base::list(from = 0, to = 0, by = 1),
        server_sizes_file = "Input/server_sizes.csv",
        file_check = "geotiff",
        flag_download = FALSE,
        flag_record_checksums = FALSE
      ),
      arch = base::list(
        product = "prod_arch",
        dir = "Input/Arch",
        files = base::list(archive.zip = list_server[["url_archive"]]),
        archive_members = base::list(
          archive.zip = list_server[["vec_members"]]
        ),
        file_check = "zip",
        flag_download = FALSE,
        flag_record_checksums = FALSE
      )
    )

  list_data_acquisition <-
    base::list(
      dir_records = "Input/Download_records",
      dir_download = "Temp/Downloads"
    )

  list_fixture <-
    base::list(
      list_data_sources = list_data_sources,
      list_data_acquisition = list_data_acquisition
    )

  path_config <-
    base::file.path(dir_root_normalised, "config.rds")

  base::saveRDS(
    object = list_fixture,
    file = path_config
  )

  # Only the template subfolders: the legacy functions directly in
  #   `R/Functions/` make `{targets}` print code-analysis warnings
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
      "purrr::map(",
      "  .x = c('neo', 'arch'),",
      "  .f = ~ build_source_acquisition_targets(",
      "    sel_source = .x,",
      "    list_data_sources = list_fixture[['list_data_sources']],",
      "    list_data_acquisition = list_fixture[['list_data_acquisition']],",
      stringr::str_glue("    path_data_storage = '{path_data_storage}'"),
      "  )",
      ") |>",
      "  purrr::list_flatten()"
    ),
    con = path_script
  )

  res <-
    base::list(
      path_data_storage = path_data_storage,
      dir_records = base::file.path(
        path_data_storage, "Input", "Download_records"
      ),
      path_config = path_config,
      path_script = path_script,
      path_store = base::file.path(dir_root_normalised, "store"),
      list_server = list_server,
      list_fixture = list_fixture
    )

  return(res)
}

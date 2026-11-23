#' @title Build a small export-stage pipeline for tests
#' @description
#' Write a project folder with the copy of the VegVault contracts, a data
#' root with small cell tables, upstream stores that stand in for the
#' extraction stage and the quality check, the export configuration and a
#' pipeline script that declares `build_export_targets()`, so tests can run
#' the stage with `run_target_pipeline()`.
#' @param dir_root
#' Character scalar. An empty temporary folder to write into.
#' @param flag_quality_passed
#' Logical. `TRUE` (default) builds a quality-check store whose public
#' target completed; `FALSE` builds one whose public target errored.
#' @param flag_replace_previous
#' Logical. The `flag_replace_previous` of the configuration. Default
#' `FALSE`.
#' @return
#' A list with `path_project`, `path_data_storage`, `path_config`,
#' `path_script`, `path_store`, `list_fixture` (the content of the RDS
#' file) and `list_cell_tables` (as from
#' `build_export_cell_tables_fixture()`).
#' @details
#' The project folder holds `Data/Contracts/` copied from the repository,
#' so the export is checked against the real contracts. The upstream store
#' `upstream` holds `neo_extracted_files`, `palaeo_extracted_files` and
#' `hwsd_extracted_files`; the store `upstream_qc` holds
#' `raster_quality_validated`. The products `neo`, `palaeo` and `hwsd` use
#' the contracts `chelsa_neoclimate`, `chelsa_trace21k_palaeoclimate` and
#' `hwsd_soil`. The soil-group codes are AC 1, CR 9, DU 10 and LV 19. The
#' WRB lookup lists AC, CR and LV in another order with GG (Glaciers) but
#' not DU, which the configuration names Durisols, as for HWSD v2.2.
build_export_pipeline_fixture <- function(
  dir_root,
  flag_quality_passed = TRUE,
  flag_replace_previous = FALSE
) {
  dir_root_normalised <-
    base::normalizePath(dir_root, winslash = "/")

  path_project <-
    base::file.path(dir_root_normalised, "Project")

  dir_contracts <-
    base::file.path(path_project, "Data", "Contracts")

  base::dir.create(path = dir_contracts, recursive = TRUE)

  base::file.copy(
    from = base::list.files(
      path = here::here("Data", "Contracts"),
      pattern = "[.]yml$",
      full.names = TRUE
    ),
    to = dir_contracts
  )

  path_data_storage <-
    base::file.path(dir_root_normalised, "Data")

  list_cell_tables <-
    build_export_cell_tables_fixture(
      dir_root = base::file.path(path_data_storage, "Processed")
    )

  dir_lookup <-
    base::file.path(path_data_storage, "Input", "HWSD_tables")

  base::dir.create(path = dir_lookup, recursive = TRUE)

  path_wrb_lookup <-
    base::file.path(dir_lookup, "d_wrb2.csv")

  readr::write_csv(
    x = tibble::tibble(
      CODE = c("LV", "GG", "AC", "CR"),
      Value = c("Luvisols", "Glaciers", "Acrisols", "Cryosols")
    ),
    file = path_wrb_lookup
  )

  # Upstream values the extraction stage and the quality check provide
  list_upstream <-
    base::list(
      neo_extracted_files = purrr::chuck(list_cell_tables, "neo"),
      palaeo_extracted_files = purrr::chuck(list_cell_tables, "palaeo"),
      hwsd_extracted_files = purrr::chuck(list_cell_tables, "hwsd")
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

  run_target_pipeline(
    script = path_script_upstream,
    store = base::file.path(path_data_storage, "Targets_data", "upstream"),
    verbose = FALSE
  )

  path_script_quality <-
    base::file.path(dir_root_normalised, "upstream_qc.R")

  text_quality_command <-
    if (
      isTRUE(flag_quality_passed)
    ) {
      "tibble::tibble(rule = 'file_present', status = 'pass')"
    } else {
      "cli::cli_abort('A required set failed.')"
    }

  base::writeLines(
    text = c(
      "list(",
      "  targets::tar_target(",
      "    name = 'raster_quality_validated',",
      stringr::str_glue("    command = {text_quality_command}"),
      "  )",
      ")"
    ),
    con = path_script_quality
  )

  # A failing quality check leaves an errored target in its store
  base::try(
    expr = run_target_pipeline(
      script = path_script_quality,
      store = base::file.path(path_data_storage, "Targets_data", "upstream_qc"),
      verbose = FALSE
    ),
    silent = TRUE
  )

  list_data_export <-
    base::list(
      store = "export",
      dir_contracts = "Data/Contracts",
      dir_data_contract = "Outputs/Data",
      dir_data = "Outputs/Data",
      dir_tables = "Outputs/Tables",
      flag_replace_previous = flag_replace_previous,
      soil_group_names_missing = base::list(DU = "Durisols"),
      products = base::list(
        neo = base::list(
          store_extraction = "upstream",
          target_extraction = "neo_extracted_files",
          contract = "chelsa_neoclimate"
        ),
        palaeo = base::list(
          store_extraction = "upstream",
          target_extraction = "palaeo_extracted_files",
          contract = "chelsa_trace21k_palaeoclimate"
        ),
        hwsd = base::list(
          store_extraction = "upstream",
          target_extraction = "hwsd_extracted_files",
          contract = "hwsd_soil"
        )
      ),
      quality_check = base::list(
        store = "upstream_qc",
        target = "raster_quality_validated"
      )
    )

  list_fixture <-
    base::list(
      list_data_export = list_data_export,
      list_soil_group_codes = base::list(AC = 1L, CR = 9L, DU = 10L, LV = 19L),
      path_wrb_lookup = path_wrb_lookup
    )

  path_config <-
    base::file.path(dir_root_normalised, "config.rds")

  base::saveRDS(object = list_fixture, file = path_config)

  vec_path_functions <-
    base::normalizePath(
      here::here("R", "Functions", c("Data_export", "Workflow", "Contracts")),
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
      "build_export_targets(",
      "  list_data_export = list_fixture[['list_data_export']],",
      stringr::str_glue("  path_data_storage = '{path_data_storage}',"),
      stringr::str_glue("  path_project = '{path_project}',"),
      "  list_soil_group_codes = list_fixture[['list_soil_group_codes']],",
      "  path_wrb_lookup = list_fixture[['path_wrb_lookup']]",
      ")"
    ),
    con = path_script
  )

  res <-
    base::list(
      path_project = path_project,
      path_data_storage = path_data_storage,
      path_config = path_config,
      path_script = path_script,
      path_store = base::file.path(
        path_data_storage, "Targets_data", "export"
      ),
      list_fixture = list_fixture,
      list_cell_tables = list_cell_tables
    )

  return(res)
}

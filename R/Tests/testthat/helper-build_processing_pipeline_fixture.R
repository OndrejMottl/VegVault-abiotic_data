#' @title Build a small processing-stage pipeline for tests
#' @description
#' Write a data root with small rasters, an upstream store that stands in
#' for the download stage, the processing configuration and a pipeline
#' script that declares `build_climate_processing_targets()` for two
#' products and `build_hwsd_processing_targets()`, so tests can run the
#' stage with `run_target_pipeline()`.
#' @param dir_root
#' Character scalar. An empty temporary folder to write into.
#' @param flag_upstream
#' Logical. Build the upstream store. Default `TRUE`; `FALSE` leaves it
#' missing.
#' @return
#' A list with `path_data_storage`, `dir_tables`, `path_config`,
#' `path_script`, `path_store`, `path_script_upstream`,
#' `path_store_upstream` and `list_fixture` (the content of the RDS file).
#' @details
#' Rasters have 8 rows and 16 columns over the globe and the aggregation
#' factor is 2, so the processed grid has 4 by 8 cells of 45 degrees.
#' Product `neo` (bio1 at 10, bio12 at 1000) has the fill value 6553.5 in
#' the raw cells (8, 1), (8, 2) and (8, 9) of bio12, which fall into the
#' processed cells 25 and 29. Product `palaeo` (bio12 at 100 and bio15 at
#' 50 at ages 0, 500 and 1000) has bio12 at 600 at age 500 in the
#' processed cells 11 and 12 (inside the artefact region) and 15 (outside
#' it), and at age 1000 in cell 19 (an age the mask does not list). The
#' soil-unit raster has 18 by 36 cells of 10 degrees: unit 2 (dominant
#' Acrisols) almost everywhere, no data in column 1, unit 3 (open water) in
#' rows 3 and 4, columns 3 to 6, unit 4 (`LK`) in rows 5 and 6, columns 7
#' to 10, unit 5 (dominant Luvisols) in rows 9 and 10, columns 11 to 14,
#' and one unit-3 cell at (13, 15). The template grid has 16 by 36 cells
#' from 90 S to 70 N.
build_processing_pipeline_fixture <- function(
  dir_root,
  flag_upstream = TRUE
) {
  dir_root_normalised <-
    base::normalizePath(dir_root, winslash = "/")

  path_data_storage <-
    base::file.path(dir_root_normalised, "Data")

  dir_input <-
    base::file.path(path_data_storage, "Input")

  base::dir.create(path = dir_input, recursive = TRUE)

  mat_neo_bio12 <-
    base::replace(
      x = base::matrix(1000, nrow = 8, ncol = 16),
      list = base::cbind(c(8, 8, 8), c(1, 2, 9)),
      values = 6553.5
    )

  build_quality_raster_fixture(
    path_file = base::file.path(dir_input, "neo_bio1.tif"),
    mat_values = base::matrix(10, nrow = 8, ncol = 16)
  )

  build_quality_raster_fixture(
    path_file = base::file.path(dir_input, "neo_bio12.tif"),
    mat_values = mat_neo_bio12
  )

  data_palaeo <-
    tidyr::expand_grid(
      variable = c("bio12", "bio15"),
      age = c(0, 500, 1000)
    ) |>
    dplyr::mutate(
      time_id = base::as.integer((2000 - .data[["age"]]) / 100),
      file_name = stringr::str_c(
        "palaeo_", .data[["variable"]], "_", .data[["time_id"]], ".tif"
      )
    )

  purrr::pwalk(
    .l = base::list(
      dplyr::pull(data_palaeo, "variable"),
      dplyr::pull(data_palaeo, "age"),
      dplyr::pull(data_palaeo, "file_name")
    ),
    .f = ~ build_quality_raster_fixture(
      path_file = base::file.path(dir_input, ..3),
      mat_values = build_processing_palaeo_matrix(
        sel_variable = ..1,
        sel_age = ..2
      )
    )
  )

  # Soil units: written as an EHdr .bil file like HWSD2.bil
  mat_units <-
    base::matrix(2, nrow = 18, ncol = 36) |>
    base::replace(
      list = base::cbind(1:18, 1),
      values = NA
    ) |>
    base::replace(
      list = base::as.matrix(tidyr::expand_grid(r = 3:4, c = 3:6)),
      values = 3
    ) |>
    base::replace(
      list = base::as.matrix(tidyr::expand_grid(r = 5:6, c = 7:10)),
      values = 4
    ) |>
    base::replace(
      list = base::as.matrix(tidyr::expand_grid(r = 9:10, c = 11:14)),
      values = 5
    ) |>
    base::replace(
      list = base::cbind(13, 15),
      values = 3
    )

  # Cell values run by row from the north-west corner
  rast_units <-
    terra::rast(
      nrows = 18,
      ncols = 36,
      xmin = -180,
      xmax = 180,
      ymin = -90,
      ymax = 90,
      crs = "EPSG:4326",
      vals = base::as.vector(base::t(mat_units))
    )

  path_units <-
    base::file.path(dir_input, "HWSD2.bil")

  terra::writeRaster(
    x = rast_units,
    filename = path_units,
    filetype = "EHdr",
    datatype = "INT2U",
    NAflag = 65535
  )

  rast_template <-
    terra::rast(
      nrows = 16,
      ncols = 36,
      xmin = -180,
      xmax = 180,
      ymin = -90,
      ymax = 70,
      crs = "EPSG:4326",
      vals = 1
    )

  path_template <-
    base::file.path(dir_input, "tmpl_bio1.tif")

  terra::writeRaster(x = rast_template, filename = path_template)

  path_smu <-
    base::file.path(dir_input, "hwsd2_smu.csv")

  readr::write_csv(
    x = tibble::tibble(
      HWSD2_SMU_ID = c(2L, 2L, 3L, 4L, 5L, 5L),
      SEQUENCE = c(1L, 2L, 1L, 1L, 1L, 2L),
      SHARE = c(60, 40, 100, 100, 70, 30),
      WRB2 = c("AC", "LV", "WR", "LK", "LV", "AC"),
      WRB2_CODE = c(1, 18, 33, 18, 18, 1),
      WRB4 = c("ACha", "LVha", "WR", "LKcc", "LVha", "ACha")
    ),
    file = path_smu
  )

  # Upstream values the download stage would provide
  list_upstream <-
    base::list(
      neo_raw_files = tibble::tibble(
        product = "prod_neo",
        variable = c("bio1", "bio12"),
        age = 0,
        time_id = NA_integer_,
        file_name = c("neo_bio1.tif", "neo_bio12.tif"),
        path_file = base::file.path(
          dir_input,
          c("neo_bio1.tif", "neo_bio12.tif")
        ),
        sha256 = c("neo1", "neo12")
      ),
      palaeo_raw_files = data_palaeo |>
        dplyr::mutate(
          product = "prod_palaeo",
          path_file = base::file.path(dir_input, .data[["file_name"]]),
          sha256 = .data[["file_name"]]
        ) |>
        dplyr::select(
          "product", "variable", "age", "time_id", "file_name", "path_file",
          "sha256"
        ),
      tmpl_raw_files = tibble::tibble(
        product = "prod_tmpl",
        variable = "bio1",
        age = 0,
        time_id = NA_integer_,
        file_name = "tmpl_bio1.tif",
        path_file = path_template,
        sha256 = "tmpl"
      ),
      hwsd_extracted_files = c(
        path_units,
        base::file.path(dir_input, "HWSD2.hdr")
      ),
      hwsd_class_tables = path_smu
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

  list_data_processing <-
    base::list(
      aggregation_factor = 2,
      climate_products = base::list(
        neo = base::list(
          source = "neo",
          store_acquisition = "upstream",
          dir = "Processed/Neo",
          aggregation_fun = "median",
          fill_mask = base::list(
            variables = base::list(bio12 = 6553.5),
            lat_max = -70
          )
        ),
        palaeo = base::list(
          source = "palaeo",
          store_acquisition = "upstream",
          dir = "Processed/Palaeo",
          aggregation_fun = "median",
          artefact_mask = base::list(
            detector = "bio12",
            variables = c("bio12", "bio15"),
            ages = 500,
            region = base::list(
              lon_min = -90, lon_max = 0, lat_min = -90, lat_max = 45
            ),
            ratio_min = 2,
            increase_min = 100
          )
        )
      ),
      hwsd = base::list(
        store_acquisition = "upstream",
        store_template = "upstream",
        template_source = "tmpl",
        template_variable = "bio1",
        dir = "Processed/Hwsd",
        soil_group_codes = base::list(AC = 1L, LV = 19L),
        non_soil_codes = base::list(GG = 33L, WR = 34L, IS = 35L, ND = 36L),
        code_unlisted = 37L
      )
    )

  list_fixture <-
    base::list(list_data_processing = list_data_processing)

  path_config <-
    base::file.path(dir_root_normalised, "config.rds")

  base::saveRDS(object = list_fixture, file = path_config)

  dir_tables <-
    base::file.path(dir_root_normalised, "Tables")

  vec_path_functions <-
    base::normalizePath(
      here::here(
        "R", "Functions",
        c("Data_acquisition", "Data_processing", "Quality_checks", "Workflow")
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
      "list_processing <- list_fixture[['list_data_processing']]",
      "c(",
      "  purrr::map(",
      "    .x = c('neo', 'palaeo'),",
      "    .f = ~ build_climate_processing_targets(",
      "      sel_product = .x,",
      "      list_data_processing = list_processing,",
      stringr::str_glue("      path_data_storage = '{path_data_storage}',"),
      stringr::str_glue("      dir_tables = '{dir_tables}'"),
      "    )",
      "  ) |>",
      "    purrr::list_flatten(),",
      "  build_hwsd_processing_targets(",
      "    list_data_processing = list_processing,",
      stringr::str_glue("    path_data_storage = '{path_data_storage}',"),
      stringr::str_glue("    dir_tables = '{dir_tables}'"),
      "  )",
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
      path_store = base::file.path(
        path_data_storage, "Targets_data", "processing"
      ),
      path_script_upstream = path_script_upstream,
      path_store_upstream = path_store_upstream,
      list_fixture = list_fixture
    )

  return(res)
}

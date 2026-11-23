#' @title Build the processing targets of one climate product
#' @description
#' Declare the `{targets}` that import the validated raw files of a climate
#' source from the download stage, aggregate every file, compute the
#' fill-value and artefact cells, write the final processed rasters with
#' those cells missing, and save the counts of masked cells.
#' @param sel_product
#' Character scalar. Name of the product in
#' `list_data_processing[["climate_products"]]`; also the prefix of every
#' target name.
#' @param list_data_processing
#' The `data_processing` section of the configuration: `aggregation_factor`
#' and `climate_products`, where each product has `source` (the source in
#' `data_sources`), `store_acquisition` (store name of its download
#' pipeline), `dir` (folder of the processed rasters, relative to the data
#' root), `aggregation_fun`, and optionally `fill_mask` (with `variables`,
#' a named list of variable to fill value, and `lat_max`) and
#' `artefact_mask` (as in `build_artefact_mask_cells()`, plus `variables`,
#' the variables to mask).
#' @param path_data_storage
#' Character scalar. The data root.
#' @param dir_tables
#' Character scalar. Folder of the tables.
#' @param runner_acquisition
#' Character scalar. Runner of the download stage, named when its store is
#' not ready. Default `"R/Analyses/01_Data_acquisition/00_run.R"`.
#' @return
#' A list of target objects: `fingerprint_<source>_raw_files`,
#' `<product>_raw_files`, `<product>_processing_files`,
#' `<product>_aggregated`, `<product>_mask_cells`, `<product>_final`,
#' `<product>_masked_cell_counts` and the public
#' `<product>_processed_files`; with a fill mask also
#' `<product>_fill_files` and `<product>_fill_cells`; with an artefact mask
#' also `<product>_artefact_cells`.
#' @details
#' The aggregation and the final rasters are dynamic branches over the
#' file plan, so a replaced raw file reruns only its own branch. The final
#' branches depend on the whole mask table, so a changed mask rewrites the
#' (small) final rasters. The configuration is validated while declaring.
#' @seealso [build_processing_files()], [save_aggregated_raster()],
#'   [compute_fill_value_cells()], [build_artefact_mask_cells()],
#'   [prepare_processing_mask_cells()], [save_masked_raster()],
#'   [save_masked_cell_counts()]
#' @examples
#' \dontrun{
#' build_climate_processing_targets(
#'   sel_product = "chelsa_v21",
#'   list_data_processing = load_config_value(value = "data_processing"),
#'   path_data_storage = path_data_storage,
#'   dir_tables = here::here("Outputs", "Tables")
#' )
#' }
build_climate_processing_targets <- function(
  sel_product,
  list_data_processing,
  path_data_storage,
  dir_tables,
  runner_acquisition = "R/Analyses/01_Data_acquisition/00_run.R"
) {
  assertthat::assert_that(
    base::is.list(list_data_processing),
    msg = "`list_data_processing` must be a list."
  )

  aggregation_factor <-
    list_data_processing[["aggregation_factor"]]

  assertthat::assert_that(
    base::is.numeric(aggregation_factor),
    base::length(aggregation_factor) == 1L,
    base::is.finite(aggregation_factor),
    aggregation_factor >= 2,
    aggregation_factor == base::round(aggregation_factor),
    msg = "`aggregation_factor` must be a whole number of at least 2."
  )

  list_products <-
    list_data_processing[["climate_products"]]

  assertthat::assert_that(
    base::is.list(list_products),
    rlang::is_string(sel_product),
    sel_product %in% base::names(list_products),
    stringr::str_detect(string = sel_product, pattern = "^[a-z0-9_]+$"),
    msg = stringr::str_c(
      "`sel_product` must be the name of one product in ",
      "`climate_products`, of lower-case letters, digits and underscores."
    )
  )

  list_product <-
    list_products[[sel_product]]

  purrr::walk(
    .x = c("source", "store_acquisition", "dir"),
    .f = ~ assertthat::assert_that(
      rlang::is_string(list_product[[.x]]) &&
        base::nzchar(list_product[[.x]]),
      msg = stringr::str_c(
        "Product `", sel_product, "` needs `", .x, "` as one string."
      )
    )
  )

  assertthat::assert_that(
    rlang::is_string(list_product[["aggregation_fun"]]),
    list_product[["aggregation_fun"]] %in% c("median", "modal"),
    msg = stringr::str_c(
      "Product `", sel_product, "` needs `aggregation_fun` \"median\" or ",
      "\"modal\"."
    )
  )

  list_fill_mask <-
    list_product[["fill_mask"]]

  flag_fill <-
    !base::is.null(list_fill_mask)

  vec_fill_values <-
    base::unlist(list_fill_mask[["variables"]])

  assertthat::assert_that(
    !flag_fill ||
      (rlang::is_named(vec_fill_values) &&
        base::is.numeric(vec_fill_values) &&
        base::is.numeric(list_fill_mask[["lat_max"]]) &&
        base::length(list_fill_mask[["lat_max"]]) == 1L),
    msg = stringr::str_c(
      "The `fill_mask` of product `", sel_product, "` needs `variables` ",
      "(variable to fill value) and `lat_max`."
    )
  )

  list_artefact_mask <-
    list_product[["artefact_mask"]]

  flag_artefact <-
    !base::is.null(list_artefact_mask)

  if (
    isTRUE(flag_artefact)
  ) {
    assertthat::assert_that(
      rlang::is_string(list_artefact_mask[["detector"]]),
      base::is.character(list_artefact_mask[["variables"]]),
      base::is.numeric(list_artefact_mask[["ages"]]),
      base::is.list(list_artefact_mask[["region"]]),
      msg = stringr::str_c(
        "The `artefact_mask` of product `", sel_product, "` needs ",
        "`detector`, `variables`, `ages` and `region`."
      )
    )

    purrr::walk(
      .x = c("ratio_min", "increase_min"),
      .f = ~ assertthat::assert_that(
        base::is.numeric(list_artefact_mask[[.x]]),
        base::length(list_artefact_mask[[.x]]) == 1L,
        base::is.finite(list_artefact_mask[[.x]]),
        list_artefact_mask[[.x]] > 0,
        msg = stringr::str_c(
          "`", .x, "` of the `artefact_mask` of product `", sel_product,
          "` must be a positive number."
        )
      )
    )
  }

  assertthat::assert_that(
    rlang::is_string(path_data_storage),
    rlang::is_string(dir_tables),
    rlang::is_string(runner_acquisition),
    msg = stringr::str_c(
      "`path_data_storage`, `dir_tables` and `runner_acquisition` must be ",
      "single strings."
    )
  )

  store_acquisition <-
    resolve_pipeline_store_path(
      path_data_storage = path_data_storage,
      store_relative_path = list_product[["store_acquisition"]]
    )

  name_source_raw <-
    stringr::str_c(list_product[["source"]], "_raw_files")

  name_fingerprint <-
    stringr::str_c("fingerprint_", name_source_raw)

  dir_processed <-
    base::file.path(path_data_storage, list_product[["dir"]])

  # Target names of the product; symbols where commands refer to them
  vec_name <-
    c(
      raw_files = "raw_files",
      processing_files = "processing_files",
      aggregated = "aggregated",
      fill_files = "fill_files",
      fill_cells = "fill_cells",
      artefact_cells = "artefact_cells",
      mask_cells = "mask_cells",
      final = "final",
      masked_cell_counts = "masked_cell_counts",
      processed_files = "processed_files"
    ) |>
    purrr::map_chr(.f = ~ stringr::str_c(sel_product, "_", .x))

  list_sym <-
    purrr::map(.x = vec_name, .f = ~ rlang::sym(.x))

  # The processing files with the aggregated rasters as built, so targets
  #   that read them depend on them
  expr_files_aggregated <-
    rlang::expr(
      dplyr::mutate(
        !!list_sym[["processing_files"]],
        path_aggregated = !!list_sym[["aggregated"]]
      )
    )

  list_targets_import <-
    list(
      # Reruns the import whenever the download store changed
      targets::tar_target_raw(
        name = name_fingerprint,
        command = rlang::expr(
          compute_target_store_fingerprint(
            store = !!store_acquisition,
            target_names = !!name_source_raw,
            runner = !!runner_acquisition
          )
        ),
        cue = targets::tar_cue(mode = "always")
      ),
      # The validated raw files of the download stage
      targets::tar_target_raw(
        name = vec_name[["raw_files"]],
        command = rlang::expr(
          load_target_store_value(
            store = !!store_acquisition,
            target_name = !!name_source_raw,
            runner = !!runner_acquisition
          )
        ),
        deps = c(name_fingerprint, "load_target_store_value")
      )
    )

  list_targets_aggregation <-
    list(
      # Aggregated, final and raw path of every file
      targets::tar_target_raw(
        name = vec_name[["processing_files"]],
        command = rlang::expr(
          build_processing_files(
            data_raw_files = !!list_sym[["raw_files"]],
            dir_processed = !!dir_processed
          )
        )
      ),
      # One aggregation per raw file, as in v1
      targets::tar_target_raw(
        name = vec_name[["aggregated"]],
        command = rlang::expr(
          save_aggregated_raster(
            path_input = purrr::chuck(
              !!list_sym[["processing_files"]],
              "path_raw"
            ),
            path_output = purrr::chuck(
              !!list_sym[["processing_files"]],
              "path_aggregated"
            ),
            aggregation_factor = !!aggregation_factor,
            aggregation_fun = !!list_product[["aggregation_fun"]]
          )
        ),
        pattern = rlang::expr(map(!!list_sym[["processing_files"]])),
        format = "file"
      )
    )

  list_targets_fill <-
    if (
      isTRUE(flag_fill)
    ) {
      list(
        # The raw files that can hold a fill value
        targets::tar_target_raw(
          name = vec_name[["fill_files"]],
          command = rlang::expr(
            dplyr::filter(
              !!list_sym[["processing_files"]],
              .data[["variable"]] %in% !!base::names(vec_fill_values)
            )
          )
        ),
        # The aggregated cells whose raw block holds a fill value
        targets::tar_target_raw(
          name = vec_name[["fill_cells"]],
          command = rlang::expr(
            dplyr::mutate(
              compute_fill_value_cells(
                path_file = purrr::chuck(
                  !!list_sym[["fill_files"]],
                  "path_raw"
                ),
                fill_value = purrr::chuck(
                  !!base::as.list(vec_fill_values),
                  purrr::chuck(!!list_sym[["fill_files"]], "variable")
                ),
                lat_max = !!list_fill_mask[["lat_max"]],
                aggregation_factor = !!aggregation_factor
              ),
              file_name = purrr::chuck(
                !!list_sym[["fill_files"]],
                "file_name"
              )
            )
          ),
          pattern = rlang::expr(map(!!list_sym[["fill_files"]]))
        )
      )
    } else {
      list()
    }

  list_targets_artefact <-
    if (
      isTRUE(flag_artefact)
    ) {
      list(
        # The aggregated cells and ages of the artefact
        targets::tar_target_raw(
          name = vec_name[["artefact_cells"]],
          command = rlang::expr(
            build_artefact_mask_cells(
              data_processing_files = !!expr_files_aggregated,
              list_artefact_mask = !!list_artefact_mask
            )
          )
        )
      )
    } else {
      list()
    }

  expr_fill_cells <-
    if (
      isTRUE(flag_fill)
    ) {
      list_sym[["fill_cells"]]
    } else {
      NULL
    }

  expr_artefact_cells <-
    if (
      isTRUE(flag_artefact)
    ) {
      list_sym[["artefact_cells"]]
    } else {
      NULL
    }

  list_targets_final <-
    list(
      # Every cell to set to missing, per file
      targets::tar_target_raw(
        name = vec_name[["mask_cells"]],
        command = rlang::expr(
          prepare_processing_mask_cells(
            data_processing_files = !!list_sym[["processing_files"]],
            data_fill_cells = !!expr_fill_cells,
            data_artefact_cells = !!expr_artefact_cells,
            vec_artefact_variables = !!(
              list_artefact_mask[["variables"]] %||% base::character(0)
            )
          )
        )
      ),
      # One final raster per file, with its masked cells missing
      targets::tar_target_raw(
        name = vec_name[["final"]],
        command = rlang::expr(
          save_masked_raster(
            path_input = !!list_sym[["aggregated"]],
            path_output = purrr::chuck(
              !!list_sym[["processing_files"]],
              "path_final"
            ),
            vec_cells = dplyr::pull(
              dplyr::filter(
                !!list_sym[["mask_cells"]],
                .data[["file_name"]] == purrr::chuck(
                  !!list_sym[["processing_files"]],
                  "file_name"
                )
              ),
              "cell"
            )
          )
        ),
        pattern = rlang::expr(
          map(!!list_sym[["processing_files"]], !!list_sym[["aggregated"]])
        ),
        format = "file"
      ),
      # The counts of masked cells for the release notes
      targets::tar_target_raw(
        name = vec_name[["masked_cell_counts"]],
        command = rlang::expr(
          save_masked_cell_counts(
            data_mask_cells = !!list_sym[["mask_cells"]],
            data_processing_files = !!expr_files_aggregated,
            path_output = !!base::file.path(
              dir_tables,
              stringr::str_c("processing_masked_cells_", sel_product, ".csv")
            )
          )
        ),
        format = "file"
      ),
      # Public: the final processed rasters, one row per file; the branch
      #   names of the final rasters are not passed on
      targets::tar_target_raw(
        name = vec_name[["processed_files"]],
        command = rlang::expr(
          dplyr::mutate(
            !!list_sym[["processing_files"]],
            path_file = base::unname(!!list_sym[["final"]])
          )
        )
      )
    )

  res <-
    c(
      list_targets_import,
      list_targets_aggregation,
      list_targets_fill,
      list_targets_artefact,
      list_targets_final
    )

  return(res)
}

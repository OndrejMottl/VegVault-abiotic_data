#' @title Build the extraction targets of the HWSD soil map
#' @description
#' Declare the `{targets}` that import the soil-group raster from the
#' processing stage and save its cell table.
#' @param list_data_extraction
#' The `data_extraction` section of the configuration, with `hwsd`:
#' `store_processing` (store name of the HWSD processing pipeline),
#' `product` and `variable` (the names of the soil map in the file plan),
#' `dir` (folder of the cell table, relative to the data root), `columns`
#' (as in `build_climate_extraction_targets()`) and, when `columns` holds
#' `var_name`, `var_names` (with a label for `variable`).
#' @param path_data_storage
#' Character scalar. The data root.
#' @param runner_processing
#' Character scalar. Runner of the processing stage, named when its store
#' is not ready. Default `"R/Analyses/02_Data_processing/00_run.R"`.
#' @return
#' A list of target objects: `fingerprint_hwsd_soil_raster`,
#' `hwsd_soil_raster`, `hwsd_extraction_files`, `hwsd_processed_raster`,
#' `hwsd_cell_table` and the public `hwsd_extracted_files` (the extraction
#' file with `path_file`, the cell table).
#' @details
#' The soil map is one raster at age 0 without a time slice. The raster is
#' a file target, so a changed soil map is extracted again. The
#' configuration is validated while declaring.
#' @seealso [build_extraction_files()], [save_raster_cell_table()]
#' @examples
#' \dontrun{
#' build_hwsd_extraction_targets(
#'   list_data_extraction = load_config_value(value = "data_extraction"),
#'   path_data_storage = path_data_storage
#' )
#' }
build_hwsd_extraction_targets <- function(
  list_data_extraction,
  path_data_storage,
  runner_processing = "R/Analyses/02_Data_processing/00_run.R"
) {
  assertthat::assert_that(
    base::is.list(list_data_extraction),
    base::is.list(list_data_extraction[["hwsd"]]),
    msg = "`list_data_extraction` must be a list with `hwsd`."
  )

  list_hwsd <-
    list_data_extraction[["hwsd"]]

  purrr::walk(
    .x = c("store_processing", "product", "variable", "dir"),
    .f = ~ assertthat::assert_that(
      rlang::is_string(list_hwsd[[.x]]) &&
        base::nzchar(list_hwsd[[.x]]),
      msg = stringr::str_c("`hwsd` needs `", .x, "` as one string.")
    )
  )

  vec_columns <-
    list_hwsd[["columns"]]

  validate_extraction_columns(
    vec_columns = vec_columns,
    list_var_names = list_hwsd[["var_names"]],
    vec_variables = list_hwsd[["variable"]],
    label = "`hwsd`"
  )

  vec_columns_constant <-
    base::setdiff(vec_columns, c("long", "lat", "value"))

  # The label is used only when the cell table has a var_name column
  list_var_names <-
    if (
      "var_name" %in% vec_columns
    ) {
      list_hwsd[["var_names"]]
    } else {
      NULL
    }

  assertthat::assert_that(
    rlang::is_string(path_data_storage),
    rlang::is_string(runner_processing),
    msg = "`path_data_storage` and `runner_processing` must be single strings."
  )

  store_processing <-
    resolve_pipeline_store_path(
      path_data_storage = path_data_storage,
      store_relative_path = list_hwsd[["store_processing"]]
    )

  dir_cell_tables <-
    base::file.path(path_data_storage, list_hwsd[["dir"]])

  res <-
    list(
      # Reruns the import whenever the processing store changed
      targets::tar_target_raw(
        name = "fingerprint_hwsd_soil_raster",
        command = rlang::expr(
          compute_target_store_fingerprint(
            store = !!store_processing,
            target_names = "hwsd_soil_raster",
            runner = !!runner_processing
          )
        ),
        cue = targets::tar_cue(mode = "always")
      ),
      # The soil-group raster of the processing stage
      targets::tar_target_raw(
        name = "hwsd_soil_raster",
        command = rlang::expr(
          load_target_store_value(
            store = !!store_processing,
            target_name = "hwsd_soil_raster",
            runner = !!runner_processing
          )
        ),
        deps = c("fingerprint_hwsd_soil_raster", "load_target_store_value")
      ),
      # Raster, cell table and fixed columns of the soil map, at age 0
      #   without a time slice
      targets::tar_target_raw(
        name = "hwsd_extraction_files",
        command = rlang::expr(
          build_extraction_files(
            data_processed_files = tibble::tibble(
              product = !!list_hwsd[["product"]],
              variable = !!list_hwsd[["variable"]],
              age = 0,
              time_id = NA_integer_,
              file_name = base::basename(hwsd_soil_raster),
              path_file = hwsd_soil_raster
            ),
            dir_cell_tables = !!dir_cell_tables,
            list_var_names = !!list_var_names
          )
        )
      ),
      # The soil-group raster as a file, so a changed raster is extracted
      #   again even when its path stays
      targets::tar_target_raw(
        name = "hwsd_processed_raster",
        command = rlang::expr(
          purrr::chuck(hwsd_extraction_files, "path_raster")
        ),
        format = "file"
      ),
      # The cell table of the soil map, with the configured columns
      targets::tar_target_raw(
        name = "hwsd_cell_table",
        command = rlang::expr(
          save_raster_cell_table(
            path_input = hwsd_processed_raster,
            path_output = purrr::chuck(
              hwsd_extraction_files,
              "path_cell_table"
            ),
            data_constants = dplyr::select(
              hwsd_extraction_files,
              dplyr::all_of(!!vec_columns_constant)
            ),
            vec_columns = !!vec_columns
          )
        ),
        format = "file"
      ),
      # Public: the cell table of the soil map
      targets::tar_target_raw(
        name = "hwsd_extracted_files",
        command = rlang::expr(
          dplyr::mutate(
            hwsd_extraction_files,
            path_file = hwsd_cell_table
          )
        )
      )
    )

  return(res)
}

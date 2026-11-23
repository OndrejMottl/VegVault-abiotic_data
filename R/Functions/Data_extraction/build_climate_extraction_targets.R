#' @title Build the extraction targets of one climate product
#' @description
#' Declare the `{targets}` that import the processed rasters of a climate
#' product from the processing stage and save one cell table per raster.
#' @param sel_product
#' Character scalar. Name of the product in
#' `list_data_extraction[["climate_products"]]`; also the prefix of every
#' target name and the product name in the processing stage.
#' @param list_data_extraction
#' The `data_extraction` section of the configuration. Each product of
#' `climate_products` has `store_processing` (store name of its processing
#' pipeline), `dir` (folder of the cell tables, relative to the data root),
#' `columns` (the columns of the cell tables in their order: `long`, `lat`,
#' `value` and any of `time_id` and `var_name`) and, when `columns` holds
#' `var_name`, `var_names` (variable to label, as in
#' `build_extraction_files()`).
#' @param path_data_storage
#' Character scalar. The data root.
#' @param runner_processing
#' Character scalar. Runner of the processing stage, named when its store
#' is not ready. Default `"R/Analyses/02_Data_processing/00_run.R"`.
#' @return
#' A list of target objects: `fingerprint_<product>_processed_files`,
#' `<product>_processed_files`, `<product>_extraction_files`,
#' `<product>_processed_rasters`, `<product>_cell_tables` and the public
#' `<product>_extracted_files` (the extraction files with `path_file`, the
#' cell table).
#' @details
#' The processed rasters are file targets, so a raster that changes is
#' extracted again even when its path stays; the cell tables are dynamic
#' branches over the file plan, so only that raster's branch reruns. The
#' configuration is validated while declaring.
#' @seealso [build_extraction_files()], [save_raster_cell_table()]
#' @examples
#' \dontrun{
#' build_climate_extraction_targets(
#'   sel_product = "chelsa_v21",
#'   list_data_extraction = load_config_value(value = "data_extraction"),
#'   path_data_storage = path_data_storage
#' )
#' }
build_climate_extraction_targets <- function(
  sel_product,
  list_data_extraction,
  path_data_storage,
  runner_processing = "R/Analyses/02_Data_processing/00_run.R"
) {
  assertthat::assert_that(
    base::is.list(list_data_extraction),
    msg = "`list_data_extraction` must be a list."
  )

  list_products <-
    list_data_extraction[["climate_products"]]

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
    .x = c("store_processing", "dir"),
    .f = ~ assertthat::assert_that(
      rlang::is_string(list_product[[.x]]) &&
        base::nzchar(list_product[[.x]]),
      msg = stringr::str_c(
        "Product `", sel_product, "` needs `", .x, "` as one string."
      )
    )
  )

  vec_columns <-
    list_product[["columns"]]

  validate_extraction_columns(
    vec_columns = vec_columns,
    list_var_names = list_product[["var_names"]],
    label = stringr::str_c("Product `", sel_product, "`")
  )

  vec_columns_constant <-
    base::setdiff(vec_columns, c("long", "lat", "value"))

  # The labels are used only when the cell tables have a var_name column
  list_var_names <-
    if (
      "var_name" %in% vec_columns
    ) {
      list_product[["var_names"]]
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
      store_relative_path = list_product[["store_processing"]]
    )

  name_processed_upstream <-
    stringr::str_c(sel_product, "_processed_files")

  dir_cell_tables <-
    base::file.path(path_data_storage, list_product[["dir"]])

  # Target names of the product; symbols where commands refer to them
  vec_name <-
    c(
      processed_files = "processed_files",
      extraction_files = "extraction_files",
      processed_rasters = "processed_rasters",
      cell_tables = "cell_tables",
      extracted_files = "extracted_files"
    ) |>
    purrr::map_chr(.f = ~ stringr::str_c(sel_product, "_", .x))

  name_fingerprint <-
    stringr::str_c("fingerprint_", name_processed_upstream)

  list_sym <-
    purrr::map(.x = vec_name, .f = ~ rlang::sym(.x))

  res <-
    list(
      # Reruns the import whenever the processing store changed
      targets::tar_target_raw(
        name = name_fingerprint,
        command = rlang::expr(
          compute_target_store_fingerprint(
            store = !!store_processing,
            target_names = !!name_processed_upstream,
            runner = !!runner_processing
          )
        ),
        cue = targets::tar_cue(mode = "always")
      ),
      # The processed rasters of the processing stage
      targets::tar_target_raw(
        name = vec_name[["processed_files"]],
        command = rlang::expr(
          load_target_store_value(
            store = !!store_processing,
            target_name = !!name_processed_upstream,
            runner = !!runner_processing
          )
        ),
        deps = c(name_fingerprint, "load_target_store_value")
      ),
      # Raster, cell table and fixed columns of every file
      targets::tar_target_raw(
        name = vec_name[["extraction_files"]],
        command = rlang::expr(
          build_extraction_files(
            data_processed_files = !!list_sym[["processed_files"]],
            dir_cell_tables = !!dir_cell_tables,
            list_var_names = !!list_var_names
          )
        )
      ),
      # Each processed raster as a file, so a changed raster reruns its
      #   own cell table even when its path stays
      targets::tar_target_raw(
        name = vec_name[["processed_rasters"]],
        command = rlang::expr(
          purrr::chuck(!!list_sym[["extraction_files"]], "path_raster")
        ),
        pattern = rlang::expr(map(!!list_sym[["extraction_files"]])),
        format = "file"
      ),
      # One cell table per processed raster, with the configured columns
      targets::tar_target_raw(
        name = vec_name[["cell_tables"]],
        command = rlang::expr(
          save_raster_cell_table(
            path_input = !!list_sym[["processed_rasters"]],
            path_output = purrr::chuck(
              !!list_sym[["extraction_files"]],
              "path_cell_table"
            ),
            data_constants = dplyr::select(
              !!list_sym[["extraction_files"]],
              dplyr::all_of(!!vec_columns_constant)
            ),
            vec_columns = !!vec_columns
          )
        ),
        pattern = rlang::expr(
          map(
            !!list_sym[["extraction_files"]],
            !!list_sym[["processed_rasters"]]
          )
        ),
        format = "file"
      ),
      # Public: the cell tables, one row per processed raster
      targets::tar_target_raw(
        name = vec_name[["extracted_files"]],
        command = rlang::expr(
          dplyr::mutate(
            !!list_sym[["extraction_files"]],
            path_file = base::unname(!!list_sym[["cell_tables"]])
          )
        )
      )
    )

  return(res)
}

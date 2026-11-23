#' @title Save the cell table of a raster
#' @description
#' Build the cell table of a single-layer raster, add the columns that
#' hold one value for the whole file (such as the time slice or the
#' variable label), order the columns as given and save the table as a
#' `qs2` file.
#' @param path_input
#' Character scalar. Path of an existing single-layer raster.
#' @param path_output
#' Character scalar. Path of the `qs2` file to write; its folder is
#' created when missing. It must differ from `path_input`.
#' @param data_constants
#' A data frame with one row and no missing value. Each column is added to
#' every row of the table with its value and class. Its names must not be
#' `long`, `lat` or `value`; it may have no column.
#' @param vec_columns
#' Character vector. The columns of the saved table in their order: `long`,
#' `lat`, `value` and the columns of `data_constants`, each once.
#' @return
#' `path_output`.
#' @details
#' The rows are those of `prepare_raster_cell_table()`. Aborts when the
#' raster has no cell with a value, so an empty table is never saved. The
#' table is written to a temporary file next to `path_output` and then
#' moved into place, so an interrupted run never leaves a partial file
#' under the final name; an existing `path_output` is replaced, as the
#' stage writes only its own outputs.
#' @seealso [prepare_raster_cell_table()], [build_extraction_files()]
#' @examples
#' \dontrun{
#' save_raster_cell_table(
#'   path_input = "Data/Processed/CHELSA_TraCE21k_V1.0/x_20_V1.0.tif",
#'   path_output = "Data/Processed/CHELSA_TraCE21k_V1.0/Cell_tables/x.qs2",
#'   data_constants = tibble::tibble(time_id = 20L),
#'   vec_columns = c("long", "lat", "time_id", "value")
#' )
#' }
save_raster_cell_table <- function(
  path_input,
  path_output,
  data_constants,
  vec_columns
) {
  assertthat::assert_that(
    rlang::is_string(path_input),
    base::file.exists(path_input),
    msg = "`path_input` must be the path to an existing raster."
  )

  assertthat::assert_that(
    rlang::is_string(path_output),
    base::nzchar(path_output),
    base::normalizePath(path_output, winslash = "/", mustWork = FALSE) !=
      base::normalizePath(path_input, winslash = "/"),
    msg = "`path_output` must be one path that differs from `path_input`."
  )

  vec_columns_cells <-
    c("long", "lat", "value")

  assertthat::assert_that(
    base::is.data.frame(data_constants),
    base::nrow(data_constants) == 1L,
    !base::anyNA(data_constants),
    !base::any(base::colnames(data_constants) %in% vec_columns_cells),
    msg = stringr::str_c(
      "`data_constants` must be a data frame with one row, no missing ",
      "value and no column named long, lat or value."
    )
  )

  vec_columns_expected <-
    c(vec_columns_cells, base::colnames(data_constants))

  assertthat::assert_that(
    base::is.character(vec_columns),
    !base::anyNA(vec_columns),
    !base::anyDuplicated(vec_columns),
    base::length(vec_columns) == base::length(vec_columns_expected),
    base::all(vec_columns %in% vec_columns_expected),
    msg = stringr::str_c(
      "`vec_columns` must name long, lat, value and the columns of ",
      "`data_constants`, each once."
    )
  )

  data_cells <-
    prepare_raster_cell_table(path_file = path_input)

  if (
    base::nrow(data_cells) == 0L
  ) {
    cli::cli_abort(
      c(
        "The raster has no cell with a value.",
        "x" = "{.file {path_input}}"
      )
    )
  }

  data_cell_table <-
    data_cells |>
    dplyr::mutate(!!!base::as.list(data_constants)) |>
    dplyr::select(dplyr::all_of(vec_columns))

  base::dir.create(
    path = base::dirname(path_output),
    recursive = TRUE,
    showWarnings = FALSE
  )

  # Written under a temporary name first, so an interrupted run never
  #   leaves a partial table under the final name
  path_temporary <-
    stringr::str_c(path_output, ".part")

  qs2::qs_save(object = data_cell_table, file = path_temporary)

  fs::file_move(path = path_temporary, new_path = path_output)

  res <-
    path_output

  return(res)
}

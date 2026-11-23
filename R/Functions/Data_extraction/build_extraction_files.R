#' @title Build the file plan of an extraction stage
#' @description
#' For every processed raster, name the cell table the extraction stage
#' writes and, when labels are given, the variable label of its rows.
#' @param data_processed_files
#' A data frame with one row per processed raster and at least the columns
#' `product`, `variable`, `age`, `time_id` (integer, may be missing),
#' `file_name` and `path_file` (the processed raster).
#' @param dir_cell_tables
#' Character scalar. Folder of the cell tables.
#' @param list_var_names
#' `NULL` (default) or a named list (or named character vector) of single
#' strings: the variable label of the rows of each variable. Every
#' variable of `data_processed_files` needs a label.
#' @return
#' A tibble with one row per processed raster, in the order of
#' `data_processed_files`, and the columns `product`, `variable`, `age`,
#' `time_id`, `file_name`, `var_name` (only when `list_var_names` is
#' given), `path_raster` (the processed raster, without names) and
#' `path_cell_table` (`<dir_cell_tables>/<file name without extension>.qs2`).
#' @details
#' Aborts when a file name is repeated, so no two rasters can write the
#' same cell table, and when a variable has no label.
#' @seealso [save_raster_cell_table()]
#' @examples
#' build_extraction_files(
#'   data_processed_files = tibble::tibble(
#'     product = "CHELSA_V2.1",
#'     variable = "bio1",
#'     age = 0,
#'     time_id = NA_integer_,
#'     file_name = "CHELSA_bio1_1981-2010_V.2.1.tif",
#'     path_file = "Data/Processed/CHELSA_V2.1/CHELSA_bio1_1981-2010_V.2.1.tif"
#'   ),
#'   dir_cell_tables = "Data/Processed/CHELSA_V2.1/Cell_tables",
#'   list_var_names = base::list(bio1 = "bio_01")
#' )
build_extraction_files <- function(
  data_processed_files,
  dir_cell_tables,
  list_var_names = NULL
) {
  vec_columns_required <-
    c("product", "variable", "age", "time_id", "file_name", "path_file")

  assertthat::assert_that(
    base::is.data.frame(data_processed_files),
    base::all(
      vec_columns_required %in% base::colnames(data_processed_files)
    ),
    msg = stringr::str_c(
      "`data_processed_files` must be a data frame with the columns ",
      stringr::str_c(vec_columns_required, collapse = ", "),
      "."
    )
  )

  assertthat::assert_that(
    base::is.integer(dplyr::pull(data_processed_files, "time_id")),
    msg = "`time_id` of `data_processed_files` must be integer."
  )

  assertthat::assert_that(
    rlang::is_string(dir_cell_tables),
    !base::is.na(dir_cell_tables),
    base::nzchar(dir_cell_tables),
    msg = "`dir_cell_tables` must be one non-empty folder path."
  )

  vec_var_names <-
    base::unlist(list_var_names)

  assertthat::assert_that(
    base::is.null(list_var_names) ||
      (rlang::is_named(list_var_names) &&
        base::is.character(vec_var_names) &&
        base::length(vec_var_names) == base::length(list_var_names) &&
        !base::anyNA(vec_var_names)),
    msg = stringr::str_c(
      "`list_var_names` must be NULL or a named list of single strings ",
      "(variable to label)."
    )
  )

  vec_files_repeated <-
    data_processed_files |>
    dplyr::count(.data[["file_name"]]) |>
    dplyr::filter(.data[["n"]] > 1L) |>
    dplyr::pull("file_name")

  if (
    base::length(vec_files_repeated) > 0L
  ) {
    cli::cli_abort(
      c(
        "Two processed rasters would write the same cell table.",
        "x" = "{.val {vec_files_repeated}}"
      )
    )
  }

  data_files <-
    data_processed_files |>
    dplyr::mutate(
      path_raster = base::unname(
        base::as.character(.data[["path_file"]])
      ),
      path_cell_table = base::file.path(
        dir_cell_tables,
        stringr::str_c(
          tools::file_path_sans_ext(.data[["file_name"]]),
          ".qs2"
        )
      )
    )

  if (
    base::is.null(list_var_names)
  ) {
    res <-
      data_files |>
      dplyr::select(
        "product", "variable", "age", "time_id", "file_name",
        "path_raster", "path_cell_table"
      )

    return(res)
  }

  vec_variables_unlabelled <-
    data_files |>
    dplyr::pull("variable") |>
    base::unique() |>
    base::setdiff(base::names(vec_var_names))

  if (
    base::length(vec_variables_unlabelled) > 0L
  ) {
    cli::cli_abort(
      c(
        "A variable has no label in `list_var_names`.",
        "x" = "{.val {vec_variables_unlabelled}}"
      )
    )
  }

  res <-
    data_files |>
    dplyr::mutate(
      var_name = base::unname(vec_var_names[.data[["variable"]]])
    ) |>
    dplyr::select(
      "product", "variable", "age", "time_id", "file_name", "var_name",
      "path_raster", "path_cell_table"
    )

  return(res)
}

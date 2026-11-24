#' @title Prepare the cells to mask in each processed raster
#' @description
#' Combine the fill-value cells and the artefact cells into one table of
#' the aggregated cells to set to missing in each processed file.
#' @param data_processing_files
#' A data frame as returned by `build_processing_files()`.
#' @param data_fill_cells
#' A data frame with the columns `file_name` and `cell`, the fill-value
#' cells of each file (from `compute_fill_value_cells()`), or `NULL` when
#' the product has no fill mask.
#' @param data_artefact_cells
#' A data frame with the columns `age` and `cell`, as returned by
#' `build_artefact_mask_cells()`, or `NULL` when the product has no
#' artefact mask.
#' @param vec_artefact_variables
#' Character vector. Variables whose files take the artefact cells of
#' their age; ignored when `data_artefact_cells` is `NULL`.
#' @return
#' A tibble with the columns `file_name`, `variable`, `age`, `cell`
#' (integer) and `mask` (`"fill_value"` or `"artefact"`), one row per file,
#' cell and mask, sorted by `file_name`, `mask` and `cell`; zero rows when
#' nothing is masked.
#' @details
#' Aborts when a fill-value cell names a file that is not in
#' `data_processing_files`, or an artefact variable has no file.
#' @seealso [compute_fill_value_cells()], [build_artefact_mask_cells()],
#'   [save_masked_raster()]
#' @examples
#' \dontrun{
#' prepare_processing_mask_cells(
#'   data_processing_files = data_processing_files,
#'   data_fill_cells = NULL,
#'   data_artefact_cells = data_artefact_cells,
#'   vec_artefact_variables = c("bio12", "bio15", "bio18", "bio19")
#' )
#' }
prepare_processing_mask_cells <- function(
  data_processing_files,
  data_fill_cells,
  data_artefact_cells,
  vec_artefact_variables
) {
  vec_columns_files <-
    c("variable", "age", "file_name")

  assertthat::assert_that(
    base::is.data.frame(data_processing_files),
    base::all(vec_columns_files %in% base::colnames(data_processing_files)),
    msg = stringr::str_c(
      "`data_processing_files` must be a data frame with the columns ",
      stringr::str_c(vec_columns_files, collapse = ", "),
      "."
    )
  )

  assertthat::assert_that(
    base::is.null(data_fill_cells) ||
      (base::is.data.frame(data_fill_cells) &&
        base::all(
          c("file_name", "cell") %in% base::colnames(data_fill_cells)
        )),
    msg = stringr::str_c(
      "`data_fill_cells` must be NULL or a data frame with the columns ",
      "`file_name` and `cell`."
    )
  )

  assertthat::assert_that(
    base::is.null(data_artefact_cells) ||
      (base::is.data.frame(data_artefact_cells) &&
        base::all(
          c("age", "cell") %in% base::colnames(data_artefact_cells)
        )),
    msg = stringr::str_c(
      "`data_artefact_cells` must be NULL or a data frame with the columns ",
      "`age` and `cell`."
    )
  )

  assertthat::assert_that(
    base::is.character(vec_artefact_variables),
    !base::anyNA(vec_artefact_variables),
    msg = "`vec_artefact_variables` must be a character vector."
  )

  data_files <-
    dplyr::select(data_processing_files, dplyr::all_of(vec_columns_files))

  data_none <-
    tibble::tibble(
      file_name = base::character(0),
      variable = base::character(0),
      age = base::numeric(0),
      cell = base::integer(0),
      mask = base::character(0)
    )

  vec_files_unknown <-
    base::setdiff(
      dplyr::pull(data_fill_cells %||% data_none, "file_name"),
      dplyr::pull(data_files, "file_name")
    )

  if (
    base::length(vec_files_unknown) > 0L
  ) {
    cli::cli_abort(
      c(
        "A fill-value cell names a file the stage does not process.",
        "x" = "{.val {vec_files_unknown}}"
      )
    )
  }

  vec_variables_without_file <-
    if (
      base::is.null(data_artefact_cells)
    ) {
      base::character(0)
    } else {
      base::setdiff(
        vec_artefact_variables,
        dplyr::pull(data_files, "variable")
      )
    }

  if (
    base::length(vec_variables_without_file) > 0L
  ) {
    cli::cli_abort(
      c(
        "An artefact variable has no file.",
        "x" = "{.val {vec_variables_without_file}}"
      )
    )
  }

  data_fill <-
    if (
      base::is.null(data_fill_cells)
    ) {
      data_none
    } else {
      data_fill_cells |>
        dplyr::select("file_name", "cell") |>
        dplyr::left_join(data_files, by = "file_name") |>
        dplyr::mutate(mask = "fill_value")
    }

  # Every listed variable takes the artefact cells of its age
  data_artefact <-
    if (
      base::is.null(data_artefact_cells)
    ) {
      data_none
    } else {
      data_files |>
        dplyr::filter(.data[["variable"]] %in% vec_artefact_variables) |>
        dplyr::inner_join(
          dplyr::select(data_artefact_cells, "age", "cell"),
          by = "age",
          relationship = "many-to-many"
        ) |>
        dplyr::mutate(mask = "artefact")
    }

  res <-
    dplyr::bind_rows(data_none, data_fill, data_artefact) |>
    dplyr::mutate(cell = base::as.integer(.data[["cell"]])) |>
    dplyr::select("file_name", "variable", "age", "cell", "mask") |>
    dplyr::arrange(.data[["file_name"]], .data[["mask"]], .data[["cell"]])

  return(res)
}

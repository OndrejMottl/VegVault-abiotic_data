#' @title Save the counts of masked cells of a processing stage
#' @description
#' Count, per file and mask, the cells set to missing and how many of them
#' held a value before, and save the table for the release notes.
#' @param data_mask_cells
#' A data frame as returned by `prepare_processing_mask_cells()`.
#' @param data_processing_files
#' A data frame as returned by `build_processing_files()`; the values
#' before masking are read from `path_aggregated`.
#' @param path_output
#' Character scalar. Path of the CSV file to write; its folder is created
#' when missing.
#' @return
#' `path_output`. The CSV file has the columns `product`, `variable`,
#' `age`, `time_id`, `file_name`, `mask`, `n_cells` and
#' `n_cells_with_value`, one row per file and mask with at least one cell,
#' sorted by `variable`, `age` and `mask`; only the header when nothing is
#' masked.
#' @seealso [prepare_processing_mask_cells()], [save_masked_raster()]
#' @examples
#' \dontrun{
#' save_masked_cell_counts(
#'   data_mask_cells = data_mask_cells,
#'   data_processing_files = data_processing_files,
#'   path_output = "Outputs/Tables/processing_masked_cells_chelsa.csv"
#' )
#' }
save_masked_cell_counts <- function(
  data_mask_cells,
  data_processing_files,
  path_output
) {
  vec_columns_mask <-
    c("file_name", "cell", "mask")

  assertthat::assert_that(
    base::is.data.frame(data_mask_cells),
    base::all(vec_columns_mask %in% base::colnames(data_mask_cells)),
    msg = stringr::str_c(
      "`data_mask_cells` must be a data frame with the columns ",
      stringr::str_c(vec_columns_mask, collapse = ", "),
      "."
    )
  )

  vec_columns_files <-
    c("product", "variable", "age", "time_id", "file_name", "path_aggregated")

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
    rlang::is_string(path_output),
    base::nzchar(path_output),
    msg = "`path_output` must be one file path."
  )

  data_per_file <-
    data_mask_cells |>
    dplyr::select(dplyr::all_of(vec_columns_mask)) |>
    dplyr::left_join(
      dplyr::select(data_processing_files, dplyr::all_of(vec_columns_files)),
      by = "file_name"
    )

  vec_paths <-
    base::unique(dplyr::pull(data_per_file, "path_aggregated"))

  # Whether each masked cell held a value before the mask
  data_had_value <-
    purrr::map(
      .x = vec_paths,
      .f = ~ tibble::tibble(
        path_aggregated = .x,
        values = base::list(terra::values(terra::rast(.x), mat = FALSE))
      )
    ) |>
    purrr::list_rbind()

  data_counts_raw <-
    if (
      base::nrow(data_per_file) == 0L
    ) {
      data_per_file |>
        dplyr::mutate(flag_had_value = base::logical(0))
    } else {
      data_per_file |>
        dplyr::left_join(data_had_value, by = "path_aggregated") |>
        dplyr::mutate(
          flag_had_value = !base::is.na(
            purrr::map2_dbl(
              .x = .data[["values"]],
              .y = .data[["cell"]],
              .f = ~ .x[.y]
            )
          )
        ) |>
        dplyr::select(-"values")
    }

  res_counts <-
    data_counts_raw |>
    dplyr::summarise(
      n_cells = dplyr::n(),
      n_cells_with_value = base::sum(.data[["flag_had_value"]]),
      .by = dplyr::all_of(
        c("product", "variable", "age", "time_id", "file_name", "mask")
      )
    ) |>
    dplyr::arrange(.data[["variable"]], .data[["age"]], .data[["mask"]])

  base::dir.create(
    path = base::dirname(path_output),
    recursive = TRUE,
    showWarnings = FALSE
  )

  readr::write_csv(x = res_counts, file = path_output, na = "")

  res <-
    path_output

  return(res)
}

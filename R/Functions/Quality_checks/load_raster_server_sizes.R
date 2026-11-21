#' @title Load the recorded server sizes of raster files
#' @description
#' Read the tracked record of the file sizes that the data provider's
#' server announced, which the raster quality check compares with the
#' local files.
#' @param path_file
#' Character scalar. Path to a CSV file with at least the columns
#' `product`, `file_name` and `size_bytes`.
#' @return
#' A tibble with the columns `product`, `file_name` (character) and
#' `size_bytes` (numeric), one row per product and file.
#' @details
#' Aborts when the file does not exist, a column is missing, a product and
#' file name appear twice, or a size is missing or not positive.
#' @seealso [build_raster_quality_expected_files()]
#' @examples
#' \dontrun{
#' load_raster_server_sizes(
#'   path_file = "Data/Input/Server_file_sizes/server_file_sizes.csv"
#' )
#' }
load_raster_server_sizes <- function(path_file) {
  assertthat::assert_that(
    rlang::is_string(path_file),
    base::file.exists(path_file),
    msg = "`path_file` must be the path to an existing CSV file."
  )

  data_raw <-
    readr::read_csv(
      file = path_file,
      col_types = readr::cols(.default = readr::col_character()),
      progress = FALSE
    )

  vec_columns <-
    c("product", "file_name", "size_bytes")

  if (
    !base::all(vec_columns %in% base::colnames(data_raw))
  ) {
    cli::cli_abort(
      c(
        "The server-size record lacks a required column.",
        "i" = "It needs `product`, `file_name` and `size_bytes`.",
        "x" = "File: {.path {path_file}}"
      )
    )
  }

  res <-
    data_raw |>
    dplyr::select(dplyr::all_of(vec_columns)) |>
    dplyr::mutate(
      size_bytes = base::as.numeric(.data[["size_bytes"]])
    )

  if (
    base::anyDuplicated(dplyr::select(res, "product", "file_name")) > 0L
  ) {
    cli::cli_abort(
      c(
        "A product and file name appear twice in the server-size record.",
        "x" = "File: {.path {path_file}}"
      )
    )
  }

  vec_sizes <-
    dplyr::pull(res, "size_bytes")

  if (
    base::any(base::is.na(vec_sizes) | vec_sizes <= 0)
  ) {
    cli::cli_abort(
      c(
        "Every size in the server-size record must be a positive number.",
        "x" = "File: {.path {path_file}}"
      )
    )
  }

  return(res)
}

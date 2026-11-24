#' @title Load the download records of the raw data sources
#' @description
#' Read every download record in a folder and bind them into one table of
#' which file was obtained from where, when, with which size and SHA-256
#' checksum.
#' @param dir_records
#' Character scalar. Folder holding the records, files named
#' `download_record_*.csv`.
#' @return
#' A tibble with the columns `product`, `file_name`, `url`, `size_bytes`
#' (numeric), `sha256`, `downloaded_utc`, `recorded_utc` and `method`
#' (character), one row per record row, sorted by `product`, `file_name`
#' and `recorded_utc`; zero rows when the folder holds no record.
#' @details
#' `method` is `"download"` for a file this stage downloaded and
#' `"checksum"` for a file that was already present and only had its
#' checksum recorded; `downloaded_utc` is `NA` for the latter. Aborts when
#' the folder does not exist or a record lacks a column.
#' @seealso [run_source_downloads()], [run_source_checksums()],
#'   [validate_source_raw_files()]
#' @examples
#' \dontrun{
#' load_download_records(dir_records = "Data/Input/Download_records")
#' }
load_download_records <- function(dir_records) {
  assertthat::assert_that(
    rlang::is_string(dir_records),
    base::dir.exists(dir_records),
    msg = "`dir_records` must be the path to an existing folder."
  )

  vec_columns <-
    c(
      "product", "file_name", "url", "size_bytes", "sha256",
      "downloaded_utc", "recorded_utc", "method"
    )

  vec_paths_records <-
    base::list.files(
      path = dir_records,
      pattern = "^download_record_.*[.]csv$",
      full.names = TRUE
    ) |>
    base::sort()

  # Every column is read as text first, so an empty record or an empty
  #   column cannot change a type
  list_records <-
    purrr::map(
      .x = vec_paths_records,
      .f = ~ readr::read_csv(
        file = .x,
        col_types = readr::cols(.default = readr::col_character()),
        na = c("", "NA"),
        progress = FALSE
      )
    )

  vec_flag_incomplete <-
    purrr::map_lgl(
      .x = list_records,
      .f = ~ !base::all(vec_columns %in% base::colnames(.x))
    )

  if (
    base::any(vec_flag_incomplete)
  ) {
    vec_files_incomplete <-
      base::basename(vec_paths_records[vec_flag_incomplete])

    cli::cli_abort(
      c(
        "A download record lacks a column.",
        "x" = "{.file {vec_files_incomplete}}",
        "i" = "Every record needs {.field {vec_columns}}."
      )
    )
  }

  data_none <-
    tibble::tibble(
      product = base::character(0),
      file_name = base::character(0),
      url = base::character(0),
      size_bytes = base::character(0),
      sha256 = base::character(0),
      downloaded_utc = base::character(0),
      recorded_utc = base::character(0),
      method = base::character(0)
    )

  res <-
    list_records |>
    purrr::map(.f = ~ dplyr::select(.x, dplyr::all_of(vec_columns))) |>
    purrr::list_rbind() |>
    dplyr::bind_rows(data_none) |>
    tibble::as_tibble() |>
    dplyr::mutate(size_bytes = base::as.numeric(.data[["size_bytes"]])) |>
    dplyr::arrange(
      .data[["product"]],
      .data[["file_name"]],
      .data[["recorded_utc"]]
    )

  return(res)
}

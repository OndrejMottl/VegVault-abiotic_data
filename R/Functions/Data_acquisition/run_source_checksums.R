#' @title Run the checksum record of present raw files behind a flag
#' @description
#' When the checksum flag is set, compute the SHA-256 checksum of every
#' complete file of a source that has no record yet and write a record;
#' otherwise do nothing. This records files that were obtained before the
#' download stage existed.
#' @param data_file_checks
#' A data frame of `diagnose_raw_file()` results for the files of the
#' source, bound together.
#' @param data_expected_files
#' A data frame as returned by `build_source_expected_files()`, with at
#' least the columns `product`, `file_name` and `url`.
#' @param dir_records
#' Character scalar. Folder of the download records.
#' @param flag_record_checksums
#' Logical scalar. `FALSE` (the configured default) returns at once,
#' without reading any file.
#' @param verbose
#' Logical. If `TRUE` (default), progress messages are printed to the
#' console via `cli`.
#' @return
#' A tibble of the rows recorded in this run, with the columns of
#' `load_download_records()` and `method` `"checksum"`; zero rows when
#' nothing was recorded.
#' @details
#' A file counts as recorded when the records hold a row for its product
#' and file name with its current size and a checksum. Only complete files
#' (`flag_complete`) are read; an incomplete file is never recorded. The
#' record is `download_record_<product>_<YYYY-MM-DD>_<HHMMSS>_checksum.csv`
#' (UTC) and is never overwritten. Reading all CHELSA-TraCE21k files takes
#' about 6 minutes at 400 MB per second.
#' @seealso [load_download_records()], [validate_source_raw_files()]
#' @examples
#' \dontrun{
#' run_source_checksums(
#'   data_file_checks = data_file_checks,
#'   data_expected_files = data_expected_files,
#'   dir_records = "Data/Input/Download_records",
#'   flag_record_checksums = FALSE
#' )
#' }
run_source_checksums <- function(
  data_file_checks,
  data_expected_files,
  dir_records,
  flag_record_checksums,
  verbose = TRUE
) {
  vec_columns_checks <-
    c("file_name", "path_file", "size_bytes", "flag_complete")

  assertthat::assert_that(
    base::is.data.frame(data_file_checks),
    base::all(vec_columns_checks %in% base::colnames(data_file_checks)),
    msg = stringr::str_c(
      "`data_file_checks` must be a data frame with the columns ",
      stringr::str_c(vec_columns_checks, collapse = ", "),
      "."
    )
  )

  vec_columns_expected <-
    c("product", "file_name", "url")

  assertthat::assert_that(
    base::is.data.frame(data_expected_files),
    base::all(vec_columns_expected %in% base::colnames(data_expected_files)),
    base::length(
      base::unique(dplyr::pull(data_expected_files, "product"))
    ) == 1L,
    msg = stringr::str_c(
      "`data_expected_files` must be a data frame of one product with ",
      "the columns ",
      stringr::str_c(vec_columns_expected, collapse = ", "),
      "."
    )
  )

  assertthat::assert_that(
    rlang::is_string(dir_records),
    msg = "`dir_records` must be a single folder path."
  )

  assertthat::assert_that(
    rlang::is_bool(flag_record_checksums),
    msg = "`flag_record_checksums` must be TRUE or FALSE."
  )

  assertthat::assert_that(
    rlang::is_bool(verbose),
    msg = "`verbose` must be TRUE or FALSE."
  )

  data_none <-
    tibble::tibble(
      product = base::character(0),
      file_name = base::character(0),
      url = base::character(0),
      size_bytes = base::numeric(0),
      sha256 = base::character(0),
      downloaded_utc = base::character(0),
      recorded_utc = base::character(0),
      method = base::character(0)
    )

  if (
    !isTRUE(flag_record_checksums)
  ) {
    return(data_none)
  }

  sel_product <-
    dplyr::first(dplyr::pull(data_expected_files, "product"))

  data_records_existing <-
    if (
      base::dir.exists(dir_records)
    ) {
      load_download_records(dir_records = dir_records)
    } else {
      data_none
    }

  # A file counts as recorded with its current size and a checksum
  data_recorded <-
    data_records_existing |>
    dplyr::filter(
      .data[["product"]] == sel_product,
      !base::is.na(.data[["sha256"]])
    ) |>
    dplyr::select("file_name", "size_bytes") |>
    dplyr::distinct() |>
    dplyr::mutate(flag_recorded = TRUE)

  data_to_record <-
    data_file_checks |>
    dplyr::filter(.data[["flag_complete"]]) |>
    dplyr::select("file_name", "path_file", "size_bytes") |>
    dplyr::left_join(
      data_recorded,
      by = c("file_name", "size_bytes")
    ) |>
    dplyr::filter(base::is.na(.data[["flag_recorded"]])) |>
    dplyr::left_join(
      dplyr::select(data_expected_files, "file_name", "url"),
      by = "file_name"
    )

  n_to_record <-
    base::nrow(data_to_record)

  if (
    n_to_record == 0L
  ) {
    return(data_none)
  }

  if (
    isTRUE(verbose)
  ) {
    cli::cli_inform("Computing the SHA-256 of {n_to_record} file{?s}.")
  }

  time_recorded <-
    base::Sys.time()

  text_stamp <-
    base::format(time_recorded, format = "%Y-%m-%d_%H%M%S", tz = "UTC")

  res <-
    data_to_record |>
    dplyr::mutate(
      product = sel_product,
      sha256 = purrr::map_chr(
        .x = .data[["path_file"]],
        .f = ~ secretbase::sha256(file = .x)
      ),
      downloaded_utc = NA_character_,
      recorded_utc = base::format(
        time_recorded,
        format = "%Y-%m-%dT%H:%M:%SZ",
        tz = "UTC"
      ),
      method = "checksum"
    ) |>
    dplyr::select(dplyr::all_of(base::colnames(data_none)))

  path_record <-
    base::file.path(
      dir_records,
      stringr::str_c(
        "download_record_", sel_product, "_", text_stamp, "_checksum.csv"
      )
    )

  if (
    base::file.exists(path_record)
  ) {
    cli::cli_abort(
      "The record {.file {path_record}} exists and is never overwritten."
    )
  }

  base::dir.create(
    path = dir_records,
    recursive = TRUE,
    showWarnings = FALSE
  )

  readr::write_csv(
    x = res,
    file = path_record,
    na = ""
  )

  if (
    isTRUE(verbose)
  ) {
    cli::cli_inform(
      "Recorded {n_to_record} checksum{?s} in {.file {path_record}}."
    )
  }

  return(res)
}

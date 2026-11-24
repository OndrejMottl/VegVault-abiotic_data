#' @title Run the downloads of a raw data source behind a flag
#' @description
#' When the download flag is set, download every expected file of a source
#' that is missing and write a download record; otherwise do nothing. A
#' file that exists is never replaced.
#' @param data_expected_files
#' A data frame as returned by `build_source_expected_files()`, with at
#' least the columns `product`, `file_name`, `path_file`, `url` and
#' `expected_size_bytes`.
#' @param flag_download
#' Logical scalar. `FALSE` (the configured default) returns at once,
#' without any network or file access.
#' @param file_check
#' Character scalar, `"geotiff"` or `"zip"`, passed to
#' `save_remote_file()`.
#' @param dir_download
#' Character scalar. Folder for temporary files.
#' @param dir_records
#' Character scalar. Folder of the download records; created when missing.
#' @param verbose
#' Logical. If `TRUE` (default), progress messages are printed to the
#' console via `cli`.
#' @return
#' A tibble of the files downloaded in this run, with the columns
#' `product`, `file_name`, `url`, `size_bytes`, `sha256`, `downloaded_utc`,
#' `recorded_utc` and `method` (`"download"`), as written to the record;
#' zero rows when nothing was downloaded.
#' @details
#' With the flag set, the function first aborts when an expected file
#' exists with a size other than the announced one (it must be moved or
#' deleted by hand, with approval) or when a missing file has no address.
#' Files are then downloaded one after another. The record
#' `download_record_<product>_<YYYY-MM-DD>_<HHMMSS>_download.csv` (UTC)
#' holds the files downloaded before a failure too; the failure is then
#' reported. An existing record is never overwritten. A rerun skips the
#' files already present, so an interrupted run can be resumed.
#' @seealso [save_remote_file()], [load_download_records()],
#'   [validate_source_raw_files()]
#' @examples
#' \dontrun{
#' run_source_downloads(
#'   data_expected_files = data_expected_files,
#'   flag_download = FALSE,
#'   file_check = "geotiff",
#'   dir_download = "Data/Temp/Downloads",
#'   dir_records = "Data/Input/Download_records"
#' )
#' }
run_source_downloads <- function(
  data_expected_files,
  flag_download,
  file_check,
  dir_download,
  dir_records,
  verbose = TRUE
) {
  vec_columns_required <-
    c("product", "file_name", "path_file", "url", "expected_size_bytes")

  assertthat::assert_that(
    base::is.data.frame(data_expected_files),
    base::all(vec_columns_required %in% base::colnames(data_expected_files)),
    base::length(
      base::unique(dplyr::pull(data_expected_files, "product"))
    ) == 1L,
    msg = stringr::str_c(
      "`data_expected_files` must be a data frame of one product with ",
      "the columns ",
      stringr::str_c(vec_columns_required, collapse = ", "),
      "."
    )
  )

  assertthat::assert_that(
    rlang::is_bool(flag_download),
    msg = "`flag_download` must be TRUE or FALSE."
  )

  assertthat::assert_that(
    rlang::is_string(file_check),
    file_check %in% c("geotiff", "zip"),
    msg = "`file_check` must be \"geotiff\" or \"zip\"."
  )

  assertthat::assert_that(
    rlang::is_string(dir_download),
    rlang::is_string(dir_records),
    msg = "`dir_download` and `dir_records` must be single folder paths."
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
    !isTRUE(flag_download)
  ) {
    return(data_none)
  }

  data_state <-
    data_expected_files |>
    dplyr::mutate(
      size_observed = base::as.numeric(
        base::file.size(.data[["path_file"]])
      )
    )

  vec_files_wrong_size <-
    data_state |>
    dplyr::filter(
      !base::is.na(.data[["size_observed"]]),
      !base::is.na(.data[["expected_size_bytes"]]),
      .data[["size_observed"]] != .data[["expected_size_bytes"]]
    ) |>
    dplyr::pull("file_name")

  n_files_wrong_size <-
    base::length(vec_files_wrong_size)

  if (
    n_files_wrong_size > 0L
  ) {
    cli::cli_abort(
      c(
        stringr::str_c(
          "{n_files_wrong_size} file{?s} exist{?s/} with a size other than ",
          "the server announced; a download never overwrites a file."
        ),
        "x" = "{.val {vec_files_wrong_size}}",
        "i" = "Move or delete them by hand first, with approval."
      )
    )
  }

  data_missing <-
    data_state |>
    dplyr::filter(base::is.na(.data[["size_observed"]]))

  if (
    base::nrow(data_missing) == 0L
  ) {
    return(data_none)
  }

  vec_files_without_url <-
    data_missing |>
    dplyr::filter(base::is.na(.data[["url"]])) |>
    dplyr::pull("file_name")

  n_files_without_url <-
    base::length(vec_files_without_url)

  if (
    n_files_without_url > 0L
  ) {
    cli::cli_abort(
      c(
        "No download address for {n_files_without_url} file{?s}.",
        "x" = "{.val {vec_files_without_url}}"
      )
    )
  }

  vec_urls <-
    dplyr::pull(data_missing, "url")

  vec_paths <-
    dplyr::pull(data_missing, "path_file")

  vec_sizes <-
    dplyr::pull(data_missing, "expected_size_bytes")

  # One file after another; a failure is kept so the files downloaded
  #   before it are still recorded. Row numbers instead of `..1`, which
  #   `{targets}` reports as a possibly incorrect context
  list_results <-
    purrr::map(
      .x = base::seq_along(vec_urls),
      .f = ~ base::tryCatch(
        expr = save_remote_file(
          url = vec_urls[.x],
          path_file = vec_paths[.x],
          expected_size_bytes = vec_sizes[.x],
          file_check = file_check,
          dir_download = dir_download,
          verbose = verbose
        ),
        error = function(e) e
      )
    )

  vec_flag_failed <-
    purrr::map_lgl(
      .x = list_results,
      .f = ~ base::inherits(.x, "error")
    )

  sel_product <-
    dplyr::first(dplyr::pull(data_expected_files, "product"))

  time_recorded <-
    base::Sys.time()

  text_stamp <-
    base::format(time_recorded, format = "%Y-%m-%d_%H%M%S", tz = "UTC")

  data_downloaded <-
    list_results[!vec_flag_failed] |>
    purrr::list_rbind()

  data_record <-
    if (
      base::nrow(data_downloaded) == 0L
    ) {
      data_none
    } else {
      data_downloaded |>
        dplyr::mutate(
          product = sel_product,
          recorded_utc = base::format(
            time_recorded,
            format = "%Y-%m-%dT%H:%M:%SZ",
            tz = "UTC"
          ),
          method = "download"
        ) |>
        dplyr::select(dplyr::all_of(base::colnames(data_none)))
    }

  n_recorded <-
    base::nrow(data_record)

  if (
    n_recorded > 0L
  ) {
    path_record <-
      base::file.path(
        dir_records,
        stringr::str_c(
          "download_record_", sel_product, "_", text_stamp, "_download.csv"
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
      x = data_record,
      file = path_record,
      na = ""
    )

    if (
      isTRUE(verbose)
    ) {
      cli::cli_inform(
        "Recorded {n_recorded} download{?s} in {.file {path_record}}."
      )
    }
  }

  if (
    base::any(vec_flag_failed)
  ) {
    vec_files_failed <-
      dplyr::pull(data_missing, "file_name")[vec_flag_failed]

    n_files_failed <-
      base::length(vec_files_failed)

    # Braces in the messages would be read as cli expressions
    vec_messages_failed <-
      purrr::map_chr(
        .x = list_results[vec_flag_failed],
        .f = ~ base::conditionMessage(.x)
      ) |>
      stringr::str_replace_all(pattern = "\\{", replacement = "{{") |>
      stringr::str_replace_all(pattern = "\\}", replacement = "}}")

    vec_bullets_failed <-
      rlang::set_names(
        vec_messages_failed,
        base::rep("*", n_files_failed)
      )

    cli::cli_abort(
      c(
        "{n_files_failed} download{?s} failed.",
        "x" = "{.val {vec_files_failed}}",
        "i" = "The files downloaded before the failure are recorded.",
        vec_bullets_failed
      )
    )
  }

  return(data_record)
}

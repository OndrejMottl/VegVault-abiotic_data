#' @title Save a remote file after checking that it is complete
#' @description
#' Download one file to a temporary name, check that it is complete, then
#' move it to its final path and compute its SHA-256 checksum. An existing
#' file is never overwritten.
#' @param url
#' Character scalar. Address of the file (any protocol `curl` supports,
#' including `file://`).
#' @param path_file
#' Character scalar. Final path of the file. It must not exist.
#' @param expected_size_bytes
#' Numeric scalar. The size the server announced, or `NA` when it announces
#' none.
#' @param file_check
#' Character scalar, `"geotiff"` or `"zip"`: how the content is checked,
#' as in `diagnose_raw_file()`.
#' @param dir_download
#' Character scalar. Folder for the temporary file, created when missing.
#' @param verbose
#' Logical. If `TRUE` (default), progress messages are printed to the
#' console via `cli`.
#' @return
#' A one-row tibble with the columns `file_name`, `url`, `size_bytes`
#' (numeric), `sha256` (character) and `downloaded_utc` (character, UTC,
#' ISO 8601 with seconds, when the download finished).
#' @details
#' The temporary file is `<dir_download>/<file name>.part`; a leftover one
#' from an interrupted run is replaced, as it is never a complete file.
#' The download is accepted only when `diagnose_raw_file()` finds it
#' complete; otherwise the temporary file is deleted and the function
#' aborts, so an incomplete download can never reach `path_file`. The
#' folder of `path_file` is created when missing.
#' @seealso [diagnose_raw_file()], [run_source_downloads()]
#' @examples
#' \dontrun{
#' save_remote_file(
#'   url = "https://example.org/bio/CHELSA_bio1_1981-2010_V.2.1.tif",
#'   path_file = "Data/Input/Neoclimate/CHELSA_V2.1/bio1.tif",
#'   expected_size_bytes = 115490323,
#'   file_check = "geotiff",
#'   dir_download = "Data/Temp/Downloads"
#' )
#' }
save_remote_file <- function(
  url,
  path_file,
  expected_size_bytes,
  file_check,
  dir_download,
  verbose = TRUE
) {
  assertthat::assert_that(
    rlang::is_string(url),
    !base::is.na(url),
    base::nzchar(url),
    msg = "`url` must be one non-empty address."
  )

  assertthat::assert_that(
    rlang::is_string(path_file),
    !base::is.na(path_file),
    msg = "`path_file` must be a single file path."
  )

  assertthat::assert_that(
    base::is.numeric(expected_size_bytes),
    base::length(expected_size_bytes) == 1L,
    msg = "`expected_size_bytes` must be one number, or NA."
  )

  assertthat::assert_that(
    rlang::is_string(file_check),
    file_check %in% c("geotiff", "zip"),
    msg = "`file_check` must be \"geotiff\" or \"zip\"."
  )

  assertthat::assert_that(
    rlang::is_string(dir_download),
    !base::is.na(dir_download),
    msg = "`dir_download` must be a single folder path."
  )

  assertthat::assert_that(
    rlang::is_bool(verbose),
    msg = "`verbose` must be TRUE or FALSE."
  )

  if (
    base::file.exists(path_file)
  ) {
    cli::cli_abort(
      c(
        "{.file {path_file}} exists; a download never overwrites a file.",
        "i" = "Move or delete it by hand first, with approval."
      )
    )
  }

  file_name <-
    base::basename(path_file)

  path_part <-
    base::file.path(dir_download, stringr::str_c(file_name, ".part"))

  base::dir.create(
    path = dir_download,
    recursive = TRUE,
    showWarnings = FALSE
  )

  # A leftover part file is never complete, so it is replaced
  if (
    base::file.exists(path_part)
  ) {
    base::file.remove(path_part)
  }

  if (
    isTRUE(verbose)
  ) {
    cli::cli_inform("Downloading {.file {file_name}} from {.url {url}}.")
  }

  res_download <-
    base::tryCatch(
      expr = curl::curl_download(
        url = url,
        destfile = path_part,
        quiet = TRUE,
        mode = "wb"
      ),
      error = function(e) e
    )

  if (
    base::inherits(res_download, "error")
  ) {
    if (
      base::file.exists(path_part)
    ) {
      base::file.remove(path_part)
    }

    cli::cli_abort(
      c(
        "The download of {.file {file_name}} failed.",
        "x" = base::conditionMessage(res_download)
      )
    )
  }

  data_check <-
    diagnose_raw_file(
      path_file = path_part,
      expected_size_bytes = expected_size_bytes,
      file_check = file_check
    )

  if (
    !dplyr::pull(data_check, "flag_complete")
  ) {
    base::file.remove(path_part)

    cli::cli_abort(
      c(
        "The download of {.file {file_name}} is incomplete; it was deleted.",
        "x" = dplyr::pull(data_check, "detail")
      )
    )
  }

  base::dir.create(
    path = base::dirname(path_file),
    recursive = TRUE,
    showWarnings = FALSE
  )

  fs::file_move(path = path_part, new_path = path_file)

  time_finished <-
    base::format(
      base::Sys.time(),
      format = "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    )

  res <-
    tibble::tibble(
      file_name = file_name,
      url = url,
      size_bytes = base::as.numeric(base::file.size(path_file)),
      sha256 = secretbase::sha256(file = path_file),
      downloaded_utc = time_finished
    )

  if (
    isTRUE(verbose)
  ) {
    cli::cli_inform("Saved {.file {file_name}} ({res[['size_bytes']]} bytes).")
  }

  return(res)
}

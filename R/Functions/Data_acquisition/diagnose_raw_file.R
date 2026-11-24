#' @title Diagnose whether a raw file is present and complete
#' @description
#' Check one raw file on disk: that it exists, that its size equals the
#' size the server announced, and that its content is whole, without
#' reading pixel data.
#' @param path_file
#' Character scalar. Path of the file.
#' @param expected_size_bytes
#' Numeric scalar. The size the server announced, or `NA` when the server
#' announces none; the size rule is then not evaluated.
#' @param file_check
#' Character scalar. How the content is checked: `"geotiff"` (no data block
#' of the TIFF block table ends beyond the end of the file, see
#' `compute_tiff_block_extent()`) or `"zip"` (the archive opens and lists
#' at least one member; a truncated zip loses its central directory).
#' @return
#' A one-row tibble with the columns `file_name` (base name of
#' `path_file`), `path_file`, `flag_present` (logical), `size_bytes`
#' (numeric, `NA` when
#' missing), `flag_size_ok` (logical; `NA` when `expected_size_bytes` is
#' `NA` or the file is missing), `flag_content_ok` (logical; `NA` when the
#' file is missing), `flag_complete` (logical: present, size not wrong and
#' content whole) and `detail` (character: what is wrong, or `""`).
#' @details
#' A missing or unreadable file does not abort: it is reported, so a
#' pipeline that maps over many files fails on that file with all others
#' diagnosed.
#' @seealso [compute_tiff_block_extent()], [save_remote_file()],
#'   [validate_source_raw_files()]
#' @examples
#' \dontrun{
#' diagnose_raw_file(
#'   path_file = "Data/Input/HWSD/hwsd2.2_db.zip",
#'   expected_size_bytes = NA_real_,
#'   file_check = "zip"
#' )
#' }
diagnose_raw_file <- function(
  path_file,
  expected_size_bytes,
  file_check
) {
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

  flag_present <-
    base::file.exists(path_file)

  size_bytes <-
    if (
      isTRUE(flag_present)
    ) {
      base::as.numeric(base::file.size(path_file))
    } else {
      NA_real_
    }

  flag_size_ok <-
    if (
      isTRUE(flag_present) && !base::is.na(expected_size_bytes)
    ) {
      size_bytes == expected_size_bytes
    } else {
      NA
    }

  # Content: the TIFF block table must fit inside the file; a zip archive
  #   must open, which needs its central directory at the end of the file
  list_content <-
    if (
      !isTRUE(flag_present)
    ) {
      base::list(flag_ok = NA, detail = "")
    } else if (
      file_check == "geotiff"
    ) {
      data_blocks <-
        compute_tiff_block_extent(path_file = path_file)

      text_read_error <-
        dplyr::pull(data_blocks, "read_error")

      n_blocks_beyond_end <-
        dplyr::pull(data_blocks, "n_blocks_beyond_end")

      if (
        !base::is.na(text_read_error)
      ) {
        base::list(
          flag_ok = FALSE,
          detail = stringr::str_c(
            "The TIFF header could not be read: ", text_read_error
          )
        )
      } else if (
        n_blocks_beyond_end > 0
      ) {
        base::list(
          flag_ok = FALSE,
          detail = stringr::str_c(
            n_blocks_beyond_end,
            " blocks end beyond the end of the file, from row ",
            dplyr::pull(data_blocks, "first_affected_row"),
            " (counted from 0)."
          )
        )
      } else {
        base::list(flag_ok = TRUE, detail = "")
      }
    } else {
      data_members <-
        base::tryCatch(
          expr = utils::unzip(zipfile = path_file, list = TRUE),
          error = function(e) NULL,
          warning = function(w) NULL
        )

      if (
        base::is.null(data_members) || base::nrow(data_members) == 0L
      ) {
        base::list(
          flag_ok = FALSE,
          detail = "The zip archive cannot be opened; it may be truncated."
        )
      } else {
        base::list(flag_ok = TRUE, detail = "")
      }
    }

  text_size_detail <-
    if (
      isFALSE(flag_size_ok)
    ) {
      stringr::str_c(
        base::format(size_bytes, scientific = FALSE),
        " bytes, the server announced ",
        base::format(expected_size_bytes, scientific = FALSE),
        "."
      )
    } else {
      ""
    }

  vec_details <-
    c(text_size_detail, list_content[["detail"]])

  text_detail <-
    if (
      !isTRUE(flag_present)
    ) {
      "The file is missing."
    } else {
      stringr::str_c(
        vec_details[base::nzchar(vec_details)],
        collapse = " "
      )
    }

  res <-
    tibble::tibble(
      file_name = base::basename(path_file),
      path_file = path_file,
      flag_present = flag_present,
      size_bytes = size_bytes,
      flag_size_ok = flag_size_ok,
      flag_content_ok = list_content[["flag_ok"]],
      flag_complete = isTRUE(flag_present) &&
        !isFALSE(flag_size_ok) &&
        isTRUE(list_content[["flag_ok"]]),
      detail = text_detail
    )

  return(res)
}

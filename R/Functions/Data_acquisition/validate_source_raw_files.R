#' @title Validate that every raw file of a source is complete and recorded
#' @description
#' Stop unless every expected file of a raw data source is present,
#' complete and listed in the download records with its current size and a
#' checksum. The message names the configuration flag that fixes each
#' problem and the stage runner to execute.
#' @param data_expected_files
#' A data frame as returned by `build_source_expected_files()`.
#' @param data_file_checks
#' A data frame of `diagnose_raw_file()` results for the files of the
#' source, bound together.
#' @param data_download_records
#' A data frame as returned by `load_download_records()`.
#' @param sel_source
#' Character scalar. Name of the source in `data_sources` of
#' `config.yml`, used in the messages.
#' @return
#' A tibble with one row per expected file and the columns `product`,
#' `variable`, `age`, `time_id`, `file_name`, `path_file`, `url`,
#' `size_bytes`, `sha256`, `downloaded_utc` and `method`, taken from the
#' latest matching record.
#' @details
#' Problems are collected before stopping, so one message lists every
#' missing or incomplete file (fixed by `flag_download`, after an
#' incomplete file is moved away by hand) and every file without a record
#' (fixed by `flag_record_checksums`). A file without a diagnosis row
#' counts as missing.
#' @seealso [diagnose_raw_file()], [load_download_records()],
#'   [run_source_downloads()], [run_source_checksums()]
#' @examples
#' \dontrun{
#' validate_source_raw_files(
#'   data_expected_files = data_expected_files,
#'   data_file_checks = data_file_checks,
#'   data_download_records = data_download_records,
#'   sel_source = "chelsa_v21"
#' )
#' }
validate_source_raw_files <- function(
  data_expected_files,
  data_file_checks,
  data_download_records,
  sel_source
) {
  vec_columns_expected <-
    c(
      "product", "variable", "age", "time_id", "file_name", "path_file",
      "url"
    )

  assertthat::assert_that(
    base::is.data.frame(data_expected_files),
    base::all(vec_columns_expected %in% base::colnames(data_expected_files)),
    msg = stringr::str_c(
      "`data_expected_files` must be a data frame with the columns ",
      stringr::str_c(vec_columns_expected, collapse = ", "),
      "."
    )
  )

  vec_columns_checks <-
    c("file_name", "flag_present", "size_bytes", "flag_complete", "detail")

  assertthat::assert_that(
    base::is.data.frame(data_file_checks),
    base::all(vec_columns_checks %in% base::colnames(data_file_checks)),
    msg = stringr::str_c(
      "`data_file_checks` must be a data frame with the columns ",
      stringr::str_c(vec_columns_checks, collapse = ", "),
      "."
    )
  )

  vec_columns_records <-
    c(
      "product", "file_name", "size_bytes", "sha256", "downloaded_utc",
      "recorded_utc", "method"
    )

  assertthat::assert_that(
    base::is.data.frame(data_download_records),
    base::all(
      vec_columns_records %in% base::colnames(data_download_records)
    ),
    msg = stringr::str_c(
      "`data_download_records` must be a data frame with the columns ",
      stringr::str_c(vec_columns_records, collapse = ", "),
      "."
    )
  )

  assertthat::assert_that(
    rlang::is_string(sel_source),
    msg = "`sel_source` must be the name of one source."
  )

  # A file without a diagnosis row counts as missing
  data_checked <-
    data_expected_files |>
    dplyr::select(dplyr::all_of(vec_columns_expected)) |>
    dplyr::left_join(
      dplyr::select(data_file_checks, dplyr::all_of(vec_columns_checks)),
      by = "file_name",
      relationship = "one-to-one"
    )

  # The latest record of each file with its current size and a checksum
  data_records_latest <-
    data_download_records |>
    dplyr::filter(!base::is.na(.data[["sha256"]])) |>
    dplyr::arrange(
      .data[["product"]],
      .data[["file_name"]],
      .data[["recorded_utc"]]
    ) |>
    dplyr::slice_tail(
      n = 1,
      by = dplyr::all_of(c("product", "file_name", "size_bytes"))
    ) |>
    dplyr::select(
      "product", "file_name", "size_bytes", "sha256", "downloaded_utc",
      "method"
    )

  data_joined <-
    data_checked |>
    dplyr::left_join(
      data_records_latest,
      by = c("product", "file_name", "size_bytes"),
      relationship = "many-to-one"
    )

  vec_files_missing <-
    data_joined |>
    dplyr::filter(!(.data[["flag_present"]] %in% TRUE)) |>
    dplyr::pull("file_name")

  data_incomplete <-
    data_joined |>
    dplyr::filter(
      .data[["flag_present"]] %in% TRUE,
      !(.data[["flag_complete"]] %in% TRUE)
    )

  vec_files_unrecorded <-
    data_joined |>
    dplyr::filter(
      .data[["flag_complete"]] %in% TRUE,
      base::is.na(.data[["sha256"]])
    ) |>
    dplyr::pull("file_name")

  n_missing <-
    base::length(vec_files_missing)

  n_incomplete <-
    base::nrow(data_incomplete)

  n_unrecorded <-
    base::length(vec_files_unrecorded)

  text_runner <-
    "R/Analyses/01_Data_acquisition/00_run.R"

  # Braces in the details would be read as cli expressions
  vec_details_incomplete <-
    stringr::str_c(
      dplyr::pull(data_incomplete, "file_name"),
      ": ",
      dplyr::pull(data_incomplete, "detail")
    ) |>
    stringr::str_replace_all(pattern = "\\{", replacement = "{{") |>
    stringr::str_replace_all(pattern = "\\}", replacement = "}}")

  vec_bullets_missing <-
    if (
      n_missing > 0L
    ) {
      c(
        "x" = "Missing ({n_missing}): {.val {vec_files_missing}}.",
        "i" = stringr::str_c(
          "Set `flag_download: true` for source `{sel_source}` in ",
          "`data_sources` of `config.yml` and run `{text_runner}`."
        )
      )
    } else {
      base::character(0)
    }

  vec_bullets_incomplete <-
    if (
      n_incomplete > 0L
    ) {
      c(
        "x" = "Incomplete ({n_incomplete}):",
        rlang::set_names(
          vec_details_incomplete,
          base::rep("*", n_incomplete)
        ),
        "i" = stringr::str_c(
          "An incomplete file is never overwritten: move it away by hand, ",
          "with approval, then download it with `flag_download: true`."
        )
      )
    } else {
      base::character(0)
    }

  vec_bullets_unrecorded <-
    if (
      n_unrecorded > 0L
    ) {
      c(
        "x" = "Without a download record ({n_unrecorded}): ",
        "*" = "{.val {vec_files_unrecorded}}",
        "i" = stringr::str_c(
          "Set `flag_record_checksums: true` for source `{sel_source}` in ",
          "`data_sources` of `config.yml` and run `{text_runner}`."
        )
      )
    } else {
      base::character(0)
    }

  if (
    n_missing + n_incomplete + n_unrecorded > 0L
  ) {
    cli::cli_abort(
      c(
        "The raw files of source {.val {sel_source}} are not ready.",
        vec_bullets_missing,
        vec_bullets_incomplete,
        vec_bullets_unrecorded
      )
    )
  }

  res <-
    data_joined |>
    dplyr::select(
      "product", "variable", "age", "time_id", "file_name", "path_file",
      "url", "size_bytes", "sha256", "downloaded_utc", "method"
    )

  return(res)
}

#' @title Save the tables of a raster quality check
#' @description
#' Write the tables of one raster quality-check set as CSV files with
#' stable names, so a rerun replaces them: the band statistics, the
#' findings that did not pass, the per-file summary and, when given, the
#' pair comparisons.
#' @param data_band_summary
#' A data frame of `compute_raster_band_summary()` results, bound together.
#' @param data_findings
#' A data frame of findings, as returned by the `diagnose_raster_*()`
#' functions and bound together.
#' @param data_file_summary
#' A data frame as returned by `summarise_raster_quality()`.
#' @param dir_output
#' Character scalar. Folder to write into; created when missing.
#' @param table_prefix
#' Character scalar of lower-case letters, digits and underscores, for
#' example `"raster_quality_neoclimate_processed"`. The files are named
#' `<table_prefix>_bands.csv`, `_findings.csv`, `_summary.csv` and
#' `_pairs.csv`.
#' @param data_pair_comparison
#' A data frame of `compute_raster_pair_comparison()` results, or `NULL`
#' (default) when the set has no pair rules.
#' @return
#' The paths of the written files, as a named character vector, invisibly.
#' @details
#' Existing files with the same names are overwritten; that is the purpose
#' of the stable names. Only findings with status `"fail"` or `"allowed"`
#' are written. Aborts before writing anything when a `file_name` or
#' `file_name_reference` value holds a path rather than a base name, so
#' the tables never contain machine-specific paths.
#' @seealso [summarise_raster_quality()], [validate_raster_quality()]
#' @examples
#' \dontrun{
#' save_raster_quality_tables(
#'   data_band_summary = data_band_summary,
#'   data_findings = data_findings,
#'   data_file_summary = data_file_summary,
#'   dir_output = here::here("Outputs", "Tables"),
#'   table_prefix = "raster_quality_neoclimate_processed"
#' )
#' }
save_raster_quality_tables <- function(
  data_band_summary,
  data_findings,
  data_file_summary,
  dir_output,
  table_prefix,
  data_pair_comparison = NULL
) {
  assertthat::assert_that(
    base::is.data.frame(data_band_summary),
    "file_name" %in% base::colnames(data_band_summary),
    msg = "`data_band_summary` must be a data frame with `file_name`."
  )

  assertthat::assert_that(
    base::is.data.frame(data_findings),
    base::all(c("file_name", "status") %in% base::colnames(data_findings)),
    msg = "`data_findings` must be a data frame with `file_name` and `status`."
  )

  assertthat::assert_that(
    base::is.data.frame(data_file_summary),
    "file_name" %in% base::colnames(data_file_summary),
    msg = "`data_file_summary` must be a data frame with `file_name`."
  )

  assertthat::assert_that(
    base::is.null(data_pair_comparison) ||
      base::is.data.frame(data_pair_comparison),
    msg = "`data_pair_comparison` must be NULL or a data frame."
  )

  assertthat::assert_that(
    base::is.character(dir_output),
    base::length(dir_output) == 1L,
    !base::is.na(dir_output),
    msg = "`dir_output` must be a single folder path."
  )

  assertthat::assert_that(
    base::is.character(table_prefix),
    base::length(table_prefix) == 1L,
    !base::is.na(table_prefix),
    stringr::str_detect(table_prefix, "^[a-z0-9_]+$"),
    msg = stringr::str_c(
      "`table_prefix` must be one name of lower-case letters, digits and ",
      "underscores."
    )
  )

  list_tables <-
    list(
      bands = data_band_summary,
      findings = dplyr::filter(data_findings, .data[["status"]] != "pass"),
      summary = data_file_summary,
      pairs = data_pair_comparison
    ) |>
    purrr::compact()

  # Base names only: a path in a file-name column would put a
  #   machine-specific path into the report
  vec_file_names <-
    list_tables |>
    purrr::map(
      .f = ~ dplyr::select(
        .x,
        dplyr::any_of(c("file_name", "file_name_reference"))
      ) |>
        base::unlist(use.names = FALSE)
    ) |>
    base::unlist(use.names = FALSE)

  vec_paths_found <-
    vec_file_names[stringr::str_detect(vec_file_names, "[/\\\\]")]

  if (
    base::length(vec_paths_found) > 0L
  ) {
    cli::cli_abort(
      c(
        "The quality tables must hold file names, not paths.",
        "x" = "Found {.val {utils::head(vec_paths_found, 3)}}."
      )
    )
  }

  base::dir.create(dir_output, recursive = TRUE, showWarnings = FALSE)

  res <-
    list_tables |>
    purrr::imap_chr(
      .f = ~ {
        path_table <-
          base::file.path(
            dir_output,
            stringr::str_c(table_prefix, "_", .y, ".csv")
          )

        readr::write_csv(x = .x, file = path_table, na = "")

        return(path_table)
      }
    )

  return(invisible(res))
}

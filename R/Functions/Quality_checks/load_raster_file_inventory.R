#' @title Load the file-system inventory of expected raster files
#' @description
#' Add the observed size and modification time of every expected raster
#' file, so that a pipeline can tell when a file was replaced and its
#' checks must run again.
#' @param data_expected_files
#' A data frame with one row per expected file and at least the column
#' `path_file` (character, no `NA`), as returned by
#' `build_raster_quality_expected_files()`.
#' @return
#' `data_expected_files` with two more columns: `observed_size_bytes`
#' (numeric) and `observed_modified_time` (character, UTC, ISO 8601 with
#' seconds), both `NA` for a file that does not exist.
#' @details
#' Reads only file-system metadata, never file contents. A missing file
#' is not an error here; `compute_raster_band_summary()` and
#' `diagnose_raster_files()` report it.
#' @seealso [build_raster_quality_expected_files()]
#' @examples
#' \dontrun{
#' load_raster_file_inventory(data_expected_files = data_expected_files)
#' }
load_raster_file_inventory <- function(data_expected_files) {
  assertthat::assert_that(
    base::is.data.frame(data_expected_files),
    "path_file" %in% base::colnames(data_expected_files),
    msg = "`data_expected_files` must be a data frame with `path_file`."
  )

  vec_paths <-
    dplyr::pull(data_expected_files, "path_file")

  assertthat::assert_that(
    base::is.character(vec_paths),
    !base::anyNA(vec_paths),
    msg = "`path_file` of `data_expected_files` must be text without NA."
  )

  data_file_info <-
    base::file.info(vec_paths, extra_cols = FALSE)

  res <-
    data_expected_files |>
    tibble::as_tibble() |>
    dplyr::mutate(
      observed_size_bytes = base::as.numeric(data_file_info[["size"]]),
      observed_modified_time = base::format(
        data_file_info[["mtime"]],
        format = "%Y-%m-%dT%H:%M:%SZ",
        tz = "UTC"
      )
    )

  return(res)
}

#' @title Build the file plan of a climate processing stage
#' @description
#' For every validated raw file of a climate source, name the aggregated
#' raster and the final processed raster the processing stage writes.
#' @param data_raw_files
#' A data frame as returned by `validate_source_raw_files()`, with at
#' least the columns `product`, `variable`, `age`, `time_id`,
#' `file_name`, `path_file` (the raw file) and `sha256`.
#' @param dir_processed
#' Character scalar. Folder of the final processed rasters; the aggregated
#' rasters before any mask go to its subfolder `Aggregated`.
#' @return
#' A tibble with one row per raw file, in the order of `data_raw_files`,
#' and the columns `product`, `variable`, `age`, `time_id`, `file_name`,
#' `path_raw`, `sha256_raw`, `path_aggregated` and `path_final`. The
#' processed files keep the raw file names.
#' @details
#' The checksum of the raw file is part of each row, so a pipeline that
#' branches over the rows reruns the branch of a replaced raw file even
#' when its path stays. Aborts when a file name is repeated, so no two raw
#' files can write the same processed raster.
#' @seealso [save_aggregated_raster()], [save_masked_raster()]
#' @examples
#' build_processing_files(
#'   data_raw_files = tibble::tibble(
#'     product = "CHELSA_V2.1",
#'     variable = "bio1",
#'     age = 0,
#'     time_id = NA_integer_,
#'     file_name = "CHELSA_bio1_1981-2010_V.2.1.tif",
#'     path_file = "Data/Input/Neoclimate/CHELSA_bio1_1981-2010_V.2.1.tif"
#'   ),
#'   dir_processed = "Data/Processed/CHELSA_V2.1"
#' )
build_processing_files <- function(
  data_raw_files,
  dir_processed
) {
  vec_columns_required <-
    c(
      "product", "variable", "age", "time_id", "file_name", "path_file",
      "sha256"
    )

  assertthat::assert_that(
    base::is.data.frame(data_raw_files),
    base::all(vec_columns_required %in% base::colnames(data_raw_files)),
    msg = stringr::str_c(
      "`data_raw_files` must be a data frame with the columns ",
      stringr::str_c(vec_columns_required, collapse = ", "),
      "."
    )
  )

  assertthat::assert_that(
    rlang::is_string(dir_processed),
    !base::is.na(dir_processed),
    base::nzchar(dir_processed),
    msg = "`dir_processed` must be one non-empty folder path."
  )

  vec_files_repeated <-
    data_raw_files |>
    dplyr::count(.data[["file_name"]]) |>
    dplyr::filter(.data[["n"]] > 1L) |>
    dplyr::pull("file_name")

  if (
    base::length(vec_files_repeated) > 0L
  ) {
    cli::cli_abort(
      c(
        "Two raw files would write the same processed raster.",
        "x" = "{.val {vec_files_repeated}}"
      )
    )
  }

  res <-
    data_raw_files |>
    dplyr::mutate(
      path_raw = .data[["path_file"]],
      sha256_raw = .data[["sha256"]],
      path_aggregated = base::file.path(
        dir_processed,
        "Aggregated",
        .data[["file_name"]]
      ),
      path_final = base::file.path(dir_processed, .data[["file_name"]])
    ) |>
    dplyr::select(
      "product", "variable", "age", "time_id", "file_name", "path_raw",
      "sha256_raw", "path_aggregated", "path_final"
    )

  return(res)
}

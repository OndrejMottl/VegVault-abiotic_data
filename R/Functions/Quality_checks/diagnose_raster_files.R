#' @title Diagnose the file-level quality rules of a set of rasters
#' @description
#' Check every expected raster file of a quality-check set: that it was
#' summarised, could be read, has a known storage type, the expected size
#' in bytes, no data block beyond the end of the file, and the expected
#' number of rows and columns and extent.
#' @param data_expected_files
#' A data frame with one row per expected file and the columns `product`,
#' `variable` (character), `age` (numeric), `file_name` (character,
#' unique), `expected_size_bytes` (numeric; `NA` to skip the size rule),
#' `expected_n_col`, `expected_n_row`, `expected_x_min`, `expected_x_max`,
#' `expected_y_min` and `expected_y_max` (numeric).
#' @param data_band_summary
#' A data frame of `compute_raster_band_summary()` results for the files
#' of the set, bound together.
#' @param data_block_extent
#' A data frame of `compute_tiff_block_extent()` results for the files of
#' the set, bound together, or `NULL` to skip the block rule (for example
#' for files that are not TIFF). Default `NULL`.
#' @param data_allow_list
#' Allow-list as described in `resolve_quality_findings()`.
#' @param extent_tolerance
#' Positive number. Largest accepted difference, in degrees, between an
#' extent edge and its expected value. Default `1e-6`.
#' @return
#' A tibble of findings as returned by `resolve_quality_findings()`, with
#' the band columns `NA`. Rules: `file_present`, `file_readable`,
#' `data_type_known`, `file_size`, `block_extent`, `dimensions` and
#' `extent`.
#' @details
#' Rules that need a readable file are evaluated only for readable files;
#' a file that is missing or unreadable fails on that rule instead. The
#' `block_extent` finding of a truncated file names the first affected row
#' and the latitude of its top edge, or says that the latitude is unknown
#' when the file could not be read. All rules have fixed thresholds, so no
#' threshold table is needed.
#' @seealso [compute_raster_band_summary()], [compute_tiff_block_extent()],
#'   [resolve_quality_findings()]
#' @examples
#' \dontrun{
#' diagnose_raster_files(
#'   data_expected_files = data_expected_files,
#'   data_band_summary = data_band_summary,
#'   data_block_extent = data_block_extent,
#'   data_allow_list = data_allow_list
#' )
#' }
diagnose_raster_files <- function(
  data_expected_files,
  data_band_summary,
  data_block_extent = NULL,
  data_allow_list,
  extent_tolerance = 1e-6
) {
  vec_columns_expected <-
    c(
      "product", "variable", "age", "file_name", "expected_size_bytes",
      "expected_n_col", "expected_n_row", "expected_x_min",
      "expected_x_max", "expected_y_min", "expected_y_max"
    )

  vec_columns_band <-
    c(
      "file_name", "n_col", "n_row", "x_min", "x_max", "y_min", "y_max",
      "data_type", "file_size_bytes", "read_error"
    )

  vec_columns_block <-
    c("file_name", "n_blocks_beyond_end", "first_affected_row", "read_error")

  assertthat::assert_that(
    base::is.data.frame(data_expected_files),
    base::all(vec_columns_expected %in% base::colnames(data_expected_files)),
    !base::anyDuplicated(dplyr::pull(data_expected_files, "file_name")),
    msg = stringr::str_c(
      "`data_expected_files` must be a data frame with unique `file_name` ",
      "and the columns ",
      stringr::str_c(vec_columns_expected, collapse = ", "),
      "."
    )
  )

  assertthat::assert_that(
    base::is.data.frame(data_band_summary),
    base::all(vec_columns_band %in% base::colnames(data_band_summary)),
    msg = stringr::str_c(
      "`data_band_summary` must be a data frame with the columns ",
      stringr::str_c(vec_columns_band, collapse = ", "),
      "."
    )
  )

  assertthat::assert_that(
    base::is.null(data_block_extent) ||
      (base::is.data.frame(data_block_extent) &&
        base::all(vec_columns_block %in% base::colnames(data_block_extent))),
    msg = stringr::str_c(
      "`data_block_extent` must be NULL or a data frame with the columns ",
      stringr::str_c(vec_columns_block, collapse = ", "),
      "."
    )
  )

  assertthat::assert_that(
    base::is.numeric(extent_tolerance),
    base::length(extent_tolerance) == 1L,
    base::is.finite(extent_tolerance),
    extent_tolerance > 0,
    msg = "`extent_tolerance` must be a positive number."
  )

  # File-level columns repeat on every band row; keep one row per file
  data_file_properties <-
    data_band_summary |>
    dplyr::select(dplyr::all_of(vec_columns_band)) |>
    dplyr::distinct(.data[["file_name"]], .keep_all = TRUE) |>
    dplyr::mutate(flag_summarised = TRUE)

  data_files <-
    data_expected_files |>
    dplyr::select(dplyr::all_of(vec_columns_expected)) |>
    dplyr::left_join(
      data_file_properties,
      by = dplyr::join_by("file_name"),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      flag_present = dplyr::coalesce(.data[["flag_summarised"]], FALSE),
      flag_readable = .data[["flag_present"]] &
        base::is.na(.data[["read_error"]])
    )

  data_present <-
    dplyr::filter(data_files, .data[["flag_present"]])

  data_readable <-
    dplyr::filter(data_files, .data[["flag_readable"]])

  list_evaluations <-
    list(
      data_files |>
        dplyr::mutate(
          rule = "file_present",
          value = base::as.numeric(!.data[["flag_present"]]),
          threshold = 0,
          detail = dplyr::if_else(
            .data[["flag_present"]],
            "",
            "The file was not summarised; it is missing from the set."
          )
        ),
      data_present |>
        dplyr::mutate(
          rule = "file_readable",
          value = base::as.numeric(!base::is.na(.data[["read_error"]])),
          threshold = 0,
          detail = dplyr::coalesce(.data[["read_error"]], "")
        ),
      data_readable |>
        dplyr::mutate(
          rule = "data_type_known",
          value = base::as.numeric(
            base::is.na(.data[["data_type"]]) |
              !base::nzchar(dplyr::coalesce(.data[["data_type"]], ""))
          ),
          threshold = 0,
          detail = stringr::str_c(
            "Storage type: ",
            dplyr::coalesce(.data[["data_type"]], "unknown"),
            "."
          )
        ),
      data_present |>
        dplyr::filter(!base::is.na(.data[["expected_size_bytes"]])) |>
        dplyr::mutate(
          rule = "file_size",
          value = base::abs(
            .data[["file_size_bytes"]] - .data[["expected_size_bytes"]]
          ),
          threshold = 0,
          detail = stringr::str_c(
            base::format(.data[["file_size_bytes"]], scientific = FALSE),
            " bytes, expected ",
            base::format(.data[["expected_size_bytes"]], scientific = FALSE),
            "."
          )
        ),
      data_readable |>
        dplyr::mutate(
          rule = "dimensions",
          value = base::as.numeric(
            .data[["n_col"]] != .data[["expected_n_col"]] |
              .data[["n_row"]] != .data[["expected_n_row"]]
          ),
          threshold = 0,
          detail = stringr::str_c(
            .data[["n_col"]], " by ", .data[["n_row"]],
            " cells, expected ",
            .data[["expected_n_col"]], " by ", .data[["expected_n_row"]], "."
          )
        ),
      data_readable |>
        dplyr::mutate(
          rule = "extent",
          value = base::pmax(
            base::abs(.data[["x_min"]] - .data[["expected_x_min"]]),
            base::abs(.data[["x_max"]] - .data[["expected_x_max"]]),
            base::abs(.data[["y_min"]] - .data[["expected_y_min"]]),
            base::abs(.data[["y_max"]] - .data[["expected_y_max"]])
          ),
          threshold = extent_tolerance,
          detail = stringr::str_c(
            "x ", .data[["x_min"]], " to ", .data[["x_max"]],
            ", y ", .data[["y_min"]], " to ", .data[["y_max"]], "."
          )
        )
    )

  # Blocks beyond the end of the file: a truncated download. The detail
  #   names the first affected row and the latitude of its top edge
  data_evaluations_block <-
    if (
      base::is.null(data_block_extent)
    ) {
      NULL
    } else {
      data_present |>
        dplyr::left_join(
          data_block_extent |>
            dplyr::select(dplyr::all_of(vec_columns_block)) |>
            dplyr::rename(read_error_block = "read_error") |>
            dplyr::mutate(flag_block = TRUE),
          by = dplyr::join_by("file_name"),
          relationship = "one-to-one"
        ) |>
        dplyr::mutate(
          lat_first_affected = .data[["y_max"]] -
            .data[["first_affected_row"]] *
              (.data[["y_max"]] - .data[["y_min"]]) / .data[["n_row"]],
          rule = "block_extent",
          value = dplyr::case_when(
            base::is.na(.data[["flag_block"]]) ~ 1,
            !base::is.na(.data[["read_error_block"]]) ~ 1,
            .default = .data[["n_blocks_beyond_end"]]
          ),
          threshold = 0,
          detail = dplyr::case_when(
            base::is.na(.data[["flag_block"]]) ~
              "No block table was computed for the file.",
            !base::is.na(.data[["read_error_block"]]) ~
              .data[["read_error_block"]],
            .data[["n_blocks_beyond_end"]] > 0 ~ stringr::str_c(
              .data[["n_blocks_beyond_end"]],
              " blocks end beyond the end of the file; the first affected ",
              "row is row ", .data[["first_affected_row"]],
              " (counted from 0), ",
              dplyr::if_else(
                base::is.na(.data[["lat_first_affected"]]),
                "whose latitude is unknown because the file was not read.",
                stringr::str_c(
                  "whose top edge lies at ",
                  base::round(.data[["lat_first_affected"]], 4),
                  " degrees."
                )
              )
            ),
            .default = ""
          )
        )
    }

  data_evaluations <-
    c(list_evaluations, list(data_evaluations_block)) |>
    purrr::compact() |>
    purrr::map(
      .f = ~ dplyr::select(
        .x,
        "product", "variable", "age", "file_name", "rule", "value",
        "threshold", "detail"
      )
    ) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      band_south = NA_real_,
      band_north = NA_real_
    )

  # Every rule has a fixed threshold, so the threshold table is empty
  data_thresholds_none <-
    tibble::tibble(
      product = base::character(0),
      variable = base::character(0),
      rule = base::character(0),
      threshold = base::numeric(0)
    )

  res <-
    resolve_quality_findings(
      data_evaluations = data_evaluations,
      data_thresholds = data_thresholds_none,
      data_allow_list = data_allow_list
    )

  return(res)
}

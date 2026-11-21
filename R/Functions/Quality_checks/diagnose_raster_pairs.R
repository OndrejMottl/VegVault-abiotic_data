#' @title Diagnose the quality rules between pairs of rasters
#' @description
#' Apply the rules between two variables of the same product and age to
#' the cell-by-cell comparisons of `compute_raster_pair_comparison()`: the
#' checked raster may not exceed its reference (for example the
#' precipitation of a quarter its annual total) and may not be missing
#' where the reference has a value.
#' @param data_expected_pairs
#' A data frame with one row per expected pair and the columns `product`,
#' `variable`, `age`, `file_name` (the checked raster) and
#' `file_name_reference`; each pair at most once.
#' @param data_pair_comparison
#' A data frame of `compute_raster_pair_comparison()` results, bound
#' together.
#' @param data_thresholds
#' Threshold table as described in `resolve_quality_findings()`, with rows
#' for the rules `pair_exceeding` and `pair_missing_only` of every checked
#' variable.
#' @param data_allow_list
#' Allow-list as described in `resolve_quality_findings()`.
#' @return
#' A tibble of findings as returned by `resolve_quality_findings()`.
#' @details
#' Rules, with the value compared with the threshold:
#' - `pair_present`: 1 when the comparison of the pair is missing; fixed
#'   threshold 0.
#' - `pair_readable`: 1 when the comparison could not be computed (its
#'   `read_error`); fixed threshold 0.
#' - `pair_exceeding`: per band, the share of cells valid in both rasters
#'   where the checked raster exceeds the reference.
#' - `pair_missing_only`: per band, the share of cells valid in the
#'   reference that are missing in the checked raster.
#' The detail names the reference file.
#' @seealso [compute_raster_pair_comparison()], [resolve_quality_findings()]
#' @examples
#' \dontrun{
#' diagnose_raster_pairs(
#'   data_expected_pairs = data_expected_pairs,
#'   data_pair_comparison = data_pair_comparison,
#'   data_thresholds = data_thresholds,
#'   data_allow_list = data_allow_list
#' )
#' }
diagnose_raster_pairs <- function(
  data_expected_pairs,
  data_pair_comparison,
  data_thresholds,
  data_allow_list
) {
  vec_columns_expected <-
    c("product", "variable", "age", "file_name", "file_name_reference")

  vec_columns_comparison <-
    c(
      "file_name", "file_name_reference", "band_south", "band_north",
      "share_exceeding", "share_missing_only", "read_error"
    )

  vec_columns_findings <-
    c(
      "product", "variable", "age", "file_name", "band_south", "band_north",
      "rule", "value", "threshold", "detail"
    )

  assertthat::assert_that(
    base::is.data.frame(data_expected_pairs),
    base::all(vec_columns_expected %in% base::colnames(data_expected_pairs)),
    !base::anyDuplicated(
      dplyr::select(data_expected_pairs, "file_name", "file_name_reference")
    ),
    msg = stringr::str_c(
      "`data_expected_pairs` must be a data frame with the columns ",
      stringr::str_c(vec_columns_expected, collapse = ", "),
      " and each pair at most once."
    )
  )

  assertthat::assert_that(
    base::is.data.frame(data_pair_comparison),
    base::all(
      vec_columns_comparison %in% base::colnames(data_pair_comparison)
    ),
    msg = stringr::str_c(
      "`data_pair_comparison` must be a data frame with the columns ",
      stringr::str_c(vec_columns_comparison, collapse = ", "),
      "."
    )
  )

  # One row per compared pair: present, and the error of an unreadable one
  data_pair_status <-
    data_pair_comparison |>
    dplyr::group_by(
      .data[["file_name"]],
      .data[["file_name_reference"]]
    ) |>
    dplyr::summarise(
      read_error = dplyr::first(
        stats::na.omit(.data[["read_error"]]),
        default = NA_character_
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(flag_compared = TRUE)

  data_pairs <-
    data_expected_pairs |>
    dplyr::select(dplyr::all_of(vec_columns_expected)) |>
    dplyr::left_join(
      data_pair_status,
      by = dplyr::join_by("file_name", "file_name_reference"),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      flag_compared = dplyr::coalesce(.data[["flag_compared"]], FALSE),
      text_reference = stringr::str_c(
        "Reference: ",
        .data[["file_name_reference"]],
        "."
      )
    )

  data_present <-
    data_pairs |>
    dplyr::mutate(
      band_south = NA_real_,
      band_north = NA_real_,
      rule = "pair_present",
      value = base::as.numeric(!.data[["flag_compared"]]),
      threshold = 0,
      detail = dplyr::if_else(
        .data[["flag_compared"]],
        .data[["text_reference"]],
        stringr::str_c(
          "The comparison is missing. ",
          .data[["text_reference"]]
        )
      )
    )

  data_readable <-
    data_pairs |>
    dplyr::filter(.data[["flag_compared"]]) |>
    dplyr::mutate(
      band_south = NA_real_,
      band_north = NA_real_,
      rule = "pair_readable",
      value = base::as.numeric(!base::is.na(.data[["read_error"]])),
      threshold = 0,
      detail = stringr::str_squish(
        stringr::str_c(
          dplyr::coalesce(.data[["read_error"]], ""),
          " ",
          .data[["text_reference"]]
        )
      )
    )

  data_bands <-
    data_pairs |>
    dplyr::filter(
      .data[["flag_compared"]],
      base::is.na(.data[["read_error"]])
    ) |>
    dplyr::select(dplyr::all_of(vec_columns_expected), "text_reference") |>
    dplyr::inner_join(
      data_pair_comparison |>
        dplyr::filter(!base::is.na(.data[["band_south"]])) |>
        dplyr::select(
          "file_name", "file_name_reference", "band_south", "band_north",
          "share_exceeding", "share_missing_only"
        ),
      by = dplyr::join_by("file_name", "file_name_reference"),
      relationship = "one-to-many"
    )

  data_exceeding <-
    data_bands |>
    dplyr::filter(!base::is.na(.data[["share_exceeding"]])) |>
    dplyr::mutate(
      rule = "pair_exceeding",
      value = .data[["share_exceeding"]],
      threshold = NA_real_,
      detail = stringr::str_c(
        "Share of cells above the reference: ",
        base::signif(.data[["share_exceeding"]], 4),
        ". ",
        .data[["text_reference"]]
      )
    )

  data_missing_only <-
    data_bands |>
    dplyr::filter(!base::is.na(.data[["share_missing_only"]])) |>
    dplyr::mutate(
      rule = "pair_missing_only",
      value = .data[["share_missing_only"]],
      threshold = NA_real_,
      detail = stringr::str_c(
        "Share of reference cells missing in this raster: ",
        base::signif(.data[["share_missing_only"]], 4),
        ". ",
        .data[["text_reference"]]
      )
    )

  data_evaluations <-
    list(data_present, data_readable, data_exceeding, data_missing_only) |>
    purrr::map(
      .f = ~ dplyr::select(.x, dplyr::all_of(vec_columns_findings))
    ) |>
    purrr::list_rbind()

  res <-
    resolve_quality_findings(
      data_evaluations = data_evaluations,
      data_thresholds = data_thresholds,
      data_allow_list = data_allow_list
    )

  return(res)
}

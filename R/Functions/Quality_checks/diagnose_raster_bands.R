#' @title Diagnose the band-level quality rules of a set of rasters
#' @description
#' Apply the value rules of the raster quality check to the latitude-band
#' statistics of a set of rasters: storage-type limit values, the share of
#' cells exactly zero (its level, its step between adjacent bands, and its
#' deviation from the other ages of the same variable), and bands with no
#' value where another variable of the same product and age has values.
#' @param data_expected_files
#' A data frame with the columns `product`, `variable`, `age` and
#' `file_name` (unique), as described in `diagnose_raster_files()`.
#' @param data_band_summary
#' A data frame of `compute_raster_band_summary()` results for the files
#' of the set, bound together. Rows of unreadable files (band columns
#' `NA`) are ignored here; `diagnose_raster_files()` reports them.
#' @param data_thresholds
#' Threshold table as described in `resolve_quality_findings()`, with rows
#' for the rules `zero_share_max`, `zero_share_step` and, for variables
#' with more than one age, `zero_share_age_deviation`.
#' @param data_allow_list
#' Allow-list as described in `resolve_quality_findings()`.
#' @param vec_type_limit_values
#' Numeric vector of storage-type limit values, from
#' `build_raster_type_limit_values()`.
#' @param value_tolerance
#' Positive number. Relative tolerance for matching the file's maximum or
#' minimum to a type-limit value. Default `1e-6`.
#' @return
#' A tibble of findings as returned by `resolve_quality_findings()`, one
#' row per file, band and rule.
#' @details
#' Rules, with the value compared with the threshold:
#' - `type_limit`: when the largest (or smallest) value of the file equals
#'   a storage-type limit, the number of cells of the band at that value;
#'   0 otherwise. Fixed threshold 0, so a capped file fails in every band
#'   that holds capped cells, unless allowed. A limit value below the
#'   file's maximum is an ordinary value (25.5 is both the 8-bit maximum
#'   at scale 0.1 and a common temperature), so it does not count.
#' - `zero_share_max`: the share of non-missing cells exactly zero.
#' - `zero_share_step`: the absolute change of that share from the band to
#'   the north; reported on the band where the share jumps, so a cut such
#'   as zeros south of one latitude fails even where the share alone would
#'   pass.
#' - `zero_share_age_deviation`: the share minus the median share of the
#'   other ages of the same product and variable in the same band;
#'   evaluated only when the variable has more than one age.
#' - `band_all_missing`: 1 when every cell of the band is missing while
#'   another variable of the same product and age has values there, else
#'   0; fixed threshold 0.
#' Rules without a defined value (for example the zero share of a band
#' with no valid cell) are not evaluated for that band. Thresholds missing
#' from `data_thresholds` fail, as `resolve_quality_findings()` describes.
#' @seealso [compute_raster_band_summary()], [resolve_quality_findings()]
#' @examples
#' \dontrun{
#' diagnose_raster_bands(
#'   data_expected_files = data_expected_files,
#'   data_band_summary = data_band_summary,
#'   data_thresholds = data_thresholds,
#'   data_allow_list = data_allow_list,
#'   vec_type_limit_values = build_raster_type_limit_values(
#'     vec_scales = 0.1,
#'     vec_offsets = c(0, -273.15),
#'     vec_integer_types = "INT2U"
#'   )
#' )
#' }
diagnose_raster_bands <- function(
  data_expected_files,
  data_band_summary,
  data_thresholds,
  data_allow_list,
  vec_type_limit_values,
  value_tolerance = 1e-6
) {
  vec_columns_expected <-
    c("product", "variable", "age", "file_name")

  vec_columns_band <-
    c(
      "file_name", "band_south", "band_north", "n_cells", "n_missing",
      "n_at_file_max", "n_at_file_min", "value_min", "value_max",
      "share_zero", "read_error"
    )

  vec_columns_findings <-
    c(
      "product", "variable", "age", "file_name", "band_south", "band_north",
      "rule", "value", "threshold", "detail"
    )

  assertthat::assert_that(
    base::is.data.frame(data_expected_files),
    base::all(vec_columns_expected %in% base::colnames(data_expected_files)),
    !base::anyDuplicated(dplyr::pull(data_expected_files, "file_name")),
    msg = stringr::str_c(
      "`data_expected_files` must be a data frame with unique `file_name` ",
      "and the columns product, variable, age and file_name."
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
    base::is.numeric(vec_type_limit_values),
    base::all(base::is.finite(vec_type_limit_values)),
    msg = "`vec_type_limit_values` must be a vector of finite numbers."
  )

  assertthat::assert_that(
    base::is.numeric(value_tolerance),
    base::length(value_tolerance) == 1L,
    base::is.finite(value_tolerance),
    value_tolerance > 0,
    msg = "`value_tolerance` must be a positive number."
  )

  # Unreadable files have no band rows; diagnose_raster_files() reports them
  data_bands <-
    data_band_summary |>
    dplyr::filter(
      base::is.na(.data[["read_error"]]),
      !base::is.na(.data[["band_south"]])
    ) |>
    dplyr::select(dplyr::all_of(vec_columns_band)) |>
    dplyr::inner_join(
      dplyr::select(data_expected_files, dplyr::all_of(vec_columns_expected)),
      by = dplyr::join_by("file_name"),
      relationship = "many-to-one"
    )

  # A cap piles cells up at the file's extreme, so a limit value counts
  #   only when it is the largest or smallest value of the file
  data_file_extremes <-
    data_bands |>
    dplyr::group_by(.data[["file_name"]]) |>
    dplyr::summarise(
      file_max = base::max(c(-Inf, .data[["value_max"]]), na.rm = TRUE),
      file_min = base::min(c(Inf, .data[["value_min"]]), na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      limit_at_max = purrr::map_dbl(
        .x = .data[["file_max"]],
        .f = ~ vec_type_limit_values[
          base::abs(vec_type_limit_values - .x) <=
            value_tolerance * base::abs(vec_type_limit_values)
        ][1]
      ),
      limit_at_min = purrr::map_dbl(
        .x = .data[["file_min"]],
        .f = ~ vec_type_limit_values[
          base::abs(vec_type_limit_values - .x) <=
            value_tolerance * base::abs(vec_type_limit_values)
        ][1]
      )
    )

  data_type_limit <-
    data_bands |>
    dplyr::left_join(
      data_file_extremes,
      by = dplyr::join_by("file_name"),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      n_capped_max = dplyr::if_else(
        base::is.na(.data[["limit_at_max"]]),
        0,
        .data[["n_at_file_max"]]
      ),
      n_capped_min = dplyr::if_else(
        base::is.na(.data[["limit_at_min"]]),
        0,
        .data[["n_at_file_min"]]
      ),
      rule = "type_limit",
      value = .data[["n_capped_max"]] + .data[["n_capped_min"]],
      threshold = 0,
      detail = stringr::str_squish(
        stringr::str_c(
          dplyr::if_else(
            .data[["n_capped_max"]] > 0,
            stringr::str_c(
              .data[["n_capped_max"]],
              " cells at the file maximum ",
              .data[["limit_at_max"]],
              ", a storage-type limit."
            ),
            ""
          ),
          " ",
          dplyr::if_else(
            .data[["n_capped_min"]] > 0,
            stringr::str_c(
              .data[["n_capped_min"]],
              " cells at the file minimum ",
              .data[["limit_at_min"]],
              ", a storage-type limit."
            ),
            ""
          )
        )
      )
    )

  data_zero_share <-
    data_bands |>
    dplyr::filter(!base::is.na(.data[["share_zero"]])) |>
    dplyr::mutate(
      rule = "zero_share_max",
      value = .data[["share_zero"]],
      threshold = NA_real_,
      detail = stringr::str_c(
        "Share of cells exactly zero: ",
        base::round(.data[["share_zero"]], 4),
        "."
      )
    )

  # The step is reported on the band where the share jumps, coming from
  #   the north
  data_zero_step <-
    data_bands |>
    dplyr::group_by(.data[["file_name"]]) |>
    dplyr::arrange(
      dplyr::desc(.data[["band_north"]]),
      .by_group = TRUE
    ) |>
    dplyr::mutate(share_north = dplyr::lag(.data[["share_zero"]])) |>
    dplyr::ungroup() |>
    dplyr::filter(
      !base::is.na(.data[["share_zero"]]),
      !base::is.na(.data[["share_north"]])
    ) |>
    dplyr::mutate(
      rule = "zero_share_step",
      value = base::abs(.data[["share_zero"]] - .data[["share_north"]]),
      threshold = NA_real_,
      detail = stringr::str_c(
        "Share of cells exactly zero changes from ",
        base::round(.data[["share_north"]], 4),
        " in the band to the north to ",
        base::round(.data[["share_zero"]], 4),
        "."
      )
    )

  # Each age against the median of the other ages of the same variable
  data_zero_age <-
    data_bands |>
    dplyr::filter(!base::is.na(.data[["share_zero"]])) |>
    dplyr::group_by(.data[["product"]], .data[["variable"]]) |>
    dplyr::filter(dplyr::n_distinct(.data[["age"]]) > 1) |>
    dplyr::group_by(
      .data[["product"]],
      .data[["variable"]],
      .data[["band_south"]],
      .data[["band_north"]]
    ) |>
    dplyr::mutate(
      share_other_ages = purrr::map_dbl(
        .x = base::seq_along(.data[["share_zero"]]),
        .f = ~ stats::median(.data[["share_zero"]][-.x], na.rm = TRUE)
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(!base::is.na(.data[["share_other_ages"]])) |>
    dplyr::mutate(
      rule = "zero_share_age_deviation",
      value = .data[["share_zero"]] - .data[["share_other_ages"]],
      threshold = NA_real_,
      detail = stringr::str_c(
        "Share of cells exactly zero is ",
        base::round(.data[["share_zero"]], 4),
        "; the median of the other ages is ",
        base::round(.data[["share_other_ages"]], 4),
        "."
      )
    )

  # Variables of the same product and age with a valid cell in each band
  data_variables_with_data <-
    data_bands |>
    dplyr::filter(.data[["n_missing"]] < .data[["n_cells"]]) |>
    dplyr::group_by(
      .data[["product"]],
      .data[["age"]],
      .data[["band_south"]],
      .data[["band_north"]]
    ) |>
    dplyr::summarise(
      list_variables_with_data = list(base::unique(.data[["variable"]])),
      .groups = "drop"
    )

  data_all_missing <-
    data_bands |>
    dplyr::left_join(
      data_variables_with_data,
      by = dplyr::join_by("product", "age", "band_south", "band_north"),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      others_with_data = purrr::map2_chr(
        .x = .data[["list_variables_with_data"]],
        .y = .data[["variable"]],
        .f = ~ stringr::str_c(
          base::sort(base::setdiff(.x, .y)),
          collapse = ", "
        )
      ),
      flag_all_missing = .data[["n_missing"]] == .data[["n_cells"]],
      rule = "band_all_missing",
      value = base::as.numeric(
        .data[["flag_all_missing"]] & base::nzchar(.data[["others_with_data"]])
      ),
      threshold = 0,
      detail = dplyr::if_else(
        .data[["value"]] > 0,
        stringr::str_c(
          "No valid cell in this band, while ",
          .data[["others_with_data"]],
          " have values."
        ),
        ""
      )
    )

  data_evaluations <-
    list(
      data_type_limit,
      data_zero_share,
      data_zero_step,
      data_zero_age,
      data_all_missing
    ) |>
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

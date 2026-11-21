#' @title Resolve raster quality evaluations into findings
#' @description
#' Give every evaluated quality rule a status: compare its value with its
#' threshold, fill thresholds from the threshold table, fail closed when a
#' threshold or a value is missing, and mark failures that an allow-list
#' entry accepts, with its reason.
#' @param data_evaluations
#' A data frame with one row per rule evaluation and the columns `product`,
#' `variable` (character), `age` (numeric), `file_name` (character),
#' `band_south`, `band_north` (numeric; `NA` for a rule about the whole
#' file), `rule` (character), `value` (numeric), `threshold` (numeric: the
#' fixed threshold of the rule, or `NA` when it comes from
#' `data_thresholds`) and `detail` (character).
#' @param data_thresholds
#' A data frame with the columns `product`, `variable`, `rule` and
#' `threshold` (finite numeric), at most one row per product, variable and
#' rule.
#' @param data_allow_list
#' A data frame of accepted exceptions with the columns `product`,
#' `variable`, `rule`, `reason` (non-empty character), `lat_min` and
#' `lat_max` (numeric; `NA` for no limit). May have zero rows.
#' @return
#' A tibble with the columns of `data_evaluations` plus `status`: `"pass"`,
#' `"fail"` or `"allowed"`.
#' @details
#' A rule fails when its value is larger than its threshold. A missing
#' value fails, because the rule could not be evaluated. A missing
#' threshold fails once per file and rule (band columns `NA`) and cannot be
#' allowed, because it is a configuration error. A failure is `"allowed"`
#' when an allow-list entry has the same product, variable and rule and,
#' if the entry has latitude limits, the band lies within them; entries
#' with limits never match a finding about the whole file. The reason is
#' added to `detail`.
#' @seealso [diagnose_raster_files()], [diagnose_raster_bands()],
#'   [diagnose_raster_pairs()]
#' @examples
#' \dontrun{
#' resolve_quality_findings(
#'   data_evaluations = data_evaluations,
#'   data_thresholds = data_thresholds,
#'   data_allow_list = data_allow_list
#' )
#' }
resolve_quality_findings <- function(
  data_evaluations,
  data_thresholds,
  data_allow_list
) {
  vec_columns_evaluations <-
    c(
      "product", "variable", "age", "file_name", "band_south", "band_north",
      "rule", "value", "threshold", "detail"
    )

  vec_columns_key <-
    c("product", "variable", "rule")

  assertthat::assert_that(
    base::is.data.frame(data_evaluations),
    base::all(vec_columns_evaluations %in% base::colnames(data_evaluations)),
    msg = stringr::str_c(
      "`data_evaluations` must be a data frame with the columns ",
      stringr::str_c(vec_columns_evaluations, collapse = ", "),
      "."
    )
  )

  assertthat::assert_that(
    base::is.data.frame(data_thresholds),
    base::all(
      c(vec_columns_key, "threshold") %in% base::colnames(data_thresholds)
    ),
    msg = stringr::str_c(
      "`data_thresholds` must be a data frame with the columns product, ",
      "variable, rule and threshold."
    )
  )

  assertthat::assert_that(
    base::is.numeric(dplyr::pull(data_thresholds, "threshold")),
    base::all(base::is.finite(dplyr::pull(data_thresholds, "threshold"))),
    !base::anyDuplicated(
      dplyr::select(data_thresholds, dplyr::all_of(vec_columns_key))
    ),
    msg = stringr::str_c(
      "`data_thresholds` must hold one finite threshold per product, ",
      "variable and rule."
    )
  )

  assertthat::assert_that(
    base::is.data.frame(data_allow_list),
    base::all(
      c(vec_columns_key, "reason", "lat_min", "lat_max") %in%
        base::colnames(data_allow_list)
    ),
    msg = stringr::str_c(
      "`data_allow_list` must be a data frame with the columns product, ",
      "variable, rule, reason, lat_min and lat_max."
    )
  )

  vec_reasons <-
    dplyr::pull(data_allow_list, "reason")

  assertthat::assert_that(
    base::is.character(vec_reasons),
    !base::anyNA(vec_reasons),
    base::all(base::nzchar(stringr::str_trim(vec_reasons))),
    msg = "Every allow-list entry needs a non-empty `reason`."
  )

  data_thresholds_selected <-
    data_thresholds |>
    dplyr::select(
      dplyr::all_of(vec_columns_key),
      threshold_table = "threshold"
    )

  data_evaluations_thresholded <-
    data_evaluations |>
    dplyr::select(dplyr::all_of(vec_columns_evaluations)) |>
    dplyr::mutate(index_row = dplyr::row_number()) |>
    dplyr::left_join(
      data_thresholds_selected,
      by = dplyr::join_by("product", "variable", "rule"),
      relationship = "many-to-one"
    ) |>
    dplyr::mutate(
      threshold = dplyr::coalesce(
        .data[["threshold"]],
        .data[["threshold_table"]]
      )
    ) |>
    dplyr::select(-"threshold_table")

  # A missing threshold is a configuration error: one failing finding per
  #   file and rule, which no allow-list entry can accept
  data_no_threshold <-
    data_evaluations_thresholded |>
    dplyr::filter(base::is.na(.data[["threshold"]])) |>
    dplyr::group_by(
      .data[["product"]],
      .data[["variable"]],
      .data[["age"]],
      .data[["file_name"]],
      .data[["rule"]]
    ) |>
    # Rows keep their input order, so the first is the earliest
    dplyr::summarise(
      index_row = dplyr::first(.data[["index_row"]]),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      band_south = NA_real_,
      band_north = NA_real_,
      value = NA_real_,
      threshold = NA_real_,
      detail = stringr::str_glue(
        "No threshold is set for rule {rule} and variable {variable}.",
        rule = .data[["rule"]],
        variable = .data[["variable"]]
      ) |>
        base::as.character(),
      status = "fail"
    )

  data_judged <-
    data_evaluations_thresholded |>
    dplyr::filter(!base::is.na(.data[["threshold"]])) |>
    dplyr::mutate(
      status = dplyr::case_when(
        base::is.na(.data[["value"]]) ~ "fail",
        .data[["value"]] > .data[["threshold"]] ~ "fail",
        .default = "pass"
      ),
      detail = dplyr::if_else(
        base::is.na(.data[["value"]]),
        stringr::str_squish(
          stringr::str_c(
            dplyr::coalesce(.data[["detail"]], ""),
            " The rule could not be evaluated."
          )
        ),
        .data[["detail"]]
      )
    )

  # Allow-list entries without latitude limits match any finding of their
  #   product, variable and rule; entries with limits match only bands
  #   that lie within them
  data_allowed_reasons <-
    data_judged |>
    dplyr::filter(.data[["status"]] == "fail") |>
    dplyr::select(
      "index_row", "product", "variable", "rule", "band_south", "band_north"
    ) |>
    dplyr::inner_join(
      dplyr::select(
        data_allow_list,
        dplyr::all_of(vec_columns_key),
        "reason",
        "lat_min",
        "lat_max"
      ),
      by = dplyr::join_by("product", "variable", "rule"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      flag_unlimited = base::is.na(.data[["lat_min"]]) &
        base::is.na(.data[["lat_max"]]),
      flag_within = !base::is.na(.data[["band_south"]]) &
        (base::is.na(.data[["lat_min"]]) |
          .data[["band_south"]] >= .data[["lat_min"]]) &
        (base::is.na(.data[["lat_max"]]) |
          .data[["band_north"]] <= .data[["lat_max"]])
    ) |>
    dplyr::filter(.data[["flag_unlimited"]] | .data[["flag_within"]]) |>
    dplyr::group_by(.data[["index_row"]]) |>
    dplyr::summarise(
      reason_allowed = stringr::str_c(
        base::unique(.data[["reason"]]),
        collapse = " "
      ),
      .groups = "drop"
    )

  data_judged_allowed <-
    data_judged |>
    dplyr::left_join(
      data_allowed_reasons,
      by = dplyr::join_by("index_row"),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      status = dplyr::if_else(
        base::is.na(.data[["reason_allowed"]]),
        .data[["status"]],
        "allowed"
      ),
      detail = dplyr::if_else(
        base::is.na(.data[["reason_allowed"]]),
        .data[["detail"]],
        stringr::str_squish(
          stringr::str_c(
            dplyr::coalesce(.data[["detail"]], ""),
            " Allowed: ",
            .data[["reason_allowed"]]
          )
        )
      )
    )

  res <-
    dplyr::bind_rows(data_judged_allowed, data_no_threshold) |>
    dplyr::arrange(.data[["index_row"]]) |>
    dplyr::select(dplyr::all_of(vec_columns_evaluations), "status") |>
    tibble::as_tibble()

  return(res)
}

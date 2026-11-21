#' @title Prepare the allow-list of the raster quality check
#' @description
#' Turn the allow-list entries of the configuration into the allow-list
#' table that `resolve_quality_findings()` reads, with one row per
#' product, variable and rule.
#' @param list_allow_list
#' An unnamed list of entries (may be empty). Each entry is a list with
#' `product` (one string), `variables` and `rules` (character vectors with
#' at least one value each), `reason` (one non-empty string) and,
#' optionally, `lat_min` and `lat_max` (one number each, or `NULL` for no
#' limit).
#' @return
#' A tibble with the columns `product`, `variable`, `rule`, `reason`
#' (character), `lat_min` and `lat_max` (numeric; `NA` for no limit), one
#' row per entry, variable and rule. An empty list gives zero rows.
#' @details
#' An entry lists several variables and rules to keep the configuration
#' short; every combination of them is allowed with the same reason and
#' latitude limits. Aborts when a required field is missing or empty, when
#' a latitude limit is not one finite number, or when `lat_min` is not
#' below `lat_max`.
#' @seealso [resolve_quality_findings()],
#'   [prepare_raster_quality_thresholds()]
#' @examples
#' prepare_raster_quality_allow_list(
#'   list_allow_list = list(
#'     list(
#'       product = "CHELSA_TraCE21k_V1.0",
#'       variables = c("bio12", "bio18"),
#'       rules = "zero_share_max",
#'       reason = "No precipitation is downscaled in Antarctica.",
#'       lat_min = -91,
#'       lat_max = -74
#'     )
#'   )
#' )
prepare_raster_quality_allow_list <- function(list_allow_list) {
  assertthat::assert_that(
    base::is.list(list_allow_list),
    base::all(purrr::map_lgl(.x = list_allow_list, .f = base::is.list)),
    msg = "`list_allow_list` must be a list of entries, each a list."
  )

  data_template <-
    tibble::tibble(
      product = base::character(0),
      variable = base::character(0),
      rule = base::character(0),
      reason = base::character(0),
      lat_min = base::numeric(0),
      lat_max = base::numeric(0)
    )

  if (
    base::length(list_allow_list) == 0L
  ) {
    return(data_template)
  }

  # One row per entry, with the raw field values as list columns
  data_entries_raw <-
    tibble::tibble(
      product = purrr::map(.x = list_allow_list, .f = ~ .x[["product"]]),
      variable = purrr::map(.x = list_allow_list, .f = ~ .x[["variables"]]),
      rule = purrr::map(.x = list_allow_list, .f = ~ .x[["rules"]]),
      reason = purrr::map(.x = list_allow_list, .f = ~ .x[["reason"]]),
      lat_min = purrr::map(.x = list_allow_list, .f = ~ .x[["lat_min"]]),
      lat_max = purrr::map(.x = list_allow_list, .f = ~ .x[["lat_max"]])
    )

  assertthat::assert_that(
    base::all(
      purrr::map_lgl(
        .x = dplyr::pull(data_entries_raw, "product"),
        .f = ~ rlang::is_string(.x) && !base::is.na(.x) && base::nzchar(.x)
      )
    ),
    msg = "Every allow-list entry needs one non-empty `product`."
  )

  assertthat::assert_that(
    base::all(
      purrr::map_lgl(
        .x = dplyr::pull(data_entries_raw, "reason"),
        .f = ~ rlang::is_string(.x) && !base::is.na(.x) && base::nzchar(.x)
      )
    ),
    msg = "Every allow-list entry needs one non-empty `reason`."
  )

  assertthat::assert_that(
    base::all(
      purrr::map_lgl(
        .x = dplyr::pull(data_entries_raw, "variable"),
        .f = ~ base::is.character(.x) &&
          base::length(.x) > 0L &&
          base::all(!base::is.na(.x) & base::nzchar(.x))
      )
    ),
    msg = "Every allow-list entry needs `variables` as non-empty text."
  )

  assertthat::assert_that(
    base::all(
      purrr::map_lgl(
        .x = dplyr::pull(data_entries_raw, "rule"),
        .f = ~ base::is.character(.x) &&
          base::length(.x) > 0L &&
          base::all(!base::is.na(.x) & base::nzchar(.x))
      )
    ),
    msg = "Every allow-list entry needs `rules` as non-empty text."
  )

  assertthat::assert_that(
    base::all(
      purrr::map_lgl(
        .x = dplyr::pull(data_entries_raw, "lat_min"),
        .f = ~ base::is.null(.x) ||
          (
            base::is.numeric(.x) &&
              base::length(.x) == 1L &&
              base::is.finite(.x)
          )
      )
    ),
    msg = "`lat_min` of an allow-list entry must be one finite number or NULL."
  )

  assertthat::assert_that(
    base::all(
      purrr::map_lgl(
        .x = dplyr::pull(data_entries_raw, "lat_max"),
        .f = ~ base::is.null(.x) ||
          (
            base::is.numeric(.x) &&
              base::length(.x) == 1L &&
              base::is.finite(.x)
          )
      )
    ),
    msg = "`lat_max` of an allow-list entry must be one finite number or NULL."
  )

  data_entries <-
    data_entries_raw |>
    dplyr::mutate(
      product = purrr::map_chr(.x = .data[["product"]], .f = ~.x),
      reason = purrr::map_chr(.x = .data[["reason"]], .f = ~.x),
      lat_min = purrr::map_dbl(
        .x = .data[["lat_min"]],
        .f = ~ base::as.numeric(.x %||% NA_real_)
      ),
      lat_max = purrr::map_dbl(
        .x = .data[["lat_max"]],
        .f = ~ base::as.numeric(.x %||% NA_real_)
      )
    )

  assertthat::assert_that(
    base::all(
      base::is.na(dplyr::pull(data_entries, "lat_min")) |
        base::is.na(dplyr::pull(data_entries, "lat_max")) |
        dplyr::pull(data_entries, "lat_min") <
          dplyr::pull(data_entries, "lat_max")
    ),
    msg = "`lat_min` of an allow-list entry must be below its `lat_max`."
  )

  res <-
    data_entries |>
    tidyr::unnest_longer(col = "variable") |>
    tidyr::unnest_longer(col = "rule") |>
    dplyr::select(dplyr::all_of(base::colnames(data_template)))

  return(res)
}

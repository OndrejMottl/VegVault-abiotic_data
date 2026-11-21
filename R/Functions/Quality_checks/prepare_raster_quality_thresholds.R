#' @title Prepare the threshold table of the raster quality check
#' @description
#' Turn the nested threshold list of the configuration (product, then
#' rule, then variable) into the threshold table that
#' `resolve_quality_findings()` reads.
#' @param list_thresholds
#' A named list with one element per product; each is a named list with
#' one element per rule; each of those is a named list (or named numeric
#' vector) of thresholds by variable, for example
#' `list(CHELSA_V2.1 = list(zero_share_max = list(bio1 = 0.05)))`.
#' @return
#' A tibble with the columns `product`, `variable`, `rule` (character) and
#' `threshold` (numeric), one row per product, rule and variable, sorted by
#' product, rule and variable.
#' @details
#' Aborts when a level is empty or unnamed, when a name is empty or
#' repeated within its level, or when a threshold is not one finite
#' number, so a configuration error never becomes a silent pass.
#' @seealso [resolve_quality_findings()],
#'   [prepare_raster_quality_allow_list()]
#' @examples
#' prepare_raster_quality_thresholds(
#'   list_thresholds = list(
#'     CHELSA_V2.1 = list(
#'       zero_share_max = list(bio1 = 0.05, bio18 = 0.131)
#'     )
#'   )
#' )
prepare_raster_quality_thresholds <- function(list_thresholds) {
  assertthat::assert_that(
    base::is.list(list_thresholds),
    rlang::is_named(list_thresholds),
    msg = "`list_thresholds` must be a named list of products."
  )

  assertthat::assert_that(
    base::all(
      purrr::map_lgl(
        .x = list_thresholds,
        .f = ~ base::is.list(.x) && rlang::is_named(.x)
      )
    ),
    msg = "Every product of `list_thresholds` must be a named list of rules."
  )

  # One element per product and rule: the thresholds by variable
  list_rule_levels <-
    purrr::list_flatten(list_thresholds) |>
    base::unname()

  assertthat::assert_that(
    base::all(
      purrr::map_lgl(
        .x = list_rule_levels,
        .f = ~ (base::is.list(.x) || base::is.numeric(.x)) &&
          rlang::is_named(.x)
      )
    ),
    msg = stringr::str_c(
      "Every rule of `list_thresholds` must be a named list of ",
      "thresholds by variable."
    )
  )

  is_name_repeated <-
    base::anyDuplicated(base::names(list_thresholds)) > 0L ||
    base::any(
      purrr::map_lgl(
        .x = c(list_thresholds, list_rule_levels),
        .f = ~ base::anyDuplicated(base::names(.x)) > 0L
      )
    )

  assertthat::assert_that(
    !is_name_repeated,
    msg = "A product, rule or variable name is repeated in `list_thresholds`."
  )

  list_values <-
    purrr::map(
      .x = list_rule_levels,
      .f = ~ base::as.list(.x)
    ) |>
    purrr::list_flatten()

  assertthat::assert_that(
    base::all(
      purrr::map_lgl(
        .x = list_values,
        .f = ~ base::is.numeric(.x) &&
          base::length(.x) == 1L &&
          base::is.finite(.x)
      )
    ),
    msg = "Every threshold in `list_thresholds` must be one finite number."
  )

  data_rules <-
    tibble::tibble(
      product = base::rep(
        base::names(list_thresholds),
        times = purrr::map_int(.x = list_thresholds, .f = base::length)
      ),
      rule = purrr::map(.x = list_thresholds, .f = base::names) |>
        base::unlist(use.names = FALSE),
      list_variables = list_rule_levels
    )

  res <-
    data_rules |>
    dplyr::mutate(
      variable = purrr::map(
        .x = .data[["list_variables"]],
        .f = ~ base::names(.x)
      ),
      threshold = purrr::map(
        .x = .data[["list_variables"]],
        .f = ~ base::as.numeric(base::unlist(.x, use.names = FALSE))
      )
    ) |>
    tidyr::unnest(cols = c("variable", "threshold")) |>
    dplyr::select("product", "variable", "rule", "threshold") |>
    dplyr::arrange(
      .data[["product"]],
      .data[["rule"]],
      .data[["variable"]]
    )

  return(res)
}

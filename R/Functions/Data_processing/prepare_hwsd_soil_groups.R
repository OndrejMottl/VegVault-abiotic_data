#' @title Prepare the class of every HWSD soil unit
#' @description
#' Give each HWSD soil unit the class of its dominant component: the code
#' of its WRB Reference Soil Group, a code for a class that is not a soil,
#' or the code for a WRB code in no list.
#' @param data_soil_units
#' A data frame as returned by `load_hwsd_soil_units()`.
#' @param list_soil_group_codes
#' A named list or named integer vector: two-letter WRB code of each soil
#' group to its code (whole numbers, unique).
#' @param list_non_soil_codes
#' A named list or named integer vector: code of each class that is not a
#' soil (for example `GG` glaciers, `WR` open water) to its code (whole
#' numbers above the soil-group codes, unique).
#' @param code_unlisted
#' Whole number. Code for a dominant WRB code in neither list (and for a
#' missing one); above all other codes.
#' @return
#' A tibble with one row per soil unit, sorted by `HWSD2_SMU_ID`, and the
#' columns `HWSD2_SMU_ID` (integer), `WRB2` (character, of the dominant
#' component), `class_code` (integer) and `class_type` (`"soil"`,
#' `"non_soil"` or `"unlisted"`).
#' @details
#' The dominant component is the one with `SEQUENCE` 1; HWSD orders the
#' components by share, so it always has the largest share. The mapping
#' goes by the two-letter code, never by HWSD's own numeric code. Aborts
#' when a soil unit has no component 1 or the codes overlap.
#' @seealso [load_hwsd_soil_units()], [save_classified_raster()]
#' @examples
#' prepare_hwsd_soil_groups(
#'   data_soil_units = tibble::tibble(
#'     HWSD2_SMU_ID = c(2L, 2L, 3L),
#'     SEQUENCE = c(1L, 2L, 1L),
#'     SHARE = c(60, 40, 100),
#'     WRB2 = c("AC", "LV", "WR")
#'   ),
#'   list_soil_group_codes = list(AC = 1L, LV = 19L),
#'   list_non_soil_codes = list(WR = 34L),
#'   code_unlisted = 37L
#' )
prepare_hwsd_soil_groups <- function(
  data_soil_units,
  list_soil_group_codes,
  list_non_soil_codes,
  code_unlisted
) {
  vec_columns <-
    c("HWSD2_SMU_ID", "SEQUENCE", "WRB2")

  assertthat::assert_that(
    base::is.data.frame(data_soil_units),
    base::all(vec_columns %in% base::colnames(data_soil_units)),
    msg = stringr::str_c(
      "`data_soil_units` must be a data frame with the columns ",
      stringr::str_c(vec_columns, collapse = ", "),
      "."
    )
  )

  vec_soil_codes <-
    base::unlist(list_soil_group_codes)

  vec_non_soil_codes <-
    base::unlist(list_non_soil_codes)

  assertthat::assert_that(
    rlang::is_named(vec_soil_codes),
    rlang::is_named(vec_non_soil_codes),
    base::is.numeric(vec_soil_codes),
    base::is.numeric(vec_non_soil_codes),
    base::all(vec_soil_codes == base::round(vec_soil_codes)),
    base::all(vec_non_soil_codes == base::round(vec_non_soil_codes)),
    !base::anyDuplicated(c(vec_soil_codes, vec_non_soil_codes)),
    !base::anyDuplicated(
      c(base::names(vec_soil_codes), base::names(vec_non_soil_codes))
    ),
    base::min(vec_non_soil_codes) > base::max(vec_soil_codes),
    msg = stringr::str_c(
      "The soil-group and non-soil codes must be named whole numbers, ",
      "unique, with every non-soil code above the soil-group codes."
    )
  )

  assertthat::assert_that(
    base::is.numeric(code_unlisted),
    base::length(code_unlisted) == 1L,
    code_unlisted == base::round(code_unlisted),
    code_unlisted > base::max(c(vec_soil_codes, vec_non_soil_codes)),
    msg = "`code_unlisted` must be one whole number above all other codes."
  )

  vec_units <-
    base::unique(dplyr::pull(data_soil_units, "HWSD2_SMU_ID"))

  data_dominant <-
    data_soil_units |>
    dplyr::filter(.data[["SEQUENCE"]] == 1L) |>
    dplyr::select("HWSD2_SMU_ID", "WRB2")

  vec_units_without_first <-
    base::setdiff(vec_units, dplyr::pull(data_dominant, "HWSD2_SMU_ID"))

  n_units_without_first <-
    base::length(vec_units_without_first)

  if (
    n_units_without_first > 0L
  ) {
    cli::cli_abort(
      c(
        stringr::str_c(
          "{n_units_without_first} soil unit{?s} ha{?s/ve} no component ",
          "with `SEQUENCE` 1."
        ),
        "x" = "{.val {vec_units_without_first}}"
      )
    )
  }

  res <-
    data_dominant |>
    dplyr::mutate(
      HWSD2_SMU_ID = base::as.integer(.data[["HWSD2_SMU_ID"]]),
      class_type = dplyr::case_when(
        .data[["WRB2"]] %in% base::names(vec_soil_codes) ~ "soil",
        .data[["WRB2"]] %in% base::names(vec_non_soil_codes) ~ "non_soil",
        .default = "unlisted"
      ),
      class_code = base::as.integer(
        dplyr::case_when(
          .data[["class_type"]] == "soil" ~
            vec_soil_codes[.data[["WRB2"]]],
          .data[["class_type"]] == "non_soil" ~
            vec_non_soil_codes[.data[["WRB2"]]],
          .default = code_unlisted
        )
      )
    ) |>
    dplyr::select("HWSD2_SMU_ID", "WRB2", "class_code", "class_type") |>
    dplyr::arrange(.data[["HWSD2_SMU_ID"]])

  return(res)
}

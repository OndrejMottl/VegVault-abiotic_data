#' @title Save the legend of the HWSD soil-group codes
#' @description
#' Write the codes of the soil groups with their WRB code and name, the
#' legend that the VegVault contract `hwsd_soil` asks the release to
#' publish next to the soil artefact.
#' @param list_soil_group_codes
#' Named list of whole numbers: the code of each soil group, named by its
#' two-letter WRB code (`soil_group_codes` in `config.yml`).
#' @param path_wrb_lookup
#' Character scalar. The HWSD v2.2 lookup table `d_wrb2.csv` with the
#' columns `CODE` (the two-letter WRB code) and `Value` (the name).
#' @param path_output
#' Character scalar. The CSV file to write; its folder is created when
#' missing.
#' @param list_names_missing
#' Named list of names, by WRB code, for soil groups that the lookup table
#' lacks (HWSD v2.2 has no row for `DU`, Durisols). Default: an empty
#' list.
#' @return
#' Character scalar. `path_output`.
#' @details
#' The columns are `value` (the code as stored in the soil artefact),
#' `wrb_code` and `soil_group`, one row per soil group, sorted by code.
#' The names come from the lookup table; `list_names_missing` only fills
#' WRB codes that the lookup table does not list, so it never replaces an
#' HWSD name. Aborts when a code is not a whole number or is repeated, when
#' a WRB code has more than one name in the lookup table or no name at
#' all, or when `list_names_missing` names a WRB code that the lookup table
#' already lists.
#' @seealso [prepare_hwsd_soil_groups()]
#' @examples
#' \dontrun{
#' save_hwsd_soil_legend(
#'   list_soil_group_codes = load_config_value(
#'     value = c("data_processing", "hwsd", "soil_group_codes")
#'   ),
#'   path_wrb_lookup = path_wrb_lookup,
#'   path_output = here::here("Outputs", "Tables", "hwsd_legend.csv")
#' )
#' }
save_hwsd_soil_legend <- function(
  list_soil_group_codes,
  path_wrb_lookup,
  path_output,
  list_names_missing = base::list()
) {
  assertthat::assert_that(
    base::is.list(list_soil_group_codes),
    base::length(list_soil_group_codes) > 0L,
    rlang::is_named(list_soil_group_codes),
    !base::anyDuplicated(base::names(list_soil_group_codes)),
    msg = stringr::str_c(
      "`list_soil_group_codes` must be a list of codes named by their",
      " WRB code."
    )
  )

  assertthat::assert_that(
    rlang::is_string(path_wrb_lookup) && base::file.exists(path_wrb_lookup),
    msg = "`path_wrb_lookup` must be the path of an existing file."
  )

  assertthat::assert_that(
    rlang::is_string(path_output) && !base::is.na(path_output),
    msg = "`path_output` must be one file path."
  )

  assertthat::assert_that(
    base::is.list(list_names_missing),
    base::length(list_names_missing) == 0L ||
      (
        rlang::is_named(list_names_missing) &&
          base::all(
            purrr::map_lgl(
              .x = list_names_missing,
              .f = ~ rlang::is_string(.x) && !base::is.na(.x) &&
                base::nzchar(.x)
            )
          )
      ),
    msg = "`list_names_missing` must be a list of names named by WRB code."
  )

  vec_codes <-
    purrr::map_dbl(
      .x = list_soil_group_codes,
      .f = ~ if (
        rlang::is_scalar_integerish(.x, finite = TRUE)
      ) {
        base::as.double(.x)
      } else {
        NA_real_
      }
    )

  vec_wrb_not_whole <-
    base::names(vec_codes)[base::is.na(vec_codes)]

  if (
    base::length(vec_wrb_not_whole) > 0L
  ) {
    cli::cli_abort(
      c(
        "Every soil-group code must be one whole number.",
        "x" = "Not a whole number: {.val {vec_wrb_not_whole}}"
      )
    )
  }

  vec_codes_repeated <-
    base::unique(vec_codes[base::duplicated(vec_codes)])

  if (
    base::length(vec_codes_repeated) > 0L
  ) {
    cli::cli_abort(
      c(
        "Every soil group needs its own code.",
        "x" = "Codes repeated: {.val {vec_codes_repeated}}"
      )
    )
  }

  data_lookup <-
    readr::read_csv(
      file = path_wrb_lookup,
      col_types = readr::cols(.default = readr::col_character())
    )

  if (
    !base::all(c("CODE", "Value") %in% base::names(data_lookup))
  ) {
    cli::cli_abort(
      "{.file {path_wrb_lookup}} must have the columns `CODE` and `Value`."
    )
  }

  # The configured names only fill gaps; an HWSD name is never replaced
  vec_codes_replaced <-
    base::intersect(
      base::names(list_names_missing),
      dplyr::pull(data_lookup, "CODE")
    )

  if (
    base::length(vec_codes_replaced) > 0L
  ) {
    cli::cli_abort(
      c(
        "`list_names_missing` names WRB codes that {.file {path_wrb_lookup}}
        already lists; HWSD names are never replaced.",
        "x" = "{.val {vec_codes_replaced}}"
      )
    )
  }

  data_names <-
    dplyr::bind_rows(
      dplyr::select(data_lookup, "CODE", "Value"),
      tibble::tibble(
        CODE = base::as.character(base::names(list_names_missing)),
        Value = base::as.character(base::unlist(list_names_missing))
      )
    )

  data_codes <-
    tibble::tibble(
      value = base::unname(vec_codes),
      wrb_code = base::names(vec_codes)
    )

  # Each WRB code needs exactly one name
  data_name_counts <-
    data_names |>
    dplyr::count(.data[["CODE"]], name = "n_names")

  vec_codes_unnamed <-
    data_codes |>
    dplyr::left_join(
      data_name_counts,
      by = dplyr::join_by("wrb_code" == "CODE")
    ) |>
    dplyr::filter(
      base::is.na(.data[["n_names"]]) | .data[["n_names"]] != 1L
    ) |>
    dplyr::pull("wrb_code")

  if (
    base::length(vec_codes_unnamed) > 0L
  ) {
    cli::cli_abort(
      c(
        "WRB codes with no name, or more than one, in
        {.file {path_wrb_lookup}} and `list_names_missing`:",
        "x" = "{.val {vec_codes_unnamed}}",
        "i" = "Name a soil group that the lookup lacks in
        `soil_group_names_missing` of `data_export` in {.file config.yml}."
      )
    )
  }

  res_legend <-
    data_codes |>
    dplyr::inner_join(
      data_names,
      by = dplyr::join_by("wrb_code" == "CODE"),
      multiple = "error",
      unmatched = "drop"
    ) |>
    dplyr::select("value", "wrb_code", soil_group = "Value") |>
    dplyr::arrange(.data[["value"]])

  base::dir.create(
    path = base::dirname(path_output),
    recursive = TRUE,
    showWarnings = FALSE
  )

  readr::write_csv(x = res_legend, file = path_output, na = "")

  res <- path_output

  return(res)
}

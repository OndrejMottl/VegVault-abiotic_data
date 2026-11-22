#' @title Load the HWSD soil-unit table
#' @description
#' Read the tracked table of HWSD v2.2 soil-unit components (written by
#' `save_hwsd_class_tables()`).
#' @param vec_paths
#' Character vector of the class-table paths; the one named
#' `hwsd2_smu.csv` is read.
#' @return
#' A tibble with the columns `HWSD2_SMU_ID` (integer), `SEQUENCE`
#' (integer), `SHARE` (numeric) and `WRB2` (character), one row per soil
#' unit and component.
#' @details
#' Aborts when `hwsd2_smu.csv` is not among `vec_paths`, when a column is
#' missing, or when a soil unit and component appear twice.
#' @seealso [save_hwsd_class_tables()], [prepare_hwsd_soil_groups()]
#' @examples
#' \dontrun{
#' load_hwsd_soil_units(vec_paths = hwsd_class_tables)
#' }
load_hwsd_soil_units <- function(vec_paths) {
  assertthat::assert_that(
    base::is.character(vec_paths),
    !base::anyNA(vec_paths),
    msg = "`vec_paths` must be a character vector of paths."
  )

  vec_path_smu <-
    vec_paths[base::basename(vec_paths) == "hwsd2_smu.csv"]

  if (
    base::length(vec_path_smu) != 1L
  ) {
    cli::cli_abort(
      "Exactly one class table named {.file hwsd2_smu.csv} is needed."
    )
  }

  data_raw <-
    readr::read_csv(
      file = vec_path_smu,
      col_types = readr::cols(.default = readr::col_character()),
      na = c("", "NA"),
      progress = FALSE
    )

  vec_columns <-
    c("HWSD2_SMU_ID", "SEQUENCE", "SHARE", "WRB2")

  vec_columns_missing <-
    base::setdiff(vec_columns, base::colnames(data_raw))

  if (
    base::length(vec_columns_missing) > 0L
  ) {
    cli::cli_abort(
      stringr::str_c(
        "{.file hwsd2_smu.csv} lacks the column{?s} ",
        "{.field {vec_columns_missing}}."
      )
    )
  }

  res <-
    data_raw |>
    dplyr::select(dplyr::all_of(vec_columns)) |>
    dplyr::mutate(
      HWSD2_SMU_ID = base::as.integer(.data[["HWSD2_SMU_ID"]]),
      SEQUENCE = base::as.integer(.data[["SEQUENCE"]]),
      SHARE = base::as.numeric(.data[["SHARE"]])
    )

  n_repeated <-
    res |>
    dplyr::count(.data[["HWSD2_SMU_ID"]], .data[["SEQUENCE"]]) |>
    dplyr::filter(.data[["n"]] > 1L) |>
    base::nrow()

  if (
    n_repeated > 0L
  ) {
    cli::cli_abort(
      "{n_repeated} soil unit{?s} list a component twice."
    )
  }

  return(res)
}

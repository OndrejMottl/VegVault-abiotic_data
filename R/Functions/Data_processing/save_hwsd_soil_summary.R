#' @title Save the summary of the processed HWSD soil map
#' @description
#' Count the processed cells by their dominant class and the soil units by
#' class, so the release notes can say how many cells became soil groups
#' and how many have no row because their dominant class is not a soil.
#' @param path_class_raster
#' Character scalar. Path of the aggregated class raster (before the
#' non-soil classes are set to missing).
#' @param data_soil_groups
#' A data frame as returned by `prepare_hwsd_soil_groups()`.
#' @param path_output
#' Character scalar. Path of the CSV file to write; its folder is created
#' when missing.
#' @return
#' `path_output`. The CSV file has the columns `class_code`, `class_type`
#' (`"soil"`, `"non_soil"`, `"unlisted"` or `"missing"`), `wrb_codes`
#' (the two-letter codes of the class, joined by `;`), `n_soil_units` and
#' `n_processed_cells`, one row per class, sorted by `class_code` with the
#' missing cells last.
#' @seealso [prepare_hwsd_soil_groups()], [save_aggregated_raster()]
#' @examples
#' \dontrun{
#' save_hwsd_soil_summary(
#'   path_class_raster = "Data/Processed/HWSD_v2.2/Intermediate/agg.tif",
#'   data_soil_groups = data_soil_groups,
#'   path_output = "Outputs/Tables/processing_hwsd_soil_summary.csv"
#' )
#' }
save_hwsd_soil_summary <- function(
  path_class_raster,
  data_soil_groups,
  path_output
) {
  assertthat::assert_that(
    rlang::is_string(path_class_raster),
    base::file.exists(path_class_raster),
    msg = "`path_class_raster` must be the path to an existing raster."
  )

  vec_columns <-
    c("HWSD2_SMU_ID", "WRB2", "class_code", "class_type")

  assertthat::assert_that(
    base::is.data.frame(data_soil_groups),
    base::all(vec_columns %in% base::colnames(data_soil_groups)),
    msg = stringr::str_c(
      "`data_soil_groups` must be a data frame with the columns ",
      stringr::str_c(vec_columns, collapse = ", "),
      "."
    )
  )

  assertthat::assert_that(
    rlang::is_string(path_output),
    base::nzchar(path_output),
    msg = "`path_output` must be one file path."
  )

  data_units <-
    data_soil_groups |>
    dplyr::summarise(
      class_type = dplyr::first(.data[["class_type"]]),
      wrb_codes = stringr::str_c(
        base::sort(base::unique(stats::na.omit(.data[["WRB2"]]))),
        collapse = ";"
      ),
      n_soil_units = dplyr::n(),
      .by = "class_code"
    ) |>
    dplyr::mutate(class_code = base::as.numeric(.data[["class_code"]]))

  data_cells <-
    tibble::tibble(
      class_code = terra::values(
        terra::rast(path_class_raster),
        mat = FALSE
      )
    ) |>
    dplyr::summarise(
      n_processed_cells = dplyr::n(),
      .by = "class_code"
    )

  # Classes from the units and the raster; missing cells last
  res_summary <-
    data_units |>
    dplyr::full_join(data_cells, by = "class_code") |>
    dplyr::mutate(
      class_type = dplyr::if_else(
        base::is.na(.data[["class_code"]]),
        "missing",
        .data[["class_type"]]
      ),
      n_processed_cells = dplyr::coalesce(
        base::as.numeric(.data[["n_processed_cells"]]),
        0
      )
    ) |>
    dplyr::arrange(
      base::is.na(.data[["class_code"]]),
      .data[["class_code"]]
    ) |>
    dplyr::select(
      "class_code", "class_type", "wrb_codes", "n_soil_units",
      "n_processed_cells"
    )

  base::dir.create(
    path = base::dirname(path_output),
    recursive = TRUE,
    showWarnings = FALSE
  )

  readr::write_csv(x = res_summary, file = path_output, na = "")

  res <-
    path_output

  return(res)
}

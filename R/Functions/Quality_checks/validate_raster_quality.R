#' @title Validate the findings of a raster quality check
#' @description
#' Stop with an error that names every failing file, variable, age, band
#' and rule when any finding of a raster quality check failed; pass the
#' findings through otherwise.
#' @param data_findings
#' A data frame of findings, as returned by the `diagnose_raster_*()`
#' functions and bound together.
#' @param n_show
#' Whole number, at least 1. Largest number of failing findings listed in
#' the error message; the rest are counted. Default `30`.
#' @return
#' `data_findings`, unchanged and invisibly, when no finding failed.
#' @details
#' Findings with status `"allowed"` do not fail. An empty findings table
#' fails, because nothing was checked. Save the tables with
#' `save_raster_quality_tables()` before calling this function, so they
#' exist on disk when it stops.
#' @seealso [summarise_raster_quality()], [save_raster_quality_tables()]
#' @examples
#' \dontrun{
#' validate_raster_quality(data_findings = data_findings)
#' }
validate_raster_quality <- function(
  data_findings,
  n_show = 30L
) {
  vec_columns_findings <-
    c(
      "variable", "age", "file_name", "band_south", "band_north", "rule",
      "detail", "status"
    )

  assertthat::assert_that(
    base::is.data.frame(data_findings),
    base::all(vec_columns_findings %in% base::colnames(data_findings)),
    msg = stringr::str_c(
      "`data_findings` must be a data frame with the columns ",
      stringr::str_c(vec_columns_findings, collapse = ", "),
      "."
    )
  )

  assertthat::assert_that(
    base::is.numeric(n_show),
    base::length(n_show) == 1L,
    !base::is.na(n_show),
    n_show >= 1,
    n_show == base::round(n_show),
    msg = "`n_show` must be a whole number of at least 1."
  )

  if (
    base::nrow(data_findings) == 0L
  ) {
    cli::cli_abort(
      "No finding was produced, so nothing was checked; the check fails."
    )
  }

  data_failures <-
    dplyr::filter(data_findings, .data[["status"]] == "fail")

  n_failures <-
    base::nrow(data_failures)

  if (
    n_failures > 0L
  ) {
    vec_lines <-
      data_failures |>
      utils::head(n = n_show) |>
      dplyr::mutate(
        text_band = dplyr::if_else(
          base::is.na(.data[["band_south"]]),
          "whole file",
          stringr::str_c(
            "band ", .data[["band_south"]], " to ", .data[["band_north"]]
          )
        ),
        text_line = stringr::str_c(
          .data[["file_name"]], " (", .data[["variable"]],
          ", age ", .data[["age"]], ", ", .data[["text_band"]], "): ",
          .data[["rule"]], ". ", .data[["detail"]]
        )
      ) |>
      dplyr::pull("text_line") |>
      # Details may hold read errors with braces, which cli would interpolate
      stringr::str_replace_all(c("\\{" = "{{", "\\}" = "}}"))

    n_unlisted <-
      n_failures - base::length(vec_lines)

    vec_message <-
      c(
        "The raster quality check found {n_failures} failing finding{?s}.",
        rlang::set_names(vec_lines, base::rep("x", base::length(vec_lines))),
        "i" = if (
          n_unlisted > 0L
        ) {
          "... and {n_unlisted} more; see the findings table."
        } else {
          "See the findings table for the values and thresholds."
        }
      )

    cli::cli_abort(vec_message)
  }

  res <- data_findings

  return(invisible(res))
}

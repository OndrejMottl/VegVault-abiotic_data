#' @title Summarise raster quality findings per file
#' @description
#' Reduce the findings of a raster quality check to one row per expected
#' file with a pass or fail status and the rules that failed or were
#' allowed.
#' @param data_expected_files
#' A data frame with the columns `product`, `variable`, `age` and
#' `file_name` (unique), as described in `diagnose_raster_files()`.
#' @param data_findings
#' A data frame of findings from `diagnose_raster_files()`,
#' `diagnose_raster_bands()` and `diagnose_raster_pairs()`, bound together.
#' @return
#' A tibble with one row per expected file and the columns `product`,
#' `variable`, `age`, `file_name`, `status` (`"pass"` or `"fail"`),
#' `n_failures` (failing findings), `failing_rules` and `allowed_rules`
#' (the rules, sorted and separated by `", "`; `NA` when there are none).
#' @details
#' A file passes only when none of its findings failed. A file without any
#' finding fails with the rule `not_checked`, because it was not checked
#' at all.
#' @seealso [validate_raster_quality()], [save_raster_quality_tables()]
#' @examples
#' \dontrun{
#' summarise_raster_quality(
#'   data_expected_files = data_expected_files,
#'   data_findings = data_findings
#' )
#' }
summarise_raster_quality <- function(
  data_expected_files,
  data_findings
) {
  vec_columns_expected <-
    c("product", "variable", "age", "file_name")

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
    base::is.data.frame(data_findings),
    base::all(
      c("file_name", "rule", "status") %in% base::colnames(data_findings)
    ),
    msg = stringr::str_c(
      "`data_findings` must be a data frame with the columns file_name, ",
      "rule and status."
    )
  )

  data_per_file <-
    data_findings |>
    dplyr::group_by(.data[["file_name"]]) |>
    dplyr::summarise(
      n_findings = dplyr::n(),
      n_failures = base::sum(.data[["status"]] == "fail"),
      failing_rules = stringr::str_c(
        base::sort(
          base::unique(.data[["rule"]][.data[["status"]] == "fail"])
        ),
        collapse = ", "
      ),
      allowed_rules = stringr::str_c(
        base::sort(
          base::unique(.data[["rule"]][.data[["status"]] == "allowed"])
        ),
        collapse = ", "
      ),
      .groups = "drop"
    )

  # A file without findings was never checked, so it fails
  res <-
    data_expected_files |>
    dplyr::select(dplyr::all_of(vec_columns_expected)) |>
    dplyr::left_join(
      data_per_file,
      by = dplyr::join_by("file_name"),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      flag_checked = !base::is.na(.data[["n_findings"]]),
      n_failures = dplyr::if_else(
        .data[["flag_checked"]],
        .data[["n_failures"]],
        1L
      ),
      failing_rules = dplyr::case_when(
        !.data[["flag_checked"]] ~ "not_checked",
        .data[["failing_rules"]] == "" ~ NA_character_,
        .default = .data[["failing_rules"]]
      ),
      allowed_rules = dplyr::na_if(.data[["allowed_rules"]], ""),
      status = dplyr::if_else(.data[["n_failures"]] > 0, "fail", "pass")
    ) |>
    dplyr::select(
      dplyr::all_of(vec_columns_expected),
      "status",
      "n_failures",
      "failing_rules",
      "allowed_rules"
    )

  return(res)
}

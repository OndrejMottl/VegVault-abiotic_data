#' @title Validate the configured columns of the cell tables
#' @description
#' Check that the configured columns of a product's cell tables hold the
#' cell columns and only fixed columns the extraction stage can fill, and
#' that a variable label is configured when the tables need one.
#' @param vec_columns
#' Character vector. The configured columns: `long`, `lat`, `value` and
#' any of `time_id` and `var_name`, each once, in any order.
#' @param list_var_names
#' `NULL` (default) or a named list (or named character vector) of single
#' strings: the label of each variable. Required when `vec_columns` holds
#' `var_name`.
#' @param vec_variables
#' `NULL` (default) or a character vector of variables that need a label
#' when `vec_columns` holds `var_name`.
#' @param label
#' Character scalar. How the messages name the configuration entry.
#' Default `"The product"`.
#' @return
#' `TRUE`, invisibly. Aborts with a message naming `columns` or
#' `var_names` when a requirement is not met.
#' @seealso [build_climate_extraction_targets()],
#'   [build_hwsd_extraction_targets()]
#' @examples
#' validate_extraction_columns(
#'   vec_columns = c("long", "lat", "time_id", "value")
#' )
validate_extraction_columns <- function(
  vec_columns,
  list_var_names = NULL,
  vec_variables = NULL,
  label = "The product"
) {
  assertthat::assert_that(
    rlang::is_string(label),
    msg = "`label` must be one string."
  )

  vec_columns_cells <-
    c("long", "lat", "value")

  vec_columns_allowed <-
    c(vec_columns_cells, "time_id", "var_name")

  assertthat::assert_that(
    base::is.character(vec_columns),
    !base::anyNA(vec_columns),
    !base::anyDuplicated(vec_columns),
    base::all(vec_columns_cells %in% vec_columns),
    base::all(vec_columns %in% vec_columns_allowed),
    msg = stringr::str_c(
      label, " needs `columns`: long, lat, value and any of time_id and ",
      "var_name, each once."
    )
  )

  vec_var_names <-
    base::unlist(list_var_names)

  assertthat::assert_that(
    !("var_name" %in% vec_columns) ||
      (rlang::is_named(list_var_names) &&
        base::is.character(vec_var_names) &&
        base::length(vec_var_names) == base::length(list_var_names) &&
        !base::anyNA(vec_var_names) &&
        base::all(vec_variables %in% base::names(vec_var_names))),
    msg = stringr::str_c(
      label, " needs `var_names` with a label for each variable, because ",
      "its `columns` hold var_name."
    )
  )

  res <- TRUE

  return(invisible(res))
}

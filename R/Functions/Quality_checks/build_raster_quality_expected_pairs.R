#' @title Build the expected pairs of a raster quality-check set
#' @description
#' Pair every checked raster of a quality-check set with its reference
#' raster of the same product and age (for example bio18 with bio12), for
#' `compute_raster_pair_comparison()` and `diagnose_raster_pairs()`.
#' @param data_expected_files
#' A data frame with one row per file and at least the columns `product`,
#' `variable` (character), `age` (numeric), `file_name` and `path_file`
#' (character), as returned by `build_raster_quality_expected_files()` or
#' `load_raster_file_inventory()`.
#' @param list_pairs
#' A list of pairs (may be empty); each is a list with `variable` (the
#' checked variable) and `reference` (its reference variable).
#' @return
#' A tibble with one row per pair and age: every column of the checked
#' file's row, followed by the columns of the reference file's row other
#' than `product` and `age`, each with the suffix `_reference` (so
#' `file_name_reference`, `path_file_reference` and so on). An empty
#' `list_pairs` gives zero rows with the same columns.
#' @details
#' Carrying every column of both files means that a change in either file
#' (for example its observed size or modification time) changes the row
#' of the pair. Aborts when a pair names a variable that the set does not
#' have, pairs a variable with itself, or is listed twice, and when a
#' checked file has no reference file of the same age.
#' @seealso [compute_raster_pair_comparison()], [diagnose_raster_pairs()]
#' @examples
#' \dontrun{
#' build_raster_quality_expected_pairs(
#'   data_expected_files = data_expected_files,
#'   list_pairs = list(list(variable = "bio18", reference = "bio12"))
#' )
#' }
build_raster_quality_expected_pairs <- function(
  data_expected_files,
  list_pairs
) {
  vec_columns_required <-
    c("product", "variable", "age", "file_name", "path_file")

  assertthat::assert_that(
    base::is.data.frame(data_expected_files),
    base::all(vec_columns_required %in% base::colnames(data_expected_files)),
    msg = stringr::str_c(
      "`data_expected_files` must be a data frame with the columns ",
      "`product`, `variable`, `age`, `file_name` and `path_file`."
    )
  )

  assertthat::assert_that(
    base::is.list(list_pairs),
    base::all(
      purrr::map_lgl(
        .x = list_pairs,
        .f = ~ base::is.list(.x) &&
          rlang::is_string(.x[["variable"]]) &&
          rlang::is_string(.x[["reference"]])
      )
    ),
    msg = stringr::str_c(
      "`list_pairs` must be a list of pairs, each a list with one ",
      "`variable` and one `reference`."
    )
  )

  data_pairs_spec <-
    tibble::tibble(
      variable = purrr::map_chr(.x = list_pairs, .f = ~ .x[["variable"]]),
      variable_reference = purrr::map_chr(
        .x = list_pairs,
        .f = ~ .x[["reference"]]
      )
    )

  vec_variables_unknown <-
    base::setdiff(
      c(
        dplyr::pull(data_pairs_spec, "variable"),
        dplyr::pull(data_pairs_spec, "variable_reference")
      ),
      dplyr::pull(data_expected_files, "variable")
    )

  assertthat::assert_that(
    base::length(vec_variables_unknown) == 0L,
    msg = stringr::str_c(
      "`list_pairs` names variables the set does not have: ",
      stringr::str_c(vec_variables_unknown, collapse = ", "),
      "."
    )
  )

  assertthat::assert_that(
    base::all(
      dplyr::pull(data_pairs_spec, "variable") !=
        dplyr::pull(data_pairs_spec, "variable_reference")
    ),
    msg = "A pair of `list_pairs` pairs a variable with itself."
  )

  assertthat::assert_that(
    !base::anyDuplicated(data_pairs_spec),
    msg = "A pair of `list_pairs` is listed twice."
  )

  vec_columns_files <-
    base::colnames(data_expected_files)

  vec_columns_reference <-
    base::setdiff(vec_columns_files, c("product", "age"))

  data_reference <-
    data_expected_files |>
    dplyr::rename_with(
      .fn = ~ stringr::str_c(.x, "_reference"),
      .cols = dplyr::all_of(vec_columns_reference)
    )

  data_pairs_joined <-
    data_pairs_spec |>
    dplyr::inner_join(
      data_expected_files,
      by = "variable",
      relationship = "one-to-many"
    ) |>
    dplyr::left_join(
      data_reference,
      by = c("product", "age", "variable_reference"),
      relationship = "many-to-one"
    )

  vec_files_without_reference <-
    data_pairs_joined |>
    dplyr::filter(base::is.na(.data[["file_name_reference"]])) |>
    dplyr::pull("file_name")

  n_files_without_reference <-
    base::length(vec_files_without_reference)

  if (
    n_files_without_reference > 0L
  ) {
    cli::cli_abort(
      c(
        stringr::str_c(
          "{n_files_without_reference} checked file{?s} without a ",
          "reference file of the same age."
        ),
        "x" = "{.val {vec_files_without_reference}}"
      )
    )
  }

  res <-
    data_pairs_joined |>
    dplyr::select(
      dplyr::all_of(
        c(
          vec_columns_files,
          stringr::str_c(vec_columns_reference, "_reference")
        )
      )
    )

  return(res)
}

#' @title Build the expected files of a raster quality-check set
#' @description
#' List every raster file that a quality-check set must contain, with its
#' path and the size, dimensions and extent it is expected to have, from
#' the configuration of the set.
#' @param list_check_set
#' A list describing the set: `product` (one string), `dir` (folder of the
#' files, relative to `path_data_storage`), `file_name_template` (a
#' `stringr::str_glue()` template using `{file_variable}` and, for
#' palaeoclimate, `{time_id}`), `variables` (named list or named character
#' vector: variable name to the token used in the file name), `ages` (a
#' list with `from`, `to` and `by`, in years before present) and, when the
#' template uses `{time_id}`, `time_id` (a list with `origin_age` and
#' `step_years`).
#' @param list_grid
#' A list with the expected `n_col`, `n_row`, `x_min`, `x_max`, `y_min`
#' and `y_max` of every file of the set.
#' @param path_data_storage
#' Character scalar. The data root that `dir` is relative to.
#' @param data_server_sizes
#' A data frame with the columns `product`, `file_name` and `size_bytes`
#' (as returned by `load_raster_server_sizes()`) to take the expected
#' sizes from, or `NULL` (default) to leave the expected size `NA`, which
#' skips the size rule of `diagnose_raster_files()`.
#' @return
#' A tibble with one row per variable and age, in the order of `variables`
#' and then of age, and the columns `product`, `variable` (character),
#' `age` (numeric), `file_name`, `path_file` (character),
#' `expected_size_bytes`, `expected_n_col`, `expected_n_row`,
#' `expected_x_min`, `expected_x_max`, `expected_y_min` and
#' `expected_y_max` (numeric).
#' @details
#' The file list comes from `build_source_expected_files()`, which also
#' lists the raw files of the download stage, so both stages name files
#' the same way. The time identifier of a palaeoclimate file is
#' `(origin_age - age) / step_years`; CHELSA-TraCE21k uses
#' `origin_age = 2000` and `step_years = 100`, so age 0 is `20` and age
#' 18000 is `-160`. Aborts when that is not a whole number, when the
#' template uses `{time_id}` but `time_id` is not given, when the file
#' names are not unique, or when `data_server_sizes` is given but holds no
#' size for an expected file: a size that is asked for but missing is a
#' configuration error, not a reason to skip the rule.
#' @seealso [build_source_expected_files()], [diagnose_raster_files()],
#'   [load_raster_server_sizes()], [load_raster_file_inventory()]
#' @examples
#' build_raster_quality_expected_files(
#'   list_check_set = list(
#'     product = "CHELSA_V2.1",
#'     dir = "Input/Neoclimate/CHELSA_V2.1",
#'     file_name_template = "CHELSA_{file_variable}_1981-2010_V.2.1.tif",
#'     variables = list(bio1 = "bio1", bio12 = "bio12"),
#'     ages = list(from = 0, to = 0, by = 1)
#'   ),
#'   list_grid = list(
#'     n_col = 43200, n_row = 20880,
#'     x_min = -180, x_max = 180, y_min = -90, y_max = 84
#'   ),
#'   path_data_storage = "Data"
#' )
build_raster_quality_expected_files <- function(
  list_check_set,
  list_grid,
  path_data_storage,
  data_server_sizes = NULL
) {
  assertthat::assert_that(
    base::is.list(list_check_set),
    msg = "`list_check_set` must be a list."
  )

  vec_grid_fields <-
    c("n_col", "n_row", "x_min", "x_max", "y_min", "y_max")

  assertthat::assert_that(
    base::is.list(list_grid),
    base::all(
      purrr::map_lgl(
        .x = vec_grid_fields,
        .f = ~ base::is.numeric(list_grid[[.x]]) &&
          base::length(list_grid[[.x]]) == 1L &&
          base::is.finite(list_grid[[.x]])
      )
    ),
    msg = stringr::str_c(
      "`list_grid` needs `n_col`, `n_row`, `x_min`, `x_max`, `y_min` ",
      "and `y_max` as numbers."
    )
  )

  data_files <-
    build_source_expected_files(
      list_source = list_check_set,
      path_data_storage = path_data_storage,
      data_server_sizes = data_server_sizes
    )

  res <-
    data_files |>
    dplyr::mutate(
      expected_n_col = base::as.numeric(list_grid[["n_col"]]),
      expected_n_row = base::as.numeric(list_grid[["n_row"]]),
      expected_x_min = base::as.numeric(list_grid[["x_min"]]),
      expected_x_max = base::as.numeric(list_grid[["x_max"]]),
      expected_y_min = base::as.numeric(list_grid[["y_min"]]),
      expected_y_max = base::as.numeric(list_grid[["y_max"]])
    ) |>
    dplyr::select(
      "product", "variable", "age", "file_name", "path_file",
      "expected_size_bytes", "expected_n_col", "expected_n_row",
      "expected_x_min", "expected_x_max", "expected_y_min",
      "expected_y_max"
    )

  return(res)
}

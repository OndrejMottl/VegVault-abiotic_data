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
#' The time identifier of a palaeoclimate file is
#' `(origin_age - age) / step_years`; CHELSA-TraCE21k uses
#' `origin_age = 2000` and `step_years = 100`, so age 0 is `20` and age
#' 18000 is `-160`. Aborts when that is not a whole number, when the
#' template uses `{time_id}` but `time_id` is not given, when the file
#' names are not unique, or when `data_server_sizes` is given but holds no
#' size for an expected file: a size that is asked for but missing is a
#' configuration error, not a reason to skip the rule.
#' @seealso [diagnose_raster_files()], [load_raster_server_sizes()],
#'   [load_raster_file_inventory()]
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

  purrr::walk(
    .x = c("product", "dir", "file_name_template"),
    .f = ~ assertthat::assert_that(
      rlang::is_string(list_check_set[[.x]]) &&
        base::nzchar(list_check_set[[.x]]),
      msg = stringr::str_c(
        "`list_check_set` needs `", .x, "` as one non-empty string."
      )
    )
  )

  vec_variables_raw <-
    list_check_set[["variables"]]

  assertthat::assert_that(
    (base::is.list(vec_variables_raw) ||
      base::is.character(vec_variables_raw)) &&
      rlang::is_named(vec_variables_raw) &&
      base::all(
        purrr::map_lgl(
          .x = vec_variables_raw,
          .f = ~ rlang::is_string(.x) && base::nzchar(.x)
        )
      ),
    !base::anyDuplicated(base::names(vec_variables_raw)),
    msg = stringr::str_c(
      "`list_check_set` needs `variables` as unique variable names ",
      "mapped to the file-name token of each."
    )
  )

  list_ages <-
    list_check_set[["ages"]]

  assertthat::assert_that(
    base::is.list(list_ages),
    base::all(
      purrr::map_lgl(
        .x = c("from", "to", "by"),
        .f = ~ base::is.numeric(list_ages[[.x]]) &&
          base::length(list_ages[[.x]]) == 1L &&
          base::is.finite(list_ages[[.x]])
      )
    ),
    msg = "`list_check_set` needs `ages` with numbers `from`, `to` and `by`."
  )

  assertthat::assert_that(
    list_ages[["by"]] > 0,
    list_ages[["to"]] >= list_ages[["from"]],
    msg = "`ages` of `list_check_set` needs `by` > 0 and `to` >= `from`."
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

  assertthat::assert_that(
    rlang::is_string(path_data_storage),
    !base::is.na(path_data_storage),
    base::nzchar(path_data_storage),
    msg = "`path_data_storage` must be one non-empty string."
  )

  assertthat::assert_that(
    base::is.null(data_server_sizes) ||
      (
        base::is.data.frame(data_server_sizes) &&
          base::all(
            c("product", "file_name", "size_bytes") %in%
              base::colnames(data_server_sizes)
          )
      ),
    msg = stringr::str_c(
      "`data_server_sizes` must be NULL or a data frame with the ",
      "columns `product`, `file_name` and `size_bytes`."
    )
  )

  flag_time_id <-
    stringr::str_detect(
      string = list_check_set[["file_name_template"]],
      pattern = stringr::fixed("{time_id}")
    )

  list_time_id <-
    list_check_set[["time_id"]]

  if (
    isTRUE(flag_time_id)
  ) {
    assertthat::assert_that(
      base::is.list(list_time_id),
      base::all(
        purrr::map_lgl(
          .x = c("origin_age", "step_years"),
          .f = ~ base::is.numeric(list_time_id[[.x]]) &&
            base::length(list_time_id[[.x]]) == 1L &&
            base::is.finite(list_time_id[[.x]])
        )
      ),
      msg = stringr::str_c(
        "The file-name template uses `{time_id}`, so `list_check_set` ",
        "needs `time_id` with numbers `origin_age` and `step_years`."
      )
    )
  }

  vec_file_variables <-
    base::unlist(vec_variables_raw)

  vec_ages <-
    base::seq(
      from = list_ages[["from"]],
      to = list_ages[["to"]],
      by = list_ages[["by"]]
    ) |>
    base::as.numeric()

  # NA for products without time identifiers; the template then does not
  #   use them
  value_origin_age <-
    base::as.numeric(list_time_id[["origin_age"]] %||% NA_real_)

  value_step_years <-
    base::as.numeric(list_time_id[["step_years"]] %||% NA_real_)

  vec_time_id <-
    (value_origin_age - vec_ages) / value_step_years

  if (
    isTRUE(flag_time_id) &&
      base::any(base::abs(vec_time_id - base::round(vec_time_id)) > 1e-9)
  ) {
    cli::cli_abort(
      c(
        "A time identifier is not a whole number.",
        "i" = stringr::str_c(
          "Check `ages` and `time_id` of the set of product ",
          "{.val {list_check_set[['product']]}}."
        )
      )
    )
  }

  data_files_time <-
    tidyr::expand_grid(
      variable = base::names(vec_file_variables),
      age = vec_ages
    ) |>
    dplyr::mutate(
      file_variable = base::unname(
        vec_file_variables[.data[["variable"]]]
      ),
      time_id = base::as.integer(
        base::round(
          (value_origin_age - .data[["age"]]) / value_step_years
        )
      )
    )

  data_files_named <-
    data_files_time |>
    dplyr::mutate(
      product = list_check_set[["product"]],
      file_name = base::as.character(
        stringr::str_glue_data(
          .x = data_files_time,
          list_check_set[["file_name_template"]]
        )
      ),
      path_file = base::file.path(
        path_data_storage,
        list_check_set[["dir"]],
        .data[["file_name"]]
      )
    )

  vec_file_names <-
    dplyr::pull(data_files_named, "file_name")

  if (
    base::anyDuplicated(vec_file_names) > 0L
  ) {
    cli::cli_abort(
      c(
        "The file names of the set are not unique.",
        "i" = stringr::str_c(
          "Check that `file_name_template` uses every varying part ",
          "(variable and time)."
        )
      )
    )
  }

  # Without a server-size record, an empty table leaves every size NA
  data_sizes_input <-
    data_server_sizes %||%
    tibble::tibble(
      product = base::character(0),
      file_name = base::character(0),
      size_bytes = base::numeric(0)
    )

  data_sizes <-
    data_sizes_input |>
    dplyr::filter(
      .data[["product"]] == list_check_set[["product"]]
    ) |>
    dplyr::mutate(
      expected_size_bytes = base::as.numeric(.data[["size_bytes"]])
    ) |>
    dplyr::select("product", "file_name", "expected_size_bytes")

  data_files_sized <-
    data_files_named |>
    dplyr::left_join(
      data_sizes,
      by = c("product", "file_name"),
      relationship = "one-to-one"
    )

  vec_files_without_size <-
    data_files_sized |>
    dplyr::filter(base::is.na(.data[["expected_size_bytes"]])) |>
    dplyr::pull("file_name")

  n_files_without_size <-
    base::length(vec_files_without_size)

  if (
    !base::is.null(data_server_sizes) &&
      n_files_without_size > 0L
  ) {
    cli::cli_abort(
      c(
        "No server size recorded for {n_files_without_size} file{?s}.",
        "x" = "{.val {vec_files_without_size}}",
        "i" = "Add them to the server-size record or skip the size rule."
      )
    )
  }

  res <-
    data_files_sized |>
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

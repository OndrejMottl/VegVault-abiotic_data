#' @title Build the expected files of a raw data source
#' @description
#' List every file that a raw data source must provide, with its path, its
#' download address and the size its server announced, from the
#' configuration of the source.
#' @param list_source
#' A list describing the source: `product` (one string), `dir` (folder of
#' the files, relative to `path_data_storage`) and either a templated or a
#' fixed file list. Templated: `file_name_template` (a
#' `stringr::str_glue()` template using `{file_variable}` and, for
#' palaeoclimate, `{time_id}`), `variables` (named list or named character
#' vector: variable name to the token used in the file name), `ages` (a
#' list with `from`, `to` and `by`, in years before present), `time_id` (a
#' list with `origin_age` and `step_years`; needed when the template uses
#' `{time_id}`) and optionally `url_template` (a template that may also use
#' `{file_name}`). Fixed: `files`, a named list of file name to download
#' address. Other elements of the list are ignored.
#' @param path_data_storage
#' Character scalar. The data root that `dir` is relative to.
#' @param data_server_sizes
#' A data frame with the columns `product`, `file_name` and `size_bytes`
#' (as returned by `load_raster_server_sizes()`) to take the expected sizes
#' from, or `NULL` (default) to leave every expected size `NA`.
#' @return
#' A tibble with one row per file and the columns `product`, `variable`
#' (character), `age` (numeric), `time_id` (integer), `file_name`,
#' `path_file`, `url` (character) and `expected_size_bytes` (numeric). A
#' templated source gives one row per variable and age, in the order of
#' `variables` and then of age; a fixed source one row per file, in the
#' order of `files`, with `variable`, `age` and `time_id` `NA`. `time_id`
#' is `NA` when the template does not use it, and `url` when the source
#' gives no address.
#' @details
#' The time identifier of a palaeoclimate file is
#' `(origin_age - age) / step_years`; CHELSA-TraCE21k uses
#' `origin_age = 2000` and `step_years = 100`, so age 0 is `20` and age
#' 18000 is `-160`. Aborts when the source has both or neither of
#' `file_name_template` and `files`, when a time identifier is not a whole
#' number, when the template uses `{time_id}` but `time_id` is not given,
#' when the file names are not unique, or when `data_server_sizes` is given
#' but holds no size for an expected file: a size that is asked for but
#' missing is a configuration error.
#' @seealso [build_raster_quality_expected_files()],
#'   [load_raster_server_sizes()], [run_source_downloads()]
#' @examples
#' build_source_expected_files(
#'   list_source = list(
#'     product = "CHELSA_V2.1",
#'     dir = "Input/Neoclimate/CHELSA_V2.1",
#'     file_name_template = "CHELSA_{file_variable}_1981-2010_V.2.1.tif",
#'     url_template = "https://example.org/bio/{file_name}",
#'     variables = list(bio1 = "bio1", bio12 = "bio12"),
#'     ages = list(from = 0, to = 0, by = 1)
#'   ),
#'   path_data_storage = "Data"
#' )
build_source_expected_files <- function(
  list_source,
  path_data_storage,
  data_server_sizes = NULL
) {
  assertthat::assert_that(
    base::is.list(list_source),
    msg = "`list_source` must be a list."
  )

  purrr::walk(
    .x = c("product", "dir"),
    .f = ~ assertthat::assert_that(
      rlang::is_string(list_source[[.x]]) &&
        base::nzchar(list_source[[.x]]),
      msg = stringr::str_c(
        "The source definition needs `", .x, "` as one non-empty string."
      )
    )
  )

  flag_template <-
    !base::is.null(list_source[["file_name_template"]])

  flag_fixed <-
    !base::is.null(list_source[["files"]])

  assertthat::assert_that(
    flag_template != flag_fixed,
    msg = stringr::str_c(
      "The source definition needs either `file_name_template` or `files`, ",
      "not both and not neither."
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

  # A templated source has one file per variable and age; a fixed source
  #   lists its files with their addresses
  data_files_listed <-
    if (
      isTRUE(flag_template)
    ) {
      assertthat::assert_that(
        rlang::is_string(list_source[["file_name_template"]]) &&
          base::nzchar(list_source[["file_name_template"]]),
        msg = stringr::str_c(
          "The source definition needs `file_name_template` as one ",
          "non-empty string."
        )
      )

      vec_variables_raw <-
        list_source[["variables"]]

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
          "The source definition needs `variables` as unique variable ",
          "names mapped to the file-name token of each."
        )
      )

      list_ages <-
        list_source[["ages"]]

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
        msg = stringr::str_c(
          "The source definition needs `ages` with numbers `from`, `to` ",
          "and `by`."
        )
      )

      assertthat::assert_that(
        list_ages[["by"]] > 0,
        list_ages[["to"]] >= list_ages[["from"]],
        msg = "`ages` of the source needs `by` > 0 and `to` >= `from`."
      )

      assertthat::assert_that(
        base::is.null(list_source[["url_template"]]) ||
          (rlang::is_string(list_source[["url_template"]]) &&
            base::nzchar(list_source[["url_template"]])),
        msg = stringr::str_c(
          "The source definition needs `url_template` as one non-empty ",
          "string, or none."
        )
      )

      flag_time_id <-
        stringr::str_detect(
          string = list_source[["file_name_template"]],
          pattern = stringr::fixed("{time_id}")
        )

      list_time_id <-
        list_source[["time_id"]]

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
            "The file-name template uses `{time_id}`, so the source ",
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

      # NA for products without time identifiers; the template then does
      #   not use them
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
              "Check `ages` and `time_id` of the source of product ",
              "{.val {list_source[['product']]}}."
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
          time_id = if (
            isTRUE(flag_time_id)
          ) {
            base::as.integer(
              base::round(
                (value_origin_age - .data[["age"]]) / value_step_years
              )
            )
          } else {
            NA_integer_
          }
        )

      data_files_template <-
        data_files_time |>
        dplyr::mutate(
          file_name = base::as.character(
            stringr::str_glue_data(
              .x = data_files_time,
              list_source[["file_name_template"]]
            )
          )
        )

      vec_urls <-
        if (
          base::is.null(list_source[["url_template"]])
        ) {
          base::rep(NA_character_, base::nrow(data_files_template))
        } else {
          base::as.character(
            stringr::str_glue_data(
              .x = data_files_template,
              list_source[["url_template"]]
            )
          )
        }

      data_files_template |>
        dplyr::mutate(url = vec_urls) |>
        dplyr::select("variable", "age", "time_id", "file_name", "url")
    } else {
      list_files <-
        list_source[["files"]]

      assertthat::assert_that(
        (base::is.list(list_files) || base::is.character(list_files)) &&
          base::length(list_files) > 0L &&
          rlang::is_named(list_files) &&
          base::all(
            purrr::map_lgl(
              .x = list_files,
              .f = ~ rlang::is_string(.x) && base::nzchar(.x)
            )
          ),
        msg = stringr::str_c(
          "The source definition needs `files` as file names mapped to ",
          "the download address of each."
        )
      )

      tibble::tibble(
        variable = NA_character_,
        age = NA_real_,
        time_id = NA_integer_,
        file_name = base::names(list_files),
        url = base::unname(base::unlist(list_files))
      )
    }

  data_files_named <-
    data_files_listed |>
    dplyr::mutate(
      product = list_source[["product"]],
      path_file = base::file.path(
        path_data_storage,
        list_source[["dir"]],
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
        "The file names of the source are not unique.",
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
      .data[["product"]] == list_source[["product"]]
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
    dplyr::select(
      "product", "variable", "age", "time_id", "file_name", "path_file",
      "url", "expected_size_bytes"
    )

  return(res)
}

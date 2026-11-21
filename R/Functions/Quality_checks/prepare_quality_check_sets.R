#' @title Prepare quality-check sets that refer to a raw data source
#' @description
#' Fill each quality-check set that names a `source` with the file
#' definition of that source (product, folder, file names, variables, ages
#' and time identifiers), so the raw files are defined once in the
#' configuration.
#' @param list_check_sets
#' The `check_sets` of the `quality_checks` configuration: a named list of
#' sets. A set either names a `source` or defines its files itself.
#' @param list_data_sources
#' The `data_sources` section of the configuration.
#' @return
#' `list_check_sets` in the same order, where every set with a `source`
#' holds `product`, `dir`, `file_name_template`, `variables`, `ages` and,
#' when the source has it, `time_id` of that source in addition to its own
#' elements, and no longer holds `source`.
#' @details
#' Aborts when a set names a source that `list_data_sources` does not
#' have, when the source has no `file_name_template` (a fixed file list
#' cannot be checked as rasters), or when the set also defines one of the
#' file elements itself, which would make two definitions.
#' @seealso [build_raster_quality_targets()],
#'   [build_source_expected_files()]
#' @examples
#' prepare_quality_check_sets(
#'   list_check_sets = list(
#'     neo_raw = list(source = "neo", required_pass = TRUE)
#'   ),
#'   list_data_sources = list(
#'     neo = list(
#'       product = "prod_neo",
#'       dir = "Input/Neo",
#'       file_name_template = "neo_{file_variable}.tif",
#'       variables = list(bio1 = "bio1"),
#'       ages = list(from = 0, to = 0, by = 1)
#'     )
#'   )
#' )
prepare_quality_check_sets <- function(
  list_check_sets,
  list_data_sources
) {
  assertthat::assert_that(
    base::is.list(list_check_sets),
    rlang::is_named(list_check_sets),
    base::all(purrr::map_lgl(.x = list_check_sets, .f = base::is.list)),
    msg = "`list_check_sets` must be a named list of check sets."
  )

  assertthat::assert_that(
    base::is.list(list_data_sources),
    rlang::is_named(list_data_sources) || base::length(list_data_sources) == 0L,
    msg = "`list_data_sources` must be a named list of sources."
  )

  vec_file_elements <-
    c("product", "dir", "file_name_template", "variables", "ages", "time_id")

  assertthat::assert_that(
    base::all(
      purrr::map_lgl(
        .x = list_check_sets,
        .f = ~ base::is.null(.x[["source"]]) ||
          rlang::is_string(.x[["source"]])
      )
    ),
    msg = "The `source` of a check set must be one source name."
  )

  vec_sources <-
    purrr::map_chr(
      .x = list_check_sets,
      .f = ~ .x[["source"]] %||% NA_character_
    )

  vec_sources_used <-
    base::unique(vec_sources[!base::is.na(vec_sources)])

  vec_sources_unknown <-
    base::setdiff(vec_sources_used, base::names(list_data_sources))

  if (
    base::length(vec_sources_unknown) > 0L
  ) {
    cli::cli_abort(
      c(
        "A check set names a source that `data_sources` does not have.",
        "x" = "{.val {vec_sources_unknown}}"
      )
    )
  }

  vec_sources_fixed <-
    vec_sources_used[
      purrr::map_lgl(
        .x = list_data_sources[vec_sources_used],
        .f = ~ base::is.null(.x[["file_name_template"]])
      )
    ]

  if (
    base::length(vec_sources_fixed) > 0L
  ) {
    cli::cli_abort(
      c(
        stringr::str_c(
          "A check set names a source without `file_name_template`; a ",
          "fixed file list cannot be checked as rasters."
        ),
        "x" = "{.val {vec_sources_fixed}}"
      )
    )
  }

  # A set that names a source must not define its files a second time
  vec_conflicts <-
    purrr::imap_chr(
      .x = list_check_sets,
      .f = ~ stringr::str_c(
        base::intersect(base::names(.x), vec_file_elements),
        collapse = ", "
      )
    )

  vec_sets_conflicting <-
    base::names(vec_conflicts)[
      !base::is.na(vec_sources) & base::nzchar(vec_conflicts)
    ]

  if (
    base::length(vec_sets_conflicting) > 0L
  ) {
    vec_details_conflicting <-
      stringr::str_c(
        vec_sets_conflicting,
        ": ",
        vec_conflicts[vec_sets_conflicting]
      )

    cli::cli_abort(
      c(
        "A check set names a source and also defines its files.",
        "x" = "{.val {vec_details_conflicting}}",
        "i" = "Define the files once, in `data_sources`."
      )
    )
  }

  res <-
    purrr::map(
      .x = list_check_sets,
      .f = ~ if (
        base::is.null(.x[["source"]])
      ) {
        .x
      } else {
        c(
          .x[base::setdiff(base::names(.x), "source")],
          purrr::keep_at(
            x = list_data_sources[[.x[["source"]]]],
            at = vec_file_elements
          )
        )
      }
    )

  return(res)
}

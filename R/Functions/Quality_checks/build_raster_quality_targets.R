#' @title Build the targets of a raster quality-check pipeline
#' @description
#' Declare the `{targets}` graph that checks one or more raster
#' quality-check sets: the expected files and pairs, the block, band and
#' pair statistics of every file (one dynamic branch per file or pair),
#' the findings, the per-file summary, the tables, and a final validation
#' that stops the run when a required set has a failing finding.
#' @param vec_check_sets
#' Character vector of unique set names, each a name of
#' `list_quality_checks[["check_sets"]]` made of lower-case letters,
#' digits and underscores.
#' @param list_quality_checks
#' The `quality_checks` section of the configuration: `band_width`,
#' `n_rows_block`, `value_tolerance`, `extent_tolerance`, `type_limits`
#' (`scales`, `offsets`, `integer_types`), `server_sizes_file` (relative
#' to `path_data_storage`; read only when a set checks server sizes),
#' `grids` (named grids as described in
#' `build_raster_quality_expected_files()`), `thresholds` (as in
#' `prepare_raster_quality_thresholds()`), `allow_list` (as in
#' `prepare_raster_quality_allow_list()`) and `check_sets`. Each check set
#' has the fields of `build_raster_quality_expected_files()` plus
#' `required_pass` (logical), `grid` (a name of `grids`),
#' `check_server_size` (logical) and `pairs` (as in
#' `build_raster_quality_expected_pairs()`; may be empty).
#' @param path_data_storage
#' Character scalar. The data root the folders of the sets are relative
#' to.
#' @param dir_tables
#' Character scalar. Folder the tables are written to.
#' @return
#' A list of target objects for `targets::tar_make()`. Shared targets:
#' `raster_quality_thresholds`, `raster_quality_allow_list`,
#' `raster_quality_type_limit_values` and, when a set checks server sizes,
#' `path_raster_server_sizes` (a file target) and
#' `data_raster_server_sizes`. Per set `<set>`: `<set>_expected_files`,
#' `<set>_file_inventory`, `<set>_block_extent`, `<set>_band_summary`,
#' `<set>_findings_files`, `<set>_findings_bands`, `<set>_findings`,
#' `<set>_file_summary` and `<set>_tables` (a file target), plus
#' `<set>_expected_pairs`, `<set>_pair_comparison` and
#' `<set>_findings_pairs` when the set has pairs. When at least one set
#' is required: `raster_quality_findings_required` and
#' `raster_quality_validated`.
#' @details
#' The configuration values are written into the target commands, so a
#' change in the configuration reruns exactly the targets it affects. The
#' file inventory always reruns; it reads only file sizes and modification
#' times, and a replaced file therefore reruns its own branches and no
#' other. The tables of a set are named `raster_quality_<set>_*.csv`.
#' `raster_quality_validated` depends on the tables of every set, so all
#' tables are written before a failing required set stops the run. Sets
#' that are not required (for example legacy rasters kept as a
#' comparison) are checked and reported but never stop the run.
#' @seealso [build_raster_quality_expected_files()],
#'   [compute_raster_band_summary()], [validate_raster_quality()]
#' @examples
#' \dontrun{
#' build_raster_quality_targets(
#'   vec_check_sets = "chelsa_v21_raw",
#'   list_quality_checks = load_config_value(value = "quality_checks"),
#'   path_data_storage = path_data_storage,
#'   dir_tables = here::here("Outputs", "Tables")
#' )
#' }
build_raster_quality_targets <- function(
  vec_check_sets,
  list_quality_checks,
  path_data_storage,
  dir_tables
) {
  assertthat::assert_that(
    base::is.list(list_quality_checks),
    msg = "`list_quality_checks` must be a list."
  )

  vec_keys_required <-
    c(
      "band_width", "n_rows_block", "value_tolerance", "extent_tolerance",
      "type_limits", "grids", "thresholds", "allow_list", "check_sets"
    )

  vec_keys_missing <-
    vec_keys_required[
      purrr::map_lgl(
        .x = vec_keys_required,
        .f = ~ base::is.null(list_quality_checks[[.x]])
      )
    ]

  assertthat::assert_that(
    base::length(vec_keys_missing) == 0L,
    msg = stringr::str_c(
      "`list_quality_checks` lacks: ",
      stringr::str_c(vec_keys_missing, collapse = ", "),
      "."
    )
  )

  assertthat::assert_that(
    base::is.character(vec_check_sets),
    base::length(vec_check_sets) > 0L,
    !base::anyNA(vec_check_sets),
    !base::anyDuplicated(vec_check_sets),
    base::all(
      stringr::str_detect(string = vec_check_sets, pattern = "^[a-z0-9_]+$")
    ),
    msg = stringr::str_c(
      "`vec_check_sets` must be unique names of lower-case letters, ",
      "digits and underscores."
    )
  )

  list_check_sets <-
    list_quality_checks[["check_sets"]]

  vec_sets_unknown <-
    base::setdiff(vec_check_sets, base::names(list_check_sets))

  assertthat::assert_that(
    base::length(vec_sets_unknown) == 0L,
    msg = stringr::str_c(
      "These check sets are not in `check_sets`: ",
      stringr::str_c(vec_sets_unknown, collapse = ", "),
      "."
    )
  )

  assertthat::assert_that(
    rlang::is_string(path_data_storage),
    !base::is.na(path_data_storage),
    base::nzchar(path_data_storage),
    msg = "`path_data_storage` must be one non-empty string."
  )

  assertthat::assert_that(
    rlang::is_string(dir_tables),
    !base::is.na(dir_tables),
    base::nzchar(dir_tables),
    msg = "`dir_tables` must be one non-empty string."
  )

  list_sets_selected <-
    list_check_sets[vec_check_sets]

  purrr::iwalk(
    .x = list_sets_selected,
    .f = ~ assertthat::assert_that(
      rlang::is_bool(.x[["required_pass"]]),
      rlang::is_bool(.x[["check_server_size"]]),
      msg = stringr::str_c(
        "Check set `", .y, "` needs `required_pass` and ",
        "`check_server_size` as TRUE or FALSE."
      )
    )
  )

  purrr::iwalk(
    .x = list_sets_selected,
    .f = ~ assertthat::assert_that(
      rlang::is_string(.x[["grid"]]),
      .x[["grid"]] %in% base::names(list_quality_checks[["grids"]]),
      msg = stringr::str_c(
        "Check set `", .y, "` names a `grid` that `grids` does not have."
      )
    )
  )

  flag_server_sizes <-
    purrr::map_lgl(
      .x = list_sets_selected,
      .f = ~ .x[["check_server_size"]]
    ) |>
    base::any()

  assertthat::assert_that(
    !flag_server_sizes ||
      rlang::is_string(list_quality_checks[["server_sizes_file"]]),
    msg = stringr::str_c(
      "A check set compares server sizes, so `list_quality_checks` ",
      "needs `server_sizes_file`."
    )
  )

  # Fail while declaring the pipeline, not hours into a run: the expected
  #   files and pairs, thresholds and allow-list are built once here
  purrr::walk(
    .x = list_sets_selected,
    .f = ~ build_raster_quality_expected_pairs(
      data_expected_files = build_raster_quality_expected_files(
        list_check_set = .x,
        list_grid = list_quality_checks[["grids"]][[.x[["grid"]]]],
        path_data_storage = path_data_storage
      ),
      list_pairs = .x[["pairs"]] %||% base::list()
    )
  )

  prepare_raster_quality_thresholds(
    list_thresholds = list_quality_checks[["thresholds"]]
  )

  prepare_raster_quality_allow_list(
    list_allow_list = list_quality_checks[["allow_list"]]
  )

  list_type_limits <-
    list_quality_checks[["type_limits"]]

  list_targets_shared <-
    list(
      targets::tar_target_raw(
        name = "raster_quality_thresholds",
        command = rlang::expr(
          prepare_raster_quality_thresholds(
            list_thresholds = !!list_quality_checks[["thresholds"]]
          )
        )
      ),
      targets::tar_target_raw(
        name = "raster_quality_allow_list",
        command = rlang::expr(
          prepare_raster_quality_allow_list(
            list_allow_list = !!list_quality_checks[["allow_list"]]
          )
        )
      ),
      targets::tar_target_raw(
        name = "raster_quality_type_limit_values",
        command = rlang::expr(
          build_raster_type_limit_values(
            vec_scales = !!base::unlist(list_type_limits[["scales"]]),
            vec_offsets = !!base::unlist(list_type_limits[["offsets"]]),
            vec_integer_types = !!base::unlist(
              list_type_limits[["integer_types"]]
            )
          )
        )
      )
    )

  list_targets_server_sizes <-
    if (
      isTRUE(flag_server_sizes)
    ) {
      list(
        targets::tar_target_raw(
          name = "path_raster_server_sizes",
          command = base::file.path(
            path_data_storage,
            list_quality_checks[["server_sizes_file"]]
          ),
          format = "file"
        ),
        targets::tar_target_raw(
          name = "data_raster_server_sizes",
          command = rlang::expr(
            load_raster_server_sizes(path_file = path_raster_server_sizes)
          )
        )
      )
    } else {
      list()
    }

  list_targets_sets <-
    purrr::map(
      .x = vec_check_sets,
      .f = ~ build_raster_quality_set_targets(
        sel_check_set = .x,
        list_quality_checks = list_quality_checks,
        path_data_storage = path_data_storage,
        dir_tables = dir_tables
      )
    ) |>
    purrr::list_flatten()

  vec_sets_required <-
    vec_check_sets[
      purrr::map_lgl(
        .x = list_sets_selected,
        .f = ~ .x[["required_pass"]]
      )
    ]

  # The validation waits for the tables of every set, so all tables are
  #   written before a failing required set stops the run
  list_targets_validation <-
    if (
      base::length(vec_sets_required) > 0L
    ) {
      list(
        targets::tar_target_raw(
          name = "raster_quality_findings_required",
          command = rlang::expr(
            dplyr::bind_rows(
              !!!purrr::map(
                .x = vec_sets_required,
                .f = ~ rlang::sym(stringr::str_c(.x, "_findings"))
              )
            )
          )
        ),
        targets::tar_target_raw(
          name = "raster_quality_validated",
          command = rlang::expr(
            validate_raster_quality(
              data_findings = raster_quality_findings_required
            )
          ),
          deps = c(
            "raster_quality_findings_required",
            "validate_raster_quality",
            stringr::str_c(vec_check_sets, "_tables")
          )
        )
      )
    } else {
      list()
    }

  res <-
    c(
      list_targets_shared,
      list_targets_server_sizes,
      list_targets_sets,
      list_targets_validation
    )

  return(res)
}

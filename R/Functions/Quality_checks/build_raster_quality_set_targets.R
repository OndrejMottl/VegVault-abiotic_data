#' @title Build the targets of one raster quality-check set
#' @description
#' Declare the `{targets}` of one quality-check set: its expected files,
#' the file inventory, one block, band and (when the set has pairs) pair
#' branch per file or pair, the findings, the per-file summary and the
#' tables. Used by `build_raster_quality_targets()`.
#' @param sel_check_set
#' Character scalar. Name of the set in
#' `list_quality_checks[["check_sets"]]`.
#' @param list_quality_checks
#' The `quality_checks` section of the configuration, as described in
#' `build_raster_quality_targets()`.
#' @param path_data_storage
#' Character scalar. The data root the folder of the set is relative to.
#' @param dir_tables
#' Character scalar. Folder the tables are written to.
#' @return
#' A list of target objects named as `build_raster_quality_targets()`
#' describes for one set: `<set>_expected_files`, `<set>_file_inventory`,
#' `<set>_block_extent`, `<set>_band_summary`, `<set>_findings_files`,
#' `<set>_findings_bands`, `<set>_findings`, `<set>_file_summary` and
#' `<set>_tables`, plus `<set>_expected_pairs`, `<set>_pair_comparison`
#' and `<set>_findings_pairs` when the set has pairs.
#' @details
#' The targets refer to the shared targets `raster_quality_thresholds`,
#' `raster_quality_allow_list`, `raster_quality_type_limit_values` and,
#' when the set checks server sizes, `data_raster_server_sizes`, which
#' `build_raster_quality_targets()` declares. The branches map over the
#' file inventory reduced to product, variable, age, file name, path,
#' observed size and modification time, so a change of an expected size
#' or extent reruns the cheap rule targets but no file statistics. The
#' configuration is validated by `build_raster_quality_targets()`.
#' @seealso [build_raster_quality_targets()]
#' @examples
#' \dontrun{
#' build_raster_quality_set_targets(
#'   sel_check_set = "chelsa_v21_raw",
#'   list_quality_checks = load_config_value(value = "quality_checks"),
#'   path_data_storage = path_data_storage,
#'   dir_tables = here::here("Outputs", "Tables")
#' )
#' }
build_raster_quality_set_targets <- function(
  sel_check_set,
  list_quality_checks,
  path_data_storage,
  dir_tables
) {
  list_check_sets <-
    list_quality_checks[["check_sets"]]

  assertthat::assert_that(
    rlang::is_string(sel_check_set),
    sel_check_set %in% base::names(list_check_sets),
    msg = "`sel_check_set` must be the name of one configured check set."
  )

  list_check_set <-
    list_check_sets[[sel_check_set]]

  list_grid <-
    list_quality_checks[["grids"]][[list_check_set[["grid"]]]]

  list_pairs <-
    list_check_set[["pairs"]] %||% base::list()

  flag_pairs <-
    base::length(list_pairs) > 0L

  # Target names of the set; symbols where commands refer to them
  vec_name <-
    c(
      expected_files = "expected_files",
      file_inventory = "file_inventory",
      block_extent = "block_extent",
      band_summary = "band_summary",
      expected_pairs = "expected_pairs",
      pair_comparison = "pair_comparison",
      findings_files = "findings_files",
      findings_bands = "findings_bands",
      findings_pairs = "findings_pairs",
      findings = "findings",
      file_summary = "file_summary",
      tables = "tables"
    ) |>
    purrr::map_chr(.f = ~ stringr::str_c(sel_check_set, "_", .x))

  list_sym <-
    purrr::map(.x = vec_name, .f = ~ rlang::sym(.x))

  expr_server_sizes <-
    if (
      isTRUE(list_check_set[["check_server_size"]])
    ) {
      rlang::sym("data_raster_server_sizes")
    } else {
      NULL
    }

  expr_pair_comparison <-
    if (
      isTRUE(flag_pairs)
    ) {
      list_sym[["pair_comparison"]]
    } else {
      NULL
    }

  list_targets_files <-
    list(
      targets::tar_target_raw(
        name = vec_name[["expected_files"]],
        command = rlang::expr(
          build_raster_quality_expected_files(
            list_check_set = !!list_check_set,
            list_grid = !!list_grid,
            path_data_storage = !!path_data_storage,
            data_server_sizes = !!expr_server_sizes
          )
        )
      ),
      # Reads only file sizes and modification times, on every run, so a
      #   replaced file reruns its own branches
      targets::tar_target_raw(
        name = vec_name[["file_inventory"]],
        command = rlang::expr(
          load_raster_file_inventory(
            data_expected_files = dplyr::select(
              !!list_sym[["expected_files"]],
              "product", "variable", "age", "file_name", "path_file"
            )
          )
        ),
        cue = targets::tar_cue(mode = "always")
      ),
      targets::tar_target_raw(
        name = vec_name[["block_extent"]],
        command = rlang::expr(
          compute_tiff_block_extent(
            path_file = purrr::chuck(
              !!list_sym[["file_inventory"]],
              "path_file"
            )
          )
        ),
        pattern = rlang::expr(map(!!list_sym[["file_inventory"]]))
      ),
      targets::tar_target_raw(
        name = vec_name[["band_summary"]],
        command = rlang::expr(
          compute_raster_band_summary(
            path_file = purrr::chuck(
              !!list_sym[["file_inventory"]],
              "path_file"
            ),
            band_width = !!list_quality_checks[["band_width"]],
            vec_type_limit_values = raster_quality_type_limit_values,
            n_rows_block = !!list_quality_checks[["n_rows_block"]],
            value_tolerance = !!list_quality_checks[["value_tolerance"]]
          )
        ),
        pattern = rlang::expr(map(!!list_sym[["file_inventory"]]))
      )
    )

  list_targets_pairs <-
    if (
      isTRUE(flag_pairs)
    ) {
      list(
        targets::tar_target_raw(
          name = vec_name[["expected_pairs"]],
          command = rlang::expr(
            build_raster_quality_expected_pairs(
              data_expected_files = !!list_sym[["file_inventory"]],
              list_pairs = !!list_pairs
            )
          )
        ),
        targets::tar_target_raw(
          name = vec_name[["pair_comparison"]],
          command = rlang::expr(
            compute_raster_pair_comparison(
              path_file = purrr::chuck(
                !!list_sym[["expected_pairs"]],
                "path_file"
              ),
              path_file_reference = purrr::chuck(
                !!list_sym[["expected_pairs"]],
                "path_file_reference"
              ),
              band_width = !!list_quality_checks[["band_width"]],
              n_rows_block = !!list_quality_checks[["n_rows_block"]],
              value_tolerance = !!list_quality_checks[["value_tolerance"]]
            )
          ),
          pattern = rlang::expr(map(!!list_sym[["expected_pairs"]]))
        )
      )
    } else {
      list()
    }

  list_targets_findings_files <-
    list(
      targets::tar_target_raw(
        name = vec_name[["findings_files"]],
        command = rlang::expr(
          diagnose_raster_files(
            data_expected_files = !!list_sym[["expected_files"]],
            data_band_summary = !!list_sym[["band_summary"]],
            data_block_extent = !!list_sym[["block_extent"]],
            data_allow_list = raster_quality_allow_list,
            extent_tolerance = !!list_quality_checks[["extent_tolerance"]]
          )
        )
      ),
      targets::tar_target_raw(
        name = vec_name[["findings_bands"]],
        command = rlang::expr(
          diagnose_raster_bands(
            data_expected_files = !!list_sym[["expected_files"]],
            data_band_summary = !!list_sym[["band_summary"]],
            data_thresholds = raster_quality_thresholds,
            data_allow_list = raster_quality_allow_list,
            vec_type_limit_values = raster_quality_type_limit_values,
            value_tolerance = !!list_quality_checks[["value_tolerance"]]
          )
        )
      )
    )

  list_targets_findings_pairs <-
    if (
      isTRUE(flag_pairs)
    ) {
      list(
        targets::tar_target_raw(
          name = vec_name[["findings_pairs"]],
          command = rlang::expr(
            diagnose_raster_pairs(
              data_expected_pairs = !!list_sym[["expected_pairs"]],
              data_pair_comparison = !!list_sym[["pair_comparison"]],
              data_thresholds = raster_quality_thresholds,
              data_allow_list = raster_quality_allow_list
            )
          )
        )
      )
    } else {
      list()
    }

  list_sym_findings <-
    list_sym[
      c("findings_files", "findings_bands", "findings_pairs")[
        c(TRUE, TRUE, flag_pairs)
      ]
    ]

  list_targets_outputs <-
    list(
      targets::tar_target_raw(
        name = vec_name[["findings"]],
        command = rlang::expr(
          dplyr::bind_rows(!!!base::unname(list_sym_findings))
        )
      ),
      targets::tar_target_raw(
        name = vec_name[["file_summary"]],
        command = rlang::expr(
          summarise_raster_quality(
            data_expected_files = !!list_sym[["expected_files"]],
            data_findings = !!list_sym[["findings"]]
          )
        )
      ),
      targets::tar_target_raw(
        name = vec_name[["tables"]],
        command = rlang::expr(
          save_raster_quality_tables(
            data_band_summary = !!list_sym[["band_summary"]],
            data_findings = !!list_sym[["findings"]],
            data_file_summary = !!list_sym[["file_summary"]],
            dir_output = !!dir_tables,
            table_prefix = !!stringr::str_c("raster_quality_", sel_check_set),
            data_pair_comparison = !!expr_pair_comparison
          )
        ),
        format = "file"
      )
    )

  res <-
    c(
      list_targets_files,
      list_targets_pairs,
      list_targets_findings_files,
      list_targets_findings_pairs,
      list_targets_outputs
    )

  return(res)
}

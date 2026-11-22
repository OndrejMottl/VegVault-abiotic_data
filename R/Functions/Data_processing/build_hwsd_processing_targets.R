#' @title Build the processing targets of the HWSD soil map
#' @description
#' Declare the `{targets}` that import the extracted HWSD files, the class
#' tables and the CHELSA V2.1 template from the download stage, give every
#' soil unit the class of its dominant component, and turn the soil-unit
#' raster into the soil-group raster on the processed CHELSA grid: classes
#' at full resolution, resampled to the CHELSA grid, aggregated by the
#' mode, and the classes that are not a soil set to missing.
#' @param list_data_processing
#' The `data_processing` section of the configuration: `aggregation_factor`
#' and `hwsd` with `store_acquisition` (store name of the HWSD download
#' pipeline), `store_template`, `template_source` and `template_variable`
#' (store name, source and variable of the download pipeline whose raw grid
#' the soil map takes: CHELSA V2.1 bio1), `dir` (relative to the data
#' root), `soil_group_codes`,
#' `non_soil_codes` and `code_unlisted` (as in
#' `prepare_hwsd_soil_groups()`).
#' @param path_data_storage
#' Character scalar. The data root.
#' @param dir_tables
#' Character scalar. Folder of the tables.
#' @param runner_acquisition
#' Character scalar. Runner of the download stage, named when a store is
#' not ready. Default `"R/Analyses/01_Data_acquisition/00_run.R"`.
#' @return
#' A list of target objects: the imports (`fingerprint_hwsd_acquisition`,
#' `hwsd_extracted_files`, `hwsd_class_tables`,
#' `fingerprint_hwsd_template`, `hwsd_template_raw_files`),
#' `hwsd_soil_units`, `hwsd_soil_groups`, `hwsd_class_raster`,
#' `hwsd_class_raster_resampled`, `hwsd_class_raster_aggregated`,
#' `hwsd_soil_summary` and the public `hwsd_soil_raster`.
#' @details
#' Every class that is not a soil group (codes above the largest soil-group
#' code) becomes missing only after the mode, so a cell whose dominant
#' class is not a soil gets no value, while a cell with a small lake keeps
#' its soil group.
#' @seealso [load_hwsd_soil_units()], [prepare_hwsd_soil_groups()],
#'   [save_classified_raster()], [save_resampled_raster()],
#'   [save_aggregated_raster()], [save_hwsd_soil_summary()]
#' @examples
#' \dontrun{
#' build_hwsd_processing_targets(
#'   list_data_processing = load_config_value(value = "data_processing"),
#'   path_data_storage = path_data_storage,
#'   dir_tables = here::here("Outputs", "Tables")
#' )
#' }
build_hwsd_processing_targets <- function(
  list_data_processing,
  path_data_storage,
  dir_tables,
  runner_acquisition = "R/Analyses/01_Data_acquisition/00_run.R"
) {
  assertthat::assert_that(
    base::is.list(list_data_processing),
    base::is.list(list_data_processing[["hwsd"]]),
    msg = "`list_data_processing` must be a list with `hwsd`."
  )

  aggregation_factor <-
    list_data_processing[["aggregation_factor"]]

  assertthat::assert_that(
    base::is.numeric(aggregation_factor),
    base::length(aggregation_factor) == 1L,
    base::is.finite(aggregation_factor),
    aggregation_factor >= 2,
    aggregation_factor == base::round(aggregation_factor),
    msg = "`aggregation_factor` must be a whole number of at least 2."
  )

  list_hwsd <-
    list_data_processing[["hwsd"]]

  purrr::walk(
    .x = c(
      "store_acquisition", "store_template", "template_source",
      "template_variable", "dir"
    ),
    .f = ~ assertthat::assert_that(
      rlang::is_string(list_hwsd[[.x]]) && base::nzchar(list_hwsd[[.x]]),
      msg = stringr::str_c("`hwsd` needs `", .x, "` as one string.")
    )
  )

  vec_soil_codes <-
    base::unlist(list_hwsd[["soil_group_codes"]])

  vec_non_soil_codes <-
    base::unlist(list_hwsd[["non_soil_codes"]])

  assertthat::assert_that(
    rlang::is_named(vec_soil_codes),
    rlang::is_named(vec_non_soil_codes),
    base::is.numeric(vec_soil_codes),
    base::is.numeric(vec_non_soil_codes),
    msg = stringr::str_c(
      "`hwsd` needs `soil_group_codes` and `non_soil_codes` as named ",
      "codes."
    )
  )

  code_unlisted <-
    list_hwsd[["code_unlisted"]]

  assertthat::assert_that(
    base::is.numeric(code_unlisted),
    base::length(code_unlisted) == 1L,
    code_unlisted > base::max(c(vec_soil_codes, vec_non_soil_codes)),
    msg = "`code_unlisted` of `hwsd` must be one number above all codes."
  )

  assertthat::assert_that(
    rlang::is_string(path_data_storage),
    rlang::is_string(dir_tables),
    rlang::is_string(runner_acquisition),
    msg = stringr::str_c(
      "`path_data_storage`, `dir_tables` and `runner_acquisition` must be ",
      "single strings."
    )
  )

  store_hwsd <-
    resolve_pipeline_store_path(
      path_data_storage = path_data_storage,
      store_relative_path = list_hwsd[["store_acquisition"]]
    )

  store_template <-
    resolve_pipeline_store_path(
      path_data_storage = path_data_storage,
      store_relative_path = list_hwsd[["store_template"]]
    )

  name_template_raw <-
    stringr::str_c(list_hwsd[["template_source"]], "_raw_files")

  dir_hwsd <-
    base::file.path(path_data_storage, list_hwsd[["dir"]])

  dir_intermediate <-
    base::file.path(dir_hwsd, "Intermediate")

  # Soil groups keep their code; every other class becomes missing
  data_classes_soil <-
    tibble::tibble(
      from = base::as.numeric(vec_soil_codes),
      to = base::as.numeric(vec_soil_codes)
    )

  list_targets_import <-
    list(
      # Reruns the imports whenever the HWSD download store changed
      targets::tar_target_raw(
        name = "fingerprint_hwsd_acquisition",
        command = rlang::expr(
          compute_target_store_fingerprint(
            store = !!store_hwsd,
            target_names = c("hwsd_extracted_files", "hwsd_class_tables"),
            runner = !!runner_acquisition
          )
        ),
        cue = targets::tar_cue(mode = "always")
      ),
      # The extracted HWSD raster and database
      targets::tar_target_raw(
        name = "hwsd_extracted_files",
        command = rlang::expr(
          load_target_store_value(
            store = !!store_hwsd,
            target_name = "hwsd_extracted_files",
            runner = !!runner_acquisition
          )
        ),
        deps = c("fingerprint_hwsd_acquisition", "load_target_store_value")
      ),
      # The tracked class tables
      targets::tar_target_raw(
        name = "hwsd_class_tables",
        command = rlang::expr(
          load_target_store_value(
            store = !!store_hwsd,
            target_name = "hwsd_class_tables",
            runner = !!runner_acquisition
          )
        ),
        deps = c("fingerprint_hwsd_acquisition", "load_target_store_value")
      ),
      # Reruns the template import whenever its download store changed
      targets::tar_target_raw(
        name = "fingerprint_hwsd_template",
        command = rlang::expr(
          compute_target_store_fingerprint(
            store = !!store_template,
            target_names = !!name_template_raw,
            runner = !!runner_acquisition
          )
        ),
        cue = targets::tar_cue(mode = "always")
      ),
      # The raw files of the source whose grid the soil map takes
      targets::tar_target_raw(
        name = "hwsd_template_raw_files",
        command = rlang::expr(
          load_target_store_value(
            store = !!store_template,
            target_name = !!name_template_raw,
            runner = !!runner_acquisition
          )
        ),
        deps = c("fingerprint_hwsd_template", "load_target_store_value")
      )
    )

  list_targets_classes <-
    list(
      targets::tar_target_raw(
        name = "hwsd_soil_units",
        command = rlang::expr(
          load_hwsd_soil_units(vec_paths = hwsd_class_tables)
        )
      ),
      # The class of the dominant component of every soil unit
      targets::tar_target_raw(
        name = "hwsd_soil_groups",
        command = rlang::expr(
          prepare_hwsd_soil_groups(
            data_soil_units = hwsd_soil_units,
            list_soil_group_codes = !!list_hwsd[["soil_group_codes"]],
            list_non_soil_codes = !!list_hwsd[["non_soil_codes"]],
            code_unlisted = !!code_unlisted
          )
        )
      ),
      # Classes at the full HWSD resolution
      targets::tar_target_raw(
        name = "hwsd_class_raster",
        command = rlang::expr(
          save_classified_raster(
            path_input = stringr::str_subset(
              string = hwsd_extracted_files,
              pattern = "[.]bil$"
            ),
            path_output = !!base::file.path(
              dir_intermediate,
              "hwsd_classes.tif"
            ),
            data_classes = dplyr::select(
              hwsd_soil_groups,
              from = "HWSD2_SMU_ID",
              to = "class_code"
            ),
            datatype = "INT1U"
          )
        ),
        format = "file"
      ),
      # Classes on the raw CHELSA grid (nearest cell), as in v1
      targets::tar_target_raw(
        name = "hwsd_class_raster_resampled",
        command = rlang::expr(
          save_resampled_raster(
            path_input = hwsd_class_raster,
            path_template = dplyr::pull(
              dplyr::filter(
                hwsd_template_raw_files,
                .data[["variable"]] == !!list_hwsd[["template_variable"]]
              ),
              "path_file"
            ),
            path_output = !!base::file.path(
              dir_intermediate,
              "hwsd_classes_resampled.tif"
            ),
            method = "near",
            datatype = "INT1U"
          )
        ),
        format = "file"
      ),
      # The dominant class of every processed cell
      targets::tar_target_raw(
        name = "hwsd_class_raster_aggregated",
        command = rlang::expr(
          save_aggregated_raster(
            path_input = hwsd_class_raster_resampled,
            path_output = !!base::file.path(
              dir_intermediate,
              "hwsd_classes_aggregated.tif"
            ),
            aggregation_factor = !!aggregation_factor,
            aggregation_fun = "modal",
            datatype = "INT1U"
          )
        ),
        format = "file"
      ),
      # Cells and soil units per class, for the release notes
      targets::tar_target_raw(
        name = "hwsd_soil_summary",
        command = rlang::expr(
          save_hwsd_soil_summary(
            path_class_raster = hwsd_class_raster_aggregated,
            data_soil_groups = hwsd_soil_groups,
            path_output = !!base::file.path(
              dir_tables,
              "processing_hwsd_soil_summary.csv"
            )
          )
        ),
        format = "file"
      ),
      # Public: the soil-group codes; a dominant class that is not a soil
      #   becomes missing
      targets::tar_target_raw(
        name = "hwsd_soil_raster",
        command = rlang::expr(
          save_classified_raster(
            path_input = hwsd_class_raster_aggregated,
            path_output = !!base::file.path(dir_hwsd, "hwsd_soil_groups.tif"),
            data_classes = !!data_classes_soil,
            datatype = "INT1U"
          )
        ),
        format = "file"
      )
    )

  res <-
    c(
      list_targets_import,
      list_targets_classes
    )

  return(res)
}

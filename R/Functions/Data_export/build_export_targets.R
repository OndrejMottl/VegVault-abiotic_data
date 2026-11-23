#' @title Build the targets of the export stage
#' @description
#' Declare the `{targets}` that save the cell tables of the extraction
#' stage as the artefacts of the VegVault producer contracts, check them
#' against the contracts and write the checksum manifest, the summary and
#' the legend of the soil-group codes.
#' @param list_data_export
#' The `data_export` section of the configuration: `dir_contracts` (the
#' project's copy of the contracts, relative to `path_project`),
#' `dir_data_contract` (the folder that the contracts' directories start
#' with), `dir_data` and `dir_tables` (the export folders, relative to
#' `path_project`), `flag_replace_previous` (logical), `products` (a named
#' list; each product has `store_extraction` and `target_extraction`, the
#' store and public target of its extraction pipeline, and `contract`, the
#' contract id), `quality_check` (`store` and `target`, the store and
#' public target of the quality check that must have passed) and,
#' optionally, `soil_group_names_missing` (names of soil groups that the
#' HWSD lookup lacks, as in `save_hwsd_soil_legend()`).
#' @param path_data_storage
#' Character scalar. The data root, which holds the target stores.
#' @param path_project
#' Character scalar. The project root.
#' @param list_soil_group_codes
#' Named list of the soil-group codes by WRB code, as in
#' `save_hwsd_soil_legend()`.
#' @param path_wrb_lookup
#' Character scalar. The HWSD lookup table `d_wrb2.csv`.
#' @param runner_extraction
#' Character scalar. Runner of the extraction stage, named when one of its
#' stores is not ready. Default `"R/Analyses/04_Data_extraction/00_run.R"`.
#' @param runner_quality_checks
#' Character scalar. Runner of the quality checks, named when their store
#' is not ready or the check failed. Default
#' `"R/Analyses/03_Quality_checks/00_run.R"`.
#' @return
#' A list of target objects. Shared: `fingerprint_quality_checks`, the
#' imported quality-check target (named as in `quality_check$target`),
#' `path_contract_copy_record`, `contract_copy_record` and, per contract
#' id `<id>`, `path_contract_<id>` and `contract_<id>`, then
#' `path_hwsd_wrb_lookup`. Per product `<p>`:
#' `fingerprint_<p>_extracted_files`, `<p>_extracted_files`,
#' `<p>_export_groups` (one group per variable), `<p>_export_cell_tables`
#' and `<p>_export_files` (file targets, one branch per variable),
#' `<p>_export_folder` and the public `<p>_export_validated`, the paths of
#' the files once they pass their contract. Public tables:
#' `path_export_manifest`, `path_export_summary` and
#' `path_hwsd_soil_legend`.
#' @details
#' The files of each contract go to the contract's `producer: directory`
#' with `dir_data_contract` replaced by `dir_data`
#' (`resolve_export_folder()`). The tables go to `dir_tables` as
#' `export_manifest.csv`, `export_summary.csv` and
#' `hwsd_soil_group_legend.csv`. The cell tables are file targets, so a
#' changed cell table reruns the export of its variable. Every export
#' waits for the imported quality check, so a failed or missing check
#' stops the stage with a message that names its runner. The folder check
#' reruns on every run, so a file added to a contract folder later is
#' found. The configuration is validated, and the contracts are read for
#' their folders, while declaring.
#' @seealso [save_cell_table_export()], [validate_export_folder()],
#'   [validate_export_artefact()], [save_export_manifest()]
#' @examples
#' \dontrun{
#' build_export_targets(
#'   list_data_export = load_config_value(value = "data_export"),
#'   path_data_storage = path_data_storage,
#'   path_project = here::here(),
#'   list_soil_group_codes = list_soil_group_codes,
#'   path_wrb_lookup = path_wrb_lookup
#' )
#' }
build_export_targets <- function(
  list_data_export,
  path_data_storage,
  path_project,
  list_soil_group_codes,
  path_wrb_lookup,
  runner_extraction = "R/Analyses/04_Data_extraction/00_run.R",
  runner_quality_checks = "R/Analyses/03_Quality_checks/00_run.R"
) {
  assertthat::assert_that(
    base::is.list(list_data_export),
    msg = "`list_data_export` must be a list."
  )

  purrr::walk(
    .x = c("dir_contracts", "dir_data_contract", "dir_data", "dir_tables"),
    .f = ~ assertthat::assert_that(
      rlang::is_string(list_data_export[[.x]]) &&
        base::nzchar(list_data_export[[.x]]),
      msg = stringr::str_c("`data_export` needs `", .x, "` as one string.")
    )
  )

  assertthat::assert_that(
    rlang::is_bool(list_data_export[["flag_replace_previous"]]),
    msg = "`data_export` needs `flag_replace_previous` as TRUE or FALSE."
  )

  list_products <-
    list_data_export[["products"]]

  assertthat::assert_that(
    base::is.list(list_products),
    base::length(list_products) > 0L,
    rlang::is_named(list_products),
    msg = "`data_export` needs `products` as a named list."
  )

  vec_products <-
    base::names(list_products)

  # Product names become parts of target names
  vec_products_invalid <-
    vec_products[!stringr::str_detect(vec_products, "^[a-z][a-z0-9_]*$")]

  assertthat::assert_that(
    base::length(vec_products_invalid) == 0L,
    msg = stringr::str_c(
      "Product names must be lower-case letters, digits and underscores: ",
      stringr::str_c(vec_products_invalid, collapse = ", ")
    )
  )

  purrr::iwalk(
    .x = list_products,
    .f = ~ {
      list_product <- .x

      sel_product <- .y

      purrr::walk(
        .x = c("store_extraction", "target_extraction", "contract"),
        .f = ~ assertthat::assert_that(
          rlang::is_string(list_product[[.x]]) &&
            base::nzchar(list_product[[.x]]),
          msg = stringr::str_c(
            "Product `", sel_product, "` needs `", .x, "` as one string."
          )
        )
      )
    }
  )

  vec_contract_ids <-
    purrr::map_chr(
      .x = list_products,
      .f = ~ purrr::chuck(.x, "contract")
    )

  assertthat::assert_that(
    !base::anyDuplicated(vec_contract_ids),
    msg = "Every product needs its own contract, or their folders clash."
  )

  list_quality_check <-
    list_data_export[["quality_check"]]

  assertthat::assert_that(
    base::is.list(list_quality_check),
    rlang::is_string(list_quality_check[["store"]]),
    rlang::is_string(list_quality_check[["target"]]),
    msg = "`data_export` needs `quality_check` with `store` and `target`."
  )

  assertthat::assert_that(
    rlang::is_string(path_data_storage),
    rlang::is_string(path_project) && base::dir.exists(path_project),
    rlang::is_string(path_wrb_lookup),
    rlang::is_string(runner_extraction),
    rlang::is_string(runner_quality_checks),
    msg = stringr::str_c(
      "`path_data_storage`, `path_project` (an existing folder),",
      " `path_wrb_lookup` and the runners must be single strings."
    )
  )

  assertthat::assert_that(
    base::is.list(list_soil_group_codes),
    rlang::is_named(list_soil_group_codes),
    msg = "`list_soil_group_codes` must be a named list."
  )

  # Optional: names of soil groups that the HWSD lookup lacks
  list_names_missing <-
    if (
      base::is.null(list_data_export[["soil_group_names_missing"]])
    ) {
      base::list()
    } else {
      list_data_export[["soil_group_names_missing"]]
    }

  assertthat::assert_that(
    base::is.list(list_names_missing),
    base::length(list_names_missing) == 0L ||
      rlang::is_named(list_names_missing),
    msg = stringr::str_c(
      "`data_export` needs `soil_group_names_missing` as a list of names",
      " named by WRB code, or none."
    )
  )

  dir_contracts <-
    base::file.path(path_project, list_data_export[["dir_contracts"]])

  list_paths_contracts <-
    vec_contract_ids |>
    rlang::set_names() |>
    purrr::map(
      .f = ~ base::file.path(dir_contracts, stringr::str_c(.x, ".yml"))
    )

  vec_contracts_missing <-
    vec_contract_ids[
      !base::file.exists(base::unlist(list_paths_contracts))
    ]

  assertthat::assert_that(
    base::length(vec_contracts_missing) == 0L,
    msg = stringr::str_c(
      "No contract file in `", dir_contracts, "` for: ",
      stringr::str_c(vec_contracts_missing, collapse = ", ")
    )
  )

  # The folders of the contracts, placed under the configured export folder
  list_dirs_output <-
    list_paths_contracts |>
    purrr::map(
      .f = ~ base::file.path(
        path_project,
        resolve_export_folder(
          dir_contract = load_producer_contract(path_contract = .x) |>
            purrr::chuck("producer", "directory"),
          dir_data_contract = list_data_export[["dir_data_contract"]],
          dir_data = list_data_export[["dir_data"]]
        )
      )
    )

  dir_tables <-
    base::file.path(path_project, list_data_export[["dir_tables"]])

  flag_replace_previous <-
    list_data_export[["flag_replace_previous"]]

  store_quality_checks <-
    resolve_pipeline_store_path(
      path_data_storage = path_data_storage,
      store_relative_path = list_quality_check[["store"]]
    )

  name_quality_check <-
    list_quality_check[["target"]]

  # Shared targets: the quality check, the contract copy and the contracts
  list_targets_shared <-
    list(
      # Reruns the import whenever the quality-check store changed
      targets::tar_target_raw(
        name = "fingerprint_quality_checks",
        command = rlang::expr(
          compute_target_store_fingerprint(
            store = !!store_quality_checks,
            target_names = !!name_quality_check,
            runner = !!runner_quality_checks
          )
        ),
        cue = targets::tar_cue(mode = "always")
      ),
      # The passed quality check; every export waits for it
      targets::tar_target_raw(
        name = name_quality_check,
        command = rlang::expr(
          load_target_store_value(
            store = !!store_quality_checks,
            target_name = !!name_quality_check,
            runner = !!runner_quality_checks
          )
        ),
        deps = c("fingerprint_quality_checks", "load_target_store_value")
      ),
      # The record of the contract copy, tracked as a file
      targets::tar_target_raw(
        name = "path_contract_copy_record",
        command = base::file.path(dir_contracts, "contract_copy.yml"),
        format = "file"
      ),
      # The VegVault commit of the copy, for the manifest
      targets::tar_target_raw(
        name = "contract_copy_record",
        command = rlang::expr(
          load_contract_copy_record(path_producer = !!path_project)
        ),
        deps = c("path_contract_copy_record", "load_contract_copy_record")
      )
    )

  # Per contract: the file, tracked so that a new copy reruns the checks,
  #   and the loaded contract
  list_targets_contracts <-
    list_paths_contracts |>
    purrr::imap(
      .f = ~ list(
        targets::tar_target_raw(
          name = stringr::str_c("path_contract_", .y),
          command = .x,
          format = "file"
        ),
        targets::tar_target_raw(
          name = stringr::str_c("contract_", .y),
          command = rlang::expr(
            load_producer_contract(
              path_contract = !!rlang::sym(
                stringr::str_c("path_contract_", .y)
              )
            )
          )
        )
      )
    ) |>
    purrr::list_flatten() |>
    base::unname()

  # Per product: import, one group per variable, cell tables as files,
  #   export files, folder check and contract check
  list_targets_products <-
    vec_products |>
    purrr::map(
      .f = ~ {
        sel_product <- .x

        list_product <-
          purrr::chuck(list_products, sel_product)

        sel_contract_id <-
          purrr::chuck(list_product, "contract")

        store_extraction <-
          resolve_pipeline_store_path(
            path_data_storage = path_data_storage,
            store_relative_path = purrr::chuck(
              list_product,
              "store_extraction"
            )
          )

        target_extraction <-
          purrr::chuck(list_product, "target_extraction")

        list_names <-
          base::list(
            fingerprint = stringr::str_c(
              "fingerprint_", sel_product, "_extracted_files"
            ),
            extracted = stringr::str_c(sel_product, "_extracted_files"),
            groups = stringr::str_c(sel_product, "_export_groups"),
            cell_tables = stringr::str_c(sel_product, "_export_cell_tables"),
            files = stringr::str_c(sel_product, "_export_files"),
            folder = stringr::str_c(sel_product, "_export_folder"),
            validated = stringr::str_c(sel_product, "_export_validated")
          )

        list_syms <-
          purrr::map(.x = list_names, .f = ~ rlang::sym(.x))

        sym_contract <-
          rlang::sym(stringr::str_c("contract_", sel_contract_id))

        list(
          # Reruns the import whenever the extraction store changed
          targets::tar_target_raw(
            name = purrr::chuck(list_names, "fingerprint"),
            command = rlang::expr(
              compute_target_store_fingerprint(
                store = !!store_extraction,
                target_names = !!target_extraction,
                runner = !!runner_extraction
              )
            ),
            cue = targets::tar_cue(mode = "always")
          ),
          # The cell tables of the extraction stage, one row per table
          targets::tar_target_raw(
            name = purrr::chuck(list_names, "extracted"),
            command = rlang::expr(
              load_target_store_value(
                store = !!store_extraction,
                target_name = !!target_extraction,
                runner = !!runner_extraction
              )
            ),
            deps = c(
              purrr::chuck(list_names, "fingerprint"),
              "load_target_store_value"
            )
          ),
          # One group per variable; waits for the passed quality check
          targets::tar_target_raw(
            name = purrr::chuck(list_names, "groups"),
            command = rlang::expr(
              targets::tar_group(
                dplyr::group_by(
                  !!purrr::chuck(list_syms, "extracted"),
                  .data[["variable"]]
                )
              )
            ),
            deps = c(
              purrr::chuck(list_names, "extracted"),
              name_quality_check
            ),
            iteration = "group"
          ),
          # The cell tables of each variable as files, so a changed cell
          #   table reruns the export of its variable
          targets::tar_target_raw(
            name = purrr::chuck(list_names, "cell_tables"),
            command = rlang::expr(
              purrr::chuck(!!purrr::chuck(list_syms, "groups"), "path_file")
            ),
            pattern = rlang::expr(map(!!purrr::chuck(list_syms, "groups"))),
            format = "file"
          ),
          # The export files of each variable, under the contract's names
          targets::tar_target_raw(
            name = purrr::chuck(list_names, "files"),
            command = rlang::expr(
              save_cell_table_export(
                data_files = !!purrr::chuck(list_syms, "groups"),
                contract = !!sym_contract,
                dir_output = !!purrr::chuck(list_dirs_output, sel_contract_id),
                flag_replace_previous = !!flag_replace_previous
              )
            ),
            pattern = rlang::expr(
              map(
                !!purrr::chuck(list_syms, "groups"),
                !!purrr::chuck(list_syms, "cell_tables")
              )
            ),
            format = "file"
          ),
          # The contract folder holds exactly these files; checked on every
          #   run, so a file added later is found
          targets::tar_target_raw(
            name = purrr::chuck(list_names, "folder"),
            command = rlang::expr(
              validate_export_folder(
                vec_paths_export = !!purrr::chuck(list_syms, "files")
              )
            ),
            cue = targets::tar_cue(mode = "always")
          ),
          # Public: the export files once they pass their contract
          targets::tar_target_raw(
            name = purrr::chuck(list_names, "validated"),
            command = rlang::expr(
              validate_export_artefact(
                vec_paths_export = !!purrr::chuck(list_syms, "folder"),
                contract = !!sym_contract
              )
            )
          )
        )
      }
    ) |>
    purrr::list_flatten()

  # The validated files and the contracts, named by contract id
  list_syms_validated <-
    vec_products |>
    purrr::map(
      .f = ~ rlang::sym(stringr::str_c(.x, "_export_validated"))
    ) |>
    rlang::set_names(vec_contract_ids)

  list_syms_contracts <-
    vec_contract_ids |>
    purrr::map(
      .f = ~ rlang::sym(stringr::str_c("contract_", .x))
    ) |>
    rlang::set_names(vec_contract_ids)

  list_targets_tables <-
    list(
      # Public: one row per exported file with its checksum
      targets::tar_target_raw(
        name = "path_export_manifest",
        command = rlang::expr(
          save_export_manifest(
            list_export_paths = list(!!!list_syms_validated),
            list_contracts = list(!!!list_syms_contracts),
            vegvault_commit = purrr::chuck(
              contract_copy_record,
              "vegvault_commit"
            ),
            path_root = !!path_project,
            path_output = !!base::file.path(dir_tables, "export_manifest.csv")
          )
        ),
        format = "file"
      ),
      # Public: the counts and value ranges per variable
      targets::tar_target_raw(
        name = "path_export_summary",
        command = rlang::expr(
          save_export_summary(
            list_export_paths = list(!!!list_syms_validated),
            list_contracts = list(!!!list_syms_contracts),
            path_output = !!base::file.path(dir_tables, "export_summary.csv")
          )
        ),
        format = "file"
      ),
      # The HWSD lookup of the soil-group names, tracked as a file
      targets::tar_target_raw(
        name = "path_hwsd_wrb_lookup",
        command = path_wrb_lookup,
        format = "file"
      ),
      # Public: the legend of the soil-group codes
      targets::tar_target_raw(
        name = "path_hwsd_soil_legend",
        command = rlang::expr(
          save_hwsd_soil_legend(
            list_soil_group_codes = !!list_soil_group_codes,
            path_wrb_lookup = path_hwsd_wrb_lookup,
            path_output = !!base::file.path(
              dir_tables,
              "hwsd_soil_group_legend.csv"
            ),
            list_names_missing = !!list_names_missing
          )
        ),
        format = "file"
      )
    )

  res <-
    c(
      list_targets_shared,
      list_targets_contracts,
      list_targets_products,
      list_targets_tables
    )

  return(res)
}

#' @title Validate exported files against their VegVault contract
#' @description
#' Check every file of an exported artefact with the contract validator of
#' the project's copy of the VegVault contracts.
#' @param vec_paths_export
#' Character vector. The paths of every file of the artefact.
#' @param contract
#' A contract as returned by `load_producer_contract()`.
#' @return
#' `vec_paths_export`, unchanged.
#' @details
#' The check is `validate_producer_artefact()`: file names and partitions,
#' object classes and row counts, columns, values, variable checks, keys
#' and rounded keys. For a contract with `variables`, the files are checked
#' one variable at a time (`vec_variables`), in the order of the contract's
#' variable table, so only one variable is in memory at once. A file whose
#' name does not match the contract and a variable without files stop the
#' function before any file is read. A violation stops it with an error of
#' class `producer_contract_error` that lists every violation of the
#' variable or artefact. The files are never changed.
#' @seealso [validate_export_folder()], [validate_producer_artefact()]
#' @examples
#' \dontrun{
#' validate_export_artefact(
#'   vec_paths_export = vec_paths_neoclimate,
#'   contract = contract_chelsa_neoclimate
#' )
#' }
validate_export_artefact <- function(
  vec_paths_export,
  contract
) {
  assertthat::assert_that(
    base::is.character(vec_paths_export),
    base::length(vec_paths_export) > 0L,
    !base::anyNA(vec_paths_export),
    base::all(base::file.exists(vec_paths_export)),
    msg = "`vec_paths_export` must be the paths of existing files."
  )

  assertthat::assert_that(
    base::is.list(contract),
    rlang::is_string(contract[["contract_id"]]),
    msg = "`contract` must be a contract from `load_producer_contract()`."
  )

  data_files <-
    build_producer_file_table(
      vec_file_names = base::basename(vec_paths_export),
      contract = contract
    ) |>
    dplyr::mutate(path_file = vec_paths_export)

  vec_names_foreign <-
    data_files |>
    dplyr::filter(!.data[["is_match"]]) |>
    dplyr::pull("file_name")

  if (
    base::length(vec_names_foreign) > 0L
  ) {
    cli::cli_abort(
      c(
        "Files whose names do not match the contract
        {.val {contract[['contract_id']]}}:",
        "x" = "{.file {vec_names_foreign}}"
      )
    )
  }

  if (
    base::is.null(contract[["variables"]])
  ) {
    validate_producer_artefact(
      contract = contract,
      path_artefact = vec_paths_export,
      verbose = FALSE
    )
  } else {
    vec_variables <-
      purrr::map_chr(
        .x = contract[["variables"]][["table"]],
        .f = ~ purrr::chuck(.x, "variable")
      )

    vec_variables_missing <-
      base::setdiff(vec_variables, dplyr::pull(data_files, "variable"))

    if (
      base::length(vec_variables_missing) > 0L
    ) {
      cli::cli_abort(
        c(
          "Variables of the contract {.val {contract[['contract_id']]}}
          without files:",
          "x" = "{.val {vec_variables_missing}}"
        )
      )
    }

    # One variable in memory at a time
    purrr::walk(
      .x = vec_variables,
      .f = ~ validate_producer_artefact(
        contract = contract,
        path_artefact = data_files |>
          dplyr::filter(.data[["variable"]] == .x) |>
          dplyr::pull("path_file"),
        vec_variables = .x,
        verbose = FALSE
      )
    )
  }

  res <- vec_paths_export

  return(res)
}

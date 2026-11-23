#' @title Save the checksum manifest of the export
#' @description
#' Write one row per exported file with its contract, variable, partition,
#' size, SHA-256 checksum and number of rows, so the files can be pinned
#' and checked after they are downloaded.
#' @param list_export_paths
#' Named list of character vectors: the paths of every file of each
#' exported artefact, named by contract id.
#' @param list_contracts
#' Named list of contracts as returned by `load_producer_contract()`, with
#' an entry for every name of `list_export_paths`.
#' @param vegvault_commit
#' Character scalar. The VegVault commit of the contract copy
#' (`vegvault_commit` of `Data/Contracts/contract_copy.yml`).
#' @param path_root
#' Character scalar. An existing folder that holds every file; paths are
#' recorded relative to it, with forward slashes.
#' @param path_output
#' Character scalar. The CSV file to write; its folder is created when
#' missing.
#' @return
#' Character scalar. `path_output`.
#' @details
#' The columns are `contract_id`, `contract_version`, `variable` (missing
#' for a contract without variables), `partition` (missing for a contract
#' that is not partitioned), `file_name`, `path`, `bytes`, `sha256`
#' (`tools::sha256sum()`), `n_rows` and `vegvault_commit`. The rows follow
#' the order of `list_export_paths`, then the file names. Missing values
#' are written as empty cells. Every file must exist, lie inside
#' `path_root` and match the file-name pattern of its contract; the files
#' are read only to count their rows.
#' @seealso [save_export_summary()]
#' @examples
#' \dontrun{
#' save_export_manifest(
#'   list_export_paths = list(chelsa_neoclimate = vec_paths_neoclimate),
#'   list_contracts = list(chelsa_neoclimate = contract_chelsa_neoclimate),
#'   vegvault_commit = "332fa0d3e62f15b9abd9bf1c97daa6c74d5d3062",
#'   path_root = here::here(),
#'   path_output = here::here("Outputs", "Tables", "export_manifest.csv")
#' )
#' }
save_export_manifest <- function(
  list_export_paths,
  list_contracts,
  vegvault_commit,
  path_root,
  path_output
) {
  assertthat::assert_that(
    base::is.list(list_export_paths),
    base::length(list_export_paths) > 0L,
    rlang::is_named(list_export_paths),
    !base::anyDuplicated(base::names(list_export_paths)),
    base::all(
      purrr::map_lgl(
        .x = list_export_paths,
        .f = ~ base::is.character(.x) && !base::anyNA(.x)
      )
    ),
    msg = stringr::str_c(
      "`list_export_paths` must be a list of file paths named by",
      " contract id."
    )
  )

  assertthat::assert_that(
    base::is.list(list_contracts),
    base::all(base::names(list_export_paths) %in% base::names(list_contracts)),
    msg = "`list_contracts` must hold a contract for every artefact."
  )

  assertthat::assert_that(
    rlang::is_string(vegvault_commit) &&
      !base::is.na(vegvault_commit) &&
      base::nzchar(vegvault_commit),
    msg = "`vegvault_commit` must be one commit id."
  )

  assertthat::assert_that(
    rlang::is_string(path_root) && base::dir.exists(path_root),
    msg = "`path_root` must be an existing folder."
  )

  assertthat::assert_that(
    rlang::is_string(path_output) && !base::is.na(path_output),
    msg = "`path_output` must be one file path."
  )

  vec_paths_all <-
    purrr::list_c(list_export_paths)

  vec_paths_missing <-
    purrr::discard(.x = vec_paths_all, .p = ~ base::file.exists(.x))

  assertthat::assert_that(
    base::length(vec_paths_missing) == 0L,
    msg = stringr::str_c(
      "These exported files do not exist: ",
      stringr::str_c(vec_paths_missing, collapse = ", ")
    )
  )

  path_root_normalised <-
    base::normalizePath(path_root, winslash = "/")

  # One row per file with the parts of its name
  data_files <-
    list_export_paths |>
    purrr::imap(
      .f = ~ build_producer_file_table(
        vec_file_names = base::basename(.x),
        contract = purrr::chuck(list_contracts, .y)
      ) |>
        dplyr::mutate(
          contract_id = .y,
          contract_version = purrr::chuck(
            list_contracts, .y, "contract_version"
          ),
          path_file = base::normalizePath(.x, winslash = "/")
        )
    ) |>
    purrr::list_rbind()

  vec_names_foreign <-
    data_files |>
    dplyr::filter(!.data[["is_match"]]) |>
    dplyr::pull("file_name")

  if (
    base::length(vec_names_foreign) > 0L
  ) {
    cli::cli_abort(
      c(
        "Files whose names do not match their contract:",
        "x" = "{.file {vec_names_foreign}}"
      )
    )
  }

  vec_paths_outside <-
    data_files |>
    dplyr::filter(
      !stringr::str_starts(
        .data[["path_file"]],
        stringr::fixed(stringr::str_c(path_root_normalised, "/"))
      )
    ) |>
    dplyr::pull("path_file")

  if (
    base::length(vec_paths_outside) > 0L
  ) {
    cli::cli_abort(
      c(
        "Files outside `path_root` ({.path {path_root_normalised}}):",
        "x" = "{.file {vec_paths_outside}}"
      )
    )
  }

  res_manifest <-
    data_files |>
    dplyr::mutate(
      order_contract = base::match(
        .data[["contract_id"]],
        base::names(list_export_paths)
      )
    ) |>
    dplyr::arrange(.data[["order_contract"]], .data[["file_name"]]) |>
    dplyr::mutate(
      partition = base::as.integer(.data[["partition"]]),
      path = stringr::str_sub(
        .data[["path_file"]],
        start = base::nchar(path_root_normalised) + 2L
      ),
      bytes = base::file.size(.data[["path_file"]]),
      sha256 = base::unname(tools::sha256sum(files = .data[["path_file"]])),
      n_rows = purrr::map_int(
        .x = .data[["path_file"]],
        .f = ~ base::nrow(qs2::qs_read(file = .x))
      ),
      vegvault_commit = vegvault_commit
    ) |>
    dplyr::select(
      "contract_id", "contract_version", "variable", "partition",
      "file_name", "path", "bytes", "sha256", "n_rows", "vegvault_commit"
    )

  base::dir.create(
    path = base::dirname(path_output),
    recursive = TRUE,
    showWarnings = FALSE
  )

  readr::write_csv(x = res_manifest, file = path_output, na = "")

  res <- path_output

  return(res)
}

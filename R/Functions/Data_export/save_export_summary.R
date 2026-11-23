#' @title Save the summary of the exported artefacts
#' @description
#' Write one row per variable of each exported artefact with its numbers
#' of files, rows, grid cells and time slices and its range of values, the
#' counts that the release notes report.
#' @param list_export_paths
#' Named list of character vectors: the paths of every file of each
#' exported artefact, named by contract id.
#' @param list_contracts
#' Named list of contracts as returned by `load_producer_contract()`, with
#' an entry for every name of `list_export_paths`.
#' @param path_output
#' Character scalar. The CSV file to write; its folder is created when
#' missing.
#' @return
#' Character scalar. `path_output`.
#' @details
#' The columns are `contract_id`, `variable` (missing for a contract
#' without variables), `n_files`, `n_rows`, `n_cells` (distinct pairs of
#' `long` and `lat`), `n_time_slices` (distinct values of `time_id`;
#' missing when the artefact has no `time_id`), `value_min` and
#' `value_max`. The rows follow the order of `list_export_paths`, then the
#' order of the contract's variable table. The files of one variable are
#' read together, one variable at a time. Every file must match the
#' file-name pattern of its contract, and every file must have the columns
#' `long`, `lat` and `value`. Missing values are written as empty cells.
#' @seealso [save_export_manifest()]
#' @examples
#' \dontrun{
#' save_export_summary(
#'   list_export_paths = list(chelsa_neoclimate = vec_paths_neoclimate),
#'   list_contracts = list(chelsa_neoclimate = contract_chelsa_neoclimate),
#'   path_output = here::here("Outputs", "Tables", "export_summary.csv")
#' )
#' }
save_export_summary <- function(
  list_export_paths,
  list_contracts,
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
    rlang::is_string(path_output) && !base::is.na(path_output),
    msg = "`path_output` must be one file path."
  )

  # One row per file with its contract variable, in the order of the
  #   artefacts and of the contract's variable table
  data_files <-
    list_export_paths |>
    purrr::imap(
      .f = ~ {
        vec_paths_contract <- .x

        sel_contract_id <- .y

        contract <-
          purrr::chuck(list_contracts, sel_contract_id)

        vec_variables_contract <-
          purrr::map_chr(
            .x = contract[["variables"]][["table"]],
            .f = ~ purrr::chuck(.x, "variable")
          )

        build_producer_file_table(
          vec_file_names = base::basename(vec_paths_contract),
          contract = contract
        ) |>
          dplyr::mutate(
            contract_id = sel_contract_id,
            path_file = vec_paths_contract,
            order_variable = base::match(
              .data[["variable"]],
              vec_variables_contract
            )
          )
      }
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

  data_groups <-
    data_files |>
    dplyr::mutate(
      order_contract = base::match(
        .data[["contract_id"]],
        base::names(list_export_paths)
      )
    ) |>
    dplyr::arrange(.data[["order_contract"]], .data[["order_variable"]]) |>
    dplyr::group_by(.data[["contract_id"]], .data[["variable"]]) |>
    dplyr::summarise(
      order_contract = dplyr::first(.data[["order_contract"]]),
      order_variable = dplyr::first(.data[["order_variable"]]),
      list_paths = base::list(.data[["path_file"]]),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data[["order_contract"]], .data[["order_variable"]])

  # The files of one variable are read together, one variable at a time
  res_summary <-
    data_groups |>
    dplyr::mutate(
      data_counts = purrr::map(
        .x = .data[["list_paths"]],
        .f = ~ {
          vec_paths_variable <- .x

          data_values <-
            purrr::map(
              .x = vec_paths_variable,
              .f = ~ qs2::qs_read(file = .x)
            ) |>
            dplyr::bind_rows()

          n_time_slices <-
            if (
              "time_id" %in% base::names(data_values)
            ) {
              dplyr::n_distinct(dplyr::pull(data_values, "time_id"))
            } else {
              NA_integer_
            }

          tibble::tibble(
            n_files = base::length(vec_paths_variable),
            n_rows = base::nrow(data_values),
            n_cells = dplyr::n_distinct(
              dplyr::pull(data_values, "long"),
              dplyr::pull(data_values, "lat")
            ),
            n_time_slices = n_time_slices,
            value_min = base::min(dplyr::pull(data_values, "value")),
            value_max = base::max(dplyr::pull(data_values, "value"))
          )
        }
      )
    ) |>
    tidyr::unnest("data_counts") |>
    dplyr::select(
      "contract_id", "variable", "n_files", "n_rows", "n_cells",
      "n_time_slices", "value_min", "value_max"
    )

  base::dir.create(
    path = base::dirname(path_output),
    recursive = TRUE,
    showWarnings = FALSE
  )

  readr::write_csv(x = res_summary, file = path_output, na = "")

  res <- path_output

  return(res)
}

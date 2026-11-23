#' @title Save the cell tables of one variable as its export files
#' @description
#' Read the cell tables of one variable, join them from the youngest to
#' the oldest age and save them under the file names of a VegVault
#' producer contract: one file, or the contract's number of batches for a
#' partitioned contract.
#' @param data_files
#' Data frame with one row per cell table of one variable and the columns
#' `variable`, `age` and `path_file` (for example one variable of the
#' public table of the extraction stage).
#' @param contract
#' A contract as returned by `load_producer_contract()`.
#' @param dir_output
#' Character scalar. The folder to save into; created when missing.
#' @param date_save
#' A `Date`, written into the file names. Default: today.
#' @param flag_replace_previous
#' Logical. Passed to `save_export_artefact()`. Default `FALSE`.
#' @return
#' Character vector. The paths of the saved files, in partition order.
#' @details
#' The cell tables are read with `qs2::qs_read()` and joined with
#' `dplyr::bind_rows()` in increasing age, each table in its own row
#' order. For a contract with the partition scheme `"integer"`, the rows
#' are split with `prepare_export_batches()` into `partitioning$count`
#' batches, numbered from `partitioning$first`; such a contract without
#' `count` stops the function. For a contract with `variables`, the
#' variable of `data_files` must be one of them; for a contract without,
#' it is not used. Each file is saved with `save_export_artefact()`.
#' @seealso [save_export_artefact()], [prepare_export_batches()]
#' @examples
#' \dontrun{
#' save_cell_table_export(
#'   data_files = data_bio1_files,
#'   contract = contract_chelsa_trace21k_palaeoclimate,
#'   dir_output = here::here("Outputs", "Data_temp", "Palaoclimate")
#' )
#' }
save_cell_table_export <- function(
  data_files,
  contract,
  dir_output,
  date_save = base::Sys.Date(),
  flag_replace_previous = FALSE
) {
  assertthat::assert_that(
    base::is.data.frame(data_files),
    base::all(c("variable", "age", "path_file") %in% base::names(data_files)),
    base::nrow(data_files) > 0L,
    msg = stringr::str_c(
      "`data_files` must be a data frame with at least one row and the",
      " columns `variable`, `age` and `path_file`."
    )
  )

  assertthat::assert_that(
    base::is.list(contract),
    rlang::is_string(contract[["contract_id"]]),
    rlang::is_string(contract[["partitioning"]][["scheme"]]),
    msg = "`contract` must be a contract from `load_producer_contract()`."
  )

  vec_variables_files <-
    dplyr::pull(data_files, "variable") |>
    base::unique()

  assertthat::assert_that(
    base::length(vec_variables_files) == 1L,
    msg = "`data_files` must hold the cell tables of one variable."
  )

  assertthat::assert_that(
    !base::anyNA(dplyr::pull(data_files, "age")),
    !base::anyDuplicated(dplyr::pull(data_files, "age")),
    msg = "Every cell table of `data_files` must have its own `age`."
  )

  vec_paths_cell_tables_missing <-
    dplyr::pull(data_files, "path_file") |>
    purrr::discard(.p = ~ base::file.exists(.x))

  assertthat::assert_that(
    base::length(vec_paths_cell_tables_missing) == 0L,
    msg = stringr::str_c(
      "These cell tables do not exist: ",
      stringr::str_c(vec_paths_cell_tables_missing, collapse = ", ")
    )
  )

  # The contract variable, or none for a contract without variables
  if (
    base::is.null(contract[["variables"]])
  ) {
    sel_variable <- NULL
  } else {
    vec_variables_contract <-
      purrr::map_chr(
        .x = contract[["variables"]][["table"]],
        .f = ~ purrr::chuck(.x, "variable")
      )

    assertthat::assert_that(
      vec_variables_files %in% vec_variables_contract,
      msg = stringr::str_c(
        "The variable `", vec_variables_files, "` of `data_files` is not a",
        " variable of the contract `", contract[["contract_id"]], "`."
      )
    )

    sel_variable <- vec_variables_files
  }

  scheme_partition <-
    contract[["partitioning"]][["scheme"]]

  n_batches <-
    contract[["partitioning"]][["count"]]

  assertthat::assert_that(
    base::identical(scheme_partition, "none") || !base::is.null(n_batches),
    msg = stringr::str_c(
      "The contract `", contract[["contract_id"]], "` is partitioned but",
      " gives no partition `count`, so the number of batches is unknown."
    )
  )

  # Youngest age first, each table in its own row order
  data_export <-
    data_files |>
    dplyr::arrange(.data[["age"]]) |>
    dplyr::pull("path_file") |>
    purrr::map(.f = ~ qs2::qs_read(file = .x)) |>
    dplyr::bind_rows()

  if (
    base::identical(scheme_partition, "none")
  ) {
    res <-
      save_export_artefact(
        data_export = data_export,
        contract = contract,
        dir_output = dir_output,
        variable = sel_variable,
        date_save = date_save,
        flag_replace_previous = flag_replace_previous
      )
  } else {
    list_batches <-
      prepare_export_batches(
        data_export = data_export,
        n_batches = n_batches
      )

    vec_partitions <-
      contract[["partitioning"]][["first"]] +
      base::seq_len(base::length(list_batches)) - 1L

    res <-
      purrr::map2_chr(
        .x = list_batches,
        .y = vec_partitions,
        .f = ~ save_export_artefact(
          data_export = .x,
          contract = contract,
          dir_output = dir_output,
          variable = sel_variable,
          partition = .y,
          date_save = date_save,
          flag_replace_previous = flag_replace_previous
        )
      )
  }

  return(res)
}

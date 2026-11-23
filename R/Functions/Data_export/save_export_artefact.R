#' @title Save one export artefact under its contract file name
#' @description
#' Save an R object as a `qs2` file under the file name that its VegVault
#' producer contract prescribes, without replacing an earlier version of
#' the same artefact unless asked to.
#' @param data_export
#' The object to save, normally a data frame.
#' @param contract
#' A contract as returned by `load_producer_contract()` (the project's copy
#' in `Data/Contracts/`).
#' @param dir_output
#' Character scalar. The folder to save into; created when missing.
#' @param variable
#' Character scalar. The contract variable of the file, for example
#' `"bio1"`, for a contract with `variables`; `NULL` (default) for a
#' contract without.
#' @param partition
#' Whole number. The partition of the file, for a contract with the
#' partition scheme `"integer"`; `NULL` (default) for the scheme `"none"`.
#' @param date_save
#' A `Date`, written into the file name. Default: today.
#' @param flag_replace_previous
#' Logical. `TRUE` removes an earlier version of the same artefact (the
#' same variable and partition under another date or hash) before saving.
#' Default `FALSE`.
#' @return
#' Character scalar. The path of the saved file.
#' @details
#' The name fills the first file-name template of the contract with
#' `build_producer_file_name()`: the date, the hash `rlang::hash()` of
#' `data_export`, the partition and the `file_token` of the variable. The
#' file is written with `qs2::qs_save()` to `<name>.part` and then renamed,
#' so an interrupted save leaves no file under the final name. A file that
#' already exists under the final name is kept and not written again.
#' Another version of the same artefact in `dir_output` stops the function
#' unless `flag_replace_previous` is `TRUE`; only files that match the
#' contract's file-name pattern with the same variable and partition are
#' ever removed. Partition schemes other than `"none"` and `"integer"` are
#' not supported.
#' @seealso [save_cell_table_export()], [build_producer_file_name()]
#' @examples
#' \dontrun{
#' save_export_artefact(
#'   data_export = data_cells,
#'   contract = load_producer_contract(
#'     here::here("Data", "Contracts", "chelsa_neoclimate.yml")
#'   ),
#'   dir_output = here::here("Outputs", "Data_temp", "Neoclimate"),
#'   variable = "bio1"
#' )
#' }
save_export_artefact <- function(
  data_export,
  contract,
  dir_output,
  variable = NULL,
  partition = NULL,
  date_save = base::Sys.Date(),
  flag_replace_previous = FALSE
) {
  assertthat::assert_that(
    base::is.list(contract),
    rlang::is_string(contract[["contract_id"]]),
    base::is.character(contract[["file"]][["name_templates"]]),
    base::length(contract[["file"]][["name_templates"]]) > 0L,
    rlang::is_string(contract[["partitioning"]][["scheme"]]),
    msg = "`contract` must be a contract from `load_producer_contract()`."
  )

  assertthat::assert_that(
    rlang::is_string(dir_output) &&
      !base::is.na(dir_output) &&
      base::nzchar(dir_output),
    msg = "`dir_output` must be one folder path."
  )

  assertthat::assert_that(
    base::inherits(date_save, "Date"),
    base::length(date_save) == 1L,
    !base::is.na(date_save),
    msg = "`date_save` must be one Date."
  )

  assertthat::assert_that(
    rlang::is_bool(flag_replace_previous),
    msg = "`flag_replace_previous` must be TRUE or FALSE."
  )

  list_variable_table <-
    contract[["variables"]][["table"]]

  # The file token of the variable, or none for a contract without
  #   variables
  if (
    base::is.null(list_variable_table)
  ) {
    assertthat::assert_that(
      base::is.null(variable),
      msg = stringr::str_c(
        "The contract `", contract[["contract_id"]], "` has no variables,",
        " so `variable` must be NULL."
      )
    )

    variable_token <- NULL
  } else {
    vec_variables <-
      purrr::map_chr(
        .x = list_variable_table,
        .f = ~ purrr::chuck(.x, "variable")
      )

    assertthat::assert_that(
      rlang::is_string(variable) && variable %in% vec_variables,
      msg = stringr::str_c(
        "`variable` must be one of the variables of the contract `",
        contract[["contract_id"]], "`: ",
        stringr::str_c(vec_variables, collapse = ", "), "."
      )
    )

    variable_token <-
      purrr::map_chr(
        .x = list_variable_table,
        .f = ~ purrr::chuck(.x, "file_token")
      )[vec_variables == variable]
  }

  scheme_partition <-
    contract[["partitioning"]][["scheme"]]

  if (
    base::identical(scheme_partition, "none")
  ) {
    assertthat::assert_that(
      base::is.null(partition),
      msg = stringr::str_c(
        "The contract `", contract[["contract_id"]], "` is not partitioned,",
        " so `partition` must be NULL."
      )
    )

    partition_integer <- NULL
  } else {
    assertthat::assert_that(
      base::identical(scheme_partition, "integer"),
      msg = "Only the partition schemes `none` and `integer` are supported."
    )

    partition_first <-
      contract[["partitioning"]][["first"]]

    assertthat::assert_that(
      rlang::is_scalar_integerish(partition, finite = TRUE) &&
        partition >= partition_first,
      msg = stringr::str_c(
        "`partition` must be one whole number of at least ",
        partition_first, "."
      )
    )

    partition_integer <-
      base::as.integer(partition)
  }

  name_file <-
    build_producer_file_name(
      name_template = contract[["file"]][["name_templates"]][1],
      date = date_save,
      hash = rlang::hash(data_export),
      partition = partition_integer,
      variable_token = variable_token
    )

  base::dir.create(path = dir_output, recursive = TRUE, showWarnings = FALSE)

  path_file <-
    base::file.path(dir_output, name_file)

  # Earlier versions: files of the contract with the same variable and
  #   partition under another name
  data_present <-
    build_producer_file_table(
      vec_file_names = base::list.files(path = dir_output),
      contract = contract
    )

  text_variable <-
    if (
      base::is.null(variable)
    ) {
      ""
    } else {
      variable
    }

  text_partition <-
    if (
      base::is.null(partition_integer)
    ) {
      ""
    } else {
      base::as.character(partition_integer)
    }

  vec_previous <-
    data_present |>
    dplyr::filter(
      .data[["is_match"]],
      .data[["file_name"]] != name_file,
      dplyr::coalesce(.data[["variable"]], "") == text_variable,
      dplyr::coalesce(
        base::as.character(base::as.integer(.data[["partition"]])),
        ""
      ) == text_partition
    ) |>
    dplyr::pull("file_name")

  if (
    base::length(vec_previous) > 0L && isFALSE(flag_replace_previous)
  ) {
    cli::cli_abort(
      c(
        "{.path {dir_output}} already holds an earlier version of
        {.file {name_file}}.",
        "x" = "{.file {vec_previous}}",
        "i" = "Remove it deliberately, or set `flag_replace_previous` to
        TRUE."
      )
    )
  }

  if (
    base::length(vec_previous) > 0L
  ) {
    vec_removed <-
      base::file.remove(base::file.path(dir_output, vec_previous))

    if (
      !base::all(vec_removed)
    ) {
      cli::cli_abort(
        "Could not remove {.file {vec_previous[!vec_removed]}}."
      )
    }
  }

  # Written under a temporary name first, so an interrupted save leaves no
  #   file under the final name
  if (
    !base::file.exists(path_file)
  ) {
    path_part <-
      stringr::str_c(path_file, ".part")

    qs2::qs_save(object = data_export, file = path_part)

    fs::file_move(path = path_part, new_path = path_file)
  }

  res <- path_file

  return(res)
}

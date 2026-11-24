#' @title Resolve the export folder of a contract
#' @description
#' Place the folder that a VegVault producer contract names for its files
#' under the configured export folder, so a test run writes the same
#' layout into another folder.
#' @param dir_contract
#' Character scalar. The `producer: directory` of the contract, for
#' example `"Outputs/Data/Neoclimate"`.
#' @param dir_data_contract
#' Character scalar. The folder that the contracts' directories start
#' with, `"Outputs/Data"`.
#' @param dir_data
#' Character scalar. The configured export folder, for example
#' `"Outputs/Data"` or `"Outputs/Data_temp"`.
#' @return
#' Character scalar. `dir_contract` with its leading `dir_data_contract`
#' replaced by `dir_data`.
#' @details
#' Folders are compared as written, with forward slashes and without a
#' trailing slash. Aborts when `dir_contract` is neither
#' `dir_data_contract` nor a folder inside it.
#' @seealso [build_export_targets()]
#' @examples
#' \dontrun{
#' resolve_export_folder(
#'   dir_contract = "Outputs/Data/Neoclimate",
#'   dir_data_contract = "Outputs/Data",
#'   dir_data = "Outputs/Data_temp"
#' )
#' }
resolve_export_folder <- function(
  dir_contract,
  dir_data_contract,
  dir_data
) {
  purrr::iwalk(
    .x = base::list(
      dir_contract = dir_contract,
      dir_data_contract = dir_data_contract,
      dir_data = dir_data
    ),
    .f = ~ assertthat::assert_that(
      rlang::is_string(.x) && !base::is.na(.x) && base::nzchar(.x),
      msg = stringr::str_c("`", .y, "` must be one non-empty folder name.")
    )
  )

  text_prefix <-
    stringr::str_c(dir_data_contract, "/")

  if (
    base::identical(dir_contract, dir_data_contract)
  ) {
    res <- dir_data
  } else if (
    stringr::str_starts(dir_contract, stringr::fixed(text_prefix))
  ) {
    res <-
      base::file.path(
        dir_data,
        stringr::str_sub(dir_contract, start = base::nchar(text_prefix) + 1L)
      )
  } else {
    cli::cli_abort(
      c(
        "The contract folder {.path {dir_contract}} is not inside
        {.path {dir_data_contract}}.",
        "i" = "Set `dir_data_contract` in {.file config.yml} to the folder
        that the contracts' directories start with."
      )
    )
  }

  return(res)
}

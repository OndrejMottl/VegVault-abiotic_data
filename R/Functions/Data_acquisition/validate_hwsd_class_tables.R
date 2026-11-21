#' @title Validate that the HWSD soil-class tables exist
#' @description
#' Stop unless the CSV files of the HWSD soil-class tables are present,
#' and name the configuration flag that writes them.
#' @param dir_tables
#' Character scalar. Folder of the CSV files.
#' @param vec_file_names
#' Character vector. File names that must be present. Default: the three
#' files `save_hwsd_class_tables()` writes.
#' @return
#' A character vector of the paths of the files, in the order of
#' `vec_file_names`.
#' @details
#' A missing file stops with a message naming
#' `data_acquisition: hwsd_class_tables: flag_read_access_database` in
#' `config.yml`, the stage runner, and the Access driver it needs.
#' @seealso [save_hwsd_class_tables()]
#' @examples
#' \dontrun{
#' validate_hwsd_class_tables(dir_tables = "Data/Input/HWSD_tables")
#' }
validate_hwsd_class_tables <- function(
  dir_tables,
  vec_file_names = c("hwsd2_smu.csv", "d_wrb2.csv", "d_wrb2code.csv")
) {
  assertthat::assert_that(
    rlang::is_string(dir_tables),
    msg = "`dir_tables` must be a single folder path."
  )

  assertthat::assert_that(
    base::is.character(vec_file_names),
    base::length(vec_file_names) > 0L,
    !base::anyNA(vec_file_names),
    msg = "`vec_file_names` must name at least one file."
  )

  vec_paths <-
    base::file.path(dir_tables, vec_file_names)

  vec_files_missing <-
    vec_file_names[!base::file.exists(vec_paths)]

  if (
    base::length(vec_files_missing) > 0L
  ) {
    cli::cli_abort(
      c(
        "The HWSD class tables are missing: {.val {vec_files_missing}}.",
        "i" = stringr::str_c(
          "Set `flag_read_access_database: true` in ",
          "`data_acquisition: hwsd_class_tables` of `config.yml` and run ",
          "`R/Analyses/01_Data_acquisition/00_run.R`. Reading the database ",
          "needs the Microsoft Access ODBC driver (Windows only)."
        )
      )
    )
  }

  res <-
    vec_paths

  return(res)
}

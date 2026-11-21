#' @title Save the HWSD soil-class tables behind a flag
#' @description
#' When the flag is set, read the soil-unit and soil-group tables from the
#' HWSD v2.2 Access database and save them as CSV files, so that later
#' stages need no Access driver; otherwise do nothing.
#' @param vec_extracted_files
#' Character vector. Paths of the extracted HWSD files; the one ending in
#' `.mdb` is read.
#' @param dir_tables
#' Character scalar. Folder of the CSV files; created when missing.
#' @param flag_read_access_database
#' Logical scalar. `FALSE` (the configured default) returns at once,
#' without reading the database.
#' @param verbose
#' Logical. If `TRUE` (default), progress messages are printed to the
#' console via `cli`.
#' @return
#' A character vector of the paths written: `hwsd2_smu.csv` (columns
#' `HWSD2_SMU_ID`, `SEQUENCE`, `SHARE`, `WRB2`, `WRB2_CODE` and `WRB4` of
#' table `HWSD2_SMU`, sorted by `HWSD2_SMU_ID` and `SEQUENCE`),
#' `d_wrb2.csv` and `d_wrb2code.csv` (the full tables `D_WRB2` and
#' `D_WRB2code`, sorted by their first column); zero length when the flag
#' is not set.
#' @details
#' Needs the Microsoft Access ODBC driver (`Microsoft Access Driver (*.mdb,
#' *.accdb)`), which exists on Windows only. Aborts before opening the
#' database when no `.mdb` file is given or when a CSV file already exists
#' (it is never overwritten), and with the driver named when the database
#' cannot be opened.
#' @seealso [validate_hwsd_class_tables()], [save_archive_members()]
#' @examples
#' \dontrun{
#' save_hwsd_class_tables(
#'   vec_extracted_files = hwsd_extracted_files,
#'   dir_tables = "Data/Input/HWSD_tables",
#'   flag_read_access_database = FALSE
#' )
#' }
save_hwsd_class_tables <- function(
  vec_extracted_files,
  dir_tables,
  flag_read_access_database,
  verbose = TRUE
) {
  assertthat::assert_that(
    base::is.character(vec_extracted_files),
    !base::anyNA(vec_extracted_files),
    msg = "`vec_extracted_files` must be a character vector of paths."
  )

  assertthat::assert_that(
    rlang::is_string(dir_tables),
    msg = "`dir_tables` must be a single folder path."
  )

  assertthat::assert_that(
    rlang::is_bool(flag_read_access_database),
    msg = "`flag_read_access_database` must be TRUE or FALSE."
  )

  assertthat::assert_that(
    rlang::is_bool(verbose),
    msg = "`verbose` must be TRUE or FALSE."
  )

  if (
    !isTRUE(flag_read_access_database)
  ) {
    return(base::character(0))
  }

  vec_path_database <-
    stringr::str_subset(string = vec_extracted_files, pattern = "[.]mdb$")

  n_databases <-
    base::length(vec_path_database)

  if (
    n_databases != 1L
  ) {
    cli::cli_abort(
      "Exactly one extracted `.mdb` file is needed; found {n_databases}."
    )
  }

  vec_file_names <-
    c("hwsd2_smu.csv", "d_wrb2.csv", "d_wrb2code.csv")

  vec_paths <-
    base::file.path(dir_tables, vec_file_names)

  vec_files_existing <-
    vec_file_names[base::file.exists(vec_paths)]

  if (
    base::length(vec_files_existing) > 0L
  ) {
    cli::cli_abort(
      c(
        "A class table exists and is never overwritten.",
        "x" = "{.val {vec_files_existing}}",
        "i" = "Move or delete it by hand first, with approval."
      )
    )
  }

  # The driver name holds braces, so the string is built without glue
  text_connection <-
    stringr::str_c(
      "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=",
      base::normalizePath(vec_path_database, winslash = "\\")
    )

  con_database <-
    base::tryCatch(
      expr = DBI::dbConnect(
        drv = odbc::odbc(),
        .connection_string = text_connection
      ),
      error = function(e) e
    )

  if (
    base::inherits(con_database, "error")
  ) {
    text_error <-
      base::conditionMessage(con_database) |>
      stringr::str_replace_all(pattern = "\\{", replacement = "{{") |>
      stringr::str_replace_all(pattern = "\\}", replacement = "}}")

    cli::cli_abort(
      c(
        "The HWSD database {.file {vec_path_database}} cannot be opened.",
        "x" = text_error,
        "i" = stringr::str_c(
          "Reading it needs the Microsoft Access ODBC driver ",
          "(Microsoft Access Driver (*.mdb, *.accdb)), which exists on ",
          "Windows only."
        )
      )
    )
  }

  withr::defer(DBI::dbDisconnect(conn = con_database))

  if (
    isTRUE(verbose)
  ) {
    cli::cli_inform("Reading the class tables of {.file {vec_path_database}}.")
  }

  data_smu <-
    DBI::dbGetQuery(
      conn = con_database,
      statement = stringr::str_c(
        "SELECT HWSD2_SMU_ID, SEQUENCE, SHARE, WRB2, WRB2_CODE, WRB4 ",
        "FROM HWSD2_SMU"
      )
    ) |>
    tibble::as_tibble() |>
    dplyr::arrange(.data[["HWSD2_SMU_ID"]], .data[["SEQUENCE"]])

  data_wrb2 <-
    DBI::dbGetQuery(conn = con_database, statement = "SELECT * FROM D_WRB2") |>
    tibble::as_tibble() |>
    dplyr::arrange(dplyr::pick(dplyr::everything()))

  data_wrb2code <-
    DBI::dbGetQuery(
      conn = con_database,
      statement = "SELECT * FROM D_WRB2code"
    ) |>
    tibble::as_tibble() |>
    dplyr::arrange(dplyr::pick(dplyr::everything()))

  base::dir.create(
    path = dir_tables,
    recursive = TRUE,
    showWarnings = FALSE
  )

  purrr::walk2(
    .x = base::list(data_smu, data_wrb2, data_wrb2code),
    .y = vec_paths,
    .f = ~ readr::write_csv(x = .x, file = .y, na = "")
  )

  if (
    isTRUE(verbose)
  ) {
    cli::cli_inform("Saved {.file {vec_paths}}.")
  }

  res <-
    vec_paths

  return(res)
}

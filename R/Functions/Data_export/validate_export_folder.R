#' @title Validate that an export folder holds exactly the exported files
#' @description
#' Check that the folder of an exported artefact holds the files the
#' export wrote and nothing else, so a file left from an earlier run, an
#' extra batch or an interrupted save cannot become part of the artefact.
#' @param vec_paths_export
#' Character vector. The paths of the files the export wrote, all in one
#' existing folder, without repeats.
#' @return
#' `vec_paths_export`, unchanged.
#' @details
#' Every entry of the folder counts: files of any name, subfolders, hidden
#' files and temporary `.part` files. Aborts naming every entry that the
#' export did not write and every written file that is missing. Nothing is
#' removed.
#' @seealso [validate_export_artefact()]
#' @examples
#' \dontrun{
#' validate_export_folder(vec_paths_export = vec_paths_neoclimate)
#' }
validate_export_folder <- function(vec_paths_export) {
  assertthat::assert_that(
    base::is.character(vec_paths_export),
    base::length(vec_paths_export) > 0L,
    !base::anyNA(vec_paths_export),
    !base::anyDuplicated(vec_paths_export),
    msg = stringr::str_c(
      "`vec_paths_export` must be a character vector of file paths without",
      " missing values or repeats."
    )
  )

  vec_dirs <-
    base::dirname(vec_paths_export) |>
    base::normalizePath(winslash = "/", mustWork = FALSE) |>
    base::unique()

  assertthat::assert_that(
    base::length(vec_dirs) == 1L,
    msg = "The files of `vec_paths_export` must lie in one folder."
  )

  assertthat::assert_that(
    base::dir.exists(vec_dirs),
    msg = "The folder of `vec_paths_export` must exist."
  )

  vec_entries <-
    base::list.files(path = vec_dirs, all.files = TRUE, no.. = TRUE)

  vec_names_export <-
    base::basename(vec_paths_export)

  vec_extra <-
    base::setdiff(vec_entries, vec_names_export)

  vec_missing <-
    base::setdiff(vec_names_export, vec_entries)

  if (
    base::length(vec_extra) > 0L || base::length(vec_missing) > 0L
  ) {
    vec_bullets_extra <-
      if (
        base::length(vec_extra) > 0L
      ) {
        c("x" = "Not written by this export: {.file {vec_extra}}")
      } else {
        NULL
      }

    vec_bullets_missing <-
      if (
        base::length(vec_missing) > 0L
      ) {
        c("x" = "Written but missing: {.file {vec_missing}}")
      } else {
        NULL
      }

    cli::cli_abort(
      c(
        "The folder {.path {vec_dirs}} does not hold exactly the exported
        files.",
        vec_bullets_extra,
        vec_bullets_missing,
        "i" = "Remove leftover files deliberately (nothing is removed here),
        then run the export again."
      )
    )
  }

  res <- vec_paths_export

  return(res)
}

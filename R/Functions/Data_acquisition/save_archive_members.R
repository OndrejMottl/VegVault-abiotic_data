#' @title Save the expected members of downloaded archives
#' @description
#' Extract the listed members of each zip archive of a source into a
#' folder and check their sizes against the archive. A member that already
#' exists with the right size is kept; one with another size is never
#' overwritten.
#' @param data_raw_files
#' A data frame as returned by `validate_source_raw_files()`, with at least
#' the columns `file_name` and `path_file`.
#' @param list_archive_members
#' A named list: archive file name to a character vector of the member
#' names to extract (members sit at the top level of the archive).
#' @param dir_extract
#' Character scalar. Folder to extract into; created when missing.
#' @param verbose
#' Logical. If `TRUE` (default), progress messages are printed to the
#' console via `cli`.
#' @return
#' A character vector of the paths of the extracted members, in the order
#' of `list_archive_members` and then of the members.
#' @details
#' Aborts when an archive is not in `data_raw_files`, when it does not list
#' a member, when an existing member has a size other than the archive
#' lists, or when an extracted member does not get the listed size.
#' @seealso [validate_source_raw_files()], [save_hwsd_class_tables()]
#' @examples
#' \dontrun{
#' save_archive_members(
#'   data_raw_files = hwsd_raw_files,
#'   list_archive_members = list(hwsd2.2_db.zip = "HWSD2.2.mdb"),
#'   dir_extract = "Data/Input/HWSD"
#' )
#' }
save_archive_members <- function(
  data_raw_files,
  list_archive_members,
  dir_extract,
  verbose = TRUE
) {
  assertthat::assert_that(
    base::is.data.frame(data_raw_files),
    base::all(c("file_name", "path_file") %in% base::colnames(data_raw_files)),
    msg = stringr::str_c(
      "`data_raw_files` must be a data frame with the columns ",
      "`file_name` and `path_file`."
    )
  )

  assertthat::assert_that(
    base::is.list(list_archive_members),
    base::length(list_archive_members) > 0L,
    rlang::is_named(list_archive_members),
    base::all(
      purrr::map_lgl(
        .x = list_archive_members,
        .f = ~ base::is.character(.x) &&
          base::length(.x) > 0L &&
          !base::anyNA(.x)
      )
    ),
    msg = stringr::str_c(
      "`list_archive_members` must name each archive with the member ",
      "names to extract."
    )
  )

  assertthat::assert_that(
    rlang::is_string(dir_extract),
    msg = "`dir_extract` must be a single folder path."
  )

  assertthat::assert_that(
    rlang::is_bool(verbose),
    msg = "`verbose` must be TRUE or FALSE."
  )

  vec_archives <-
    base::names(list_archive_members)

  vec_archives_unknown <-
    base::setdiff(vec_archives, dplyr::pull(data_raw_files, "file_name"))

  if (
    base::length(vec_archives_unknown) > 0L
  ) {
    cli::cli_abort(
      "No validated raw file for archive {.val {vec_archives_unknown}}."
    )
  }

  # One row per archive and member, in the order of the configuration
  data_pairs <-
    tibble::tibble(
      file_name = base::rep(
        vec_archives,
        base::lengths(list_archive_members)
      ),
      member = base::unlist(list_archive_members, use.names = FALSE)
    ) |>
    dplyr::left_join(
      dplyr::select(data_raw_files, "file_name", path_archive = "path_file"),
      by = "file_name",
      relationship = "many-to-one"
    )

  # Reading the central directory of an archive extracts nothing
  data_listed <-
    data_raw_files |>
    dplyr::filter(.data[["file_name"]] %in% vec_archives) |>
    dplyr::mutate(
      listing = purrr::map(
        .x = .data[["path_file"]],
        .f = ~ tibble::as_tibble(utils::unzip(zipfile = .x, list = TRUE))
      )
    ) |>
    dplyr::select("file_name", "listing") |>
    tidyr::unnest(cols = "listing") |>
    dplyr::select("file_name", member = "Name", size_listed = "Length")

  data_members <-
    data_pairs |>
    dplyr::left_join(
      data_listed,
      by = c("file_name", "member"),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      size_listed = base::as.numeric(.data[["size_listed"]]),
      path_member = base::file.path(dir_extract, .data[["member"]]),
      size_existing = base::as.numeric(
        base::file.size(.data[["path_member"]])
      )
    )

  vec_members_unlisted <-
    data_members |>
    dplyr::filter(base::is.na(.data[["size_listed"]])) |>
    dplyr::pull("member")

  if (
    base::length(vec_members_unlisted) > 0L
  ) {
    cli::cli_abort(
      c(
        "An archive does not list a member to extract.",
        "x" = "{.val {vec_members_unlisted}}"
      )
    )
  }

  vec_members_differing <-
    data_members |>
    dplyr::filter(
      !base::is.na(.data[["size_existing"]]),
      .data[["size_existing"]] != .data[["size_listed"]]
    ) |>
    dplyr::pull("member")

  if (
    base::length(vec_members_differing) > 0L
  ) {
    cli::cli_abort(
      c(
        stringr::str_c(
          "An extracted member exists with a size other than the archive ",
          "lists; it is never overwritten."
        ),
        "x" = "{.val {vec_members_differing}}",
        "i" = "Move or delete it by hand first, with approval."
      )
    )
  }

  data_members_new <-
    data_members |>
    dplyr::filter(base::is.na(.data[["size_existing"]]))

  if (
    base::nrow(data_members_new) > 0L
  ) {
    if (
      isTRUE(verbose)
    ) {
      cli::cli_inform(
        "Extracting {.val {dplyr::pull(data_members_new, 'member')}}."
      )
    }

    base::dir.create(
      path = dir_extract,
      recursive = TRUE,
      showWarnings = FALSE
    )

    # Members grouped by their archive, one extraction per archive
    list_members_new <-
      base::split(
        x = dplyr::pull(data_members_new, "member"),
        f = dplyr::pull(data_members_new, "path_archive")
      )

    purrr::iwalk(
      .x = list_members_new,
      .f = ~ utils::unzip(
        zipfile = .y,
        files = .x,
        exdir = dir_extract,
        overwrite = FALSE
      )
    )
  }

  vec_sizes_after <-
    base::as.numeric(
      base::file.size(dplyr::pull(data_members, "path_member"))
    )

  vec_members_wrong <-
    dplyr::pull(data_members, "member")[
      base::is.na(vec_sizes_after) |
        vec_sizes_after != dplyr::pull(data_members, "size_listed")
    ]

  if (
    base::length(vec_members_wrong) > 0L
  ) {
    cli::cli_abort(
      c(
        "A member was not extracted with the size the archive lists.",
        "x" = "{.val {vec_members_wrong}}"
      )
    )
  }

  res <-
    dplyr::pull(data_members, "path_member")

  return(res)
}

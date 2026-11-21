#' @title Build the download-stage targets of one raw data source
#' @description
#' Declare the `{targets}` that obtain and check the raw files of one data
#' source: the expected files, the flag-gated downloads, the file
#' inventory, one completeness branch per file, the flag-gated checksum
#' record, the download records and the validated file table, plus the
#' extraction of archive members for a source that lists them.
#' @param sel_source
#' Character scalar. Name of the source in `list_data_sources`; also the
#' prefix of every target name.
#' @param list_data_sources
#' The `data_sources` section of the configuration: one list per source
#' as described in `build_source_expected_files()`, with `file_check`
#' (`"geotiff"` or `"zip"`), `flag_download` and `flag_record_checksums`
#' (TRUE or FALSE), optionally `server_sizes_file` (relative to the data
#' root) and, for archives, `archive_members` (as in
#' `save_archive_members()`).
#' @param list_data_acquisition
#' The `data_acquisition` section of the configuration, with `dir_records`
#' and `dir_download` (relative to the data root).
#' @param path_data_storage
#' Character scalar. The data root.
#' @return
#' A list of target objects: `<source>_expected_files`,
#' `<source>_downloads`, `<source>_file_inventory`, `<source>_file_checks`,
#' `<source>_checksums`, `<source>_download_records` and the public
#' `<source>_raw_files`; with `server_sizes_file` also
#' `<source>_path_server_sizes` and `<source>_server_sizes`; with
#' `archive_members` also the public `<source>_extracted_files`.
#' @details
#' The source is validated while declaring, by building its expected files
#' once. The downloads and the checksum record always run when their flag
#' is set and are skipped otherwise; the inventory always runs, so a
#' replaced file reruns only its own completeness branch. Flag literals are
#' part of the commands, so changing a flag reruns the targets that use it.
#' @seealso [build_source_expected_files()], [run_source_downloads()],
#'   [diagnose_raw_file()], [run_source_checksums()],
#'   [validate_source_raw_files()], [save_archive_members()]
#' @examples
#' \dontrun{
#' build_source_acquisition_targets(
#'   sel_source = "chelsa_v21",
#'   list_data_sources = load_config_value(value = "data_sources"),
#'   list_data_acquisition = load_config_value(value = "data_acquisition"),
#'   path_data_storage = path_data_storage
#' )
#' }
build_source_acquisition_targets <- function(
  sel_source,
  list_data_sources,
  list_data_acquisition,
  path_data_storage
) {
  assertthat::assert_that(
    base::is.list(list_data_sources),
    rlang::is_string(sel_source),
    sel_source %in% base::names(list_data_sources),
    stringr::str_detect(string = sel_source, pattern = "^[a-z0-9_]+$"),
    msg = stringr::str_c(
      "`sel_source` must be the name of one source in ",
      "`list_data_sources`, of lower-case letters, digits and underscores."
    )
  )

  list_source <-
    list_data_sources[[sel_source]]

  assertthat::assert_that(
    rlang::is_string(list_source[["file_check"]]),
    list_source[["file_check"]] %in% c("geotiff", "zip"),
    msg = stringr::str_c(
      "Source `", sel_source, "` needs `file_check` \"geotiff\" or \"zip\"."
    )
  )

  assertthat::assert_that(
    rlang::is_bool(list_source[["flag_download"]]),
    rlang::is_bool(list_source[["flag_record_checksums"]]),
    msg = stringr::str_c(
      "Source `", sel_source, "` needs `flag_download` and ",
      "`flag_record_checksums` as TRUE or FALSE."
    )
  )

  assertthat::assert_that(
    base::is.null(list_source[["server_sizes_file"]]) ||
      rlang::is_string(list_source[["server_sizes_file"]]),
    msg = stringr::str_c(
      "Source `", sel_source, "` needs `server_sizes_file` as one path, ",
      "or none."
    )
  )

  assertthat::assert_that(
    base::is.list(list_data_acquisition),
    rlang::is_string(list_data_acquisition[["dir_records"]]),
    rlang::is_string(list_data_acquisition[["dir_download"]]),
    msg = stringr::str_c(
      "`list_data_acquisition` needs `dir_records` and `dir_download` ",
      "as folder paths."
    )
  )

  assertthat::assert_that(
    rlang::is_string(path_data_storage),
    !base::is.na(path_data_storage),
    base::nzchar(path_data_storage),
    msg = "`path_data_storage` must be one non-empty string."
  )

  # Only the file definition enters the expected-files command, so a
  #   changed flag does not rebuild the file list
  list_source_files <-
    purrr::discard_at(
      x = list_source,
      at = c(
        "file_check", "flag_download", "flag_record_checksums",
        "server_sizes_file", "archive_members"
      )
    )

  # Fail while declaring the pipeline, not during a run
  build_source_expected_files(
    list_source = list_source_files,
    path_data_storage = path_data_storage
  )

  list_archive_members <-
    list_source[["archive_members"]]

  flag_server_sizes <-
    !base::is.null(list_source[["server_sizes_file"]])

  flag_archives <-
    !base::is.null(list_archive_members)

  flag_download <-
    list_source[["flag_download"]]

  flag_record_checksums <-
    list_source[["flag_record_checksums"]]

  file_check <-
    list_source[["file_check"]]

  dir_records <-
    base::file.path(path_data_storage, list_data_acquisition[["dir_records"]])

  dir_download <-
    base::file.path(
      path_data_storage,
      list_data_acquisition[["dir_download"]]
    )

  # Target names of the source; symbols where commands refer to them
  vec_name <-
    c(
      path_server_sizes = "path_server_sizes",
      server_sizes = "server_sizes",
      expected_files = "expected_files",
      downloads = "downloads",
      file_inventory = "file_inventory",
      file_checks = "file_checks",
      checksums = "checksums",
      download_records = "download_records",
      raw_files = "raw_files",
      extracted_files = "extracted_files"
    ) |>
    purrr::map_chr(.f = ~ stringr::str_c(sel_source, "_", .x))

  list_sym <-
    purrr::map(.x = vec_name, .f = ~ rlang::sym(.x))

  expr_server_sizes <-
    if (
      isTRUE(flag_server_sizes)
    ) {
      list_sym[["server_sizes"]]
    } else {
      NULL
    }

  list_targets_sizes <-
    if (
      isTRUE(flag_server_sizes)
    ) {
      list(
        # The tracked record of the sizes the server announced
        targets::tar_target_raw(
          name = vec_name[["path_server_sizes"]],
          command = base::file.path(
            path_data_storage,
            list_source[["server_sizes_file"]]
          ),
          format = "file"
        ),
        targets::tar_target_raw(
          name = vec_name[["server_sizes"]],
          command = rlang::expr(
            load_raster_server_sizes(
              path_file = !!list_sym[["path_server_sizes"]]
            )
          )
        )
      )
    } else {
      list()
    }

  list_targets_files <-
    list(
      # Paths, addresses and announced sizes of every expected file
      targets::tar_target_raw(
        name = vec_name[["expected_files"]],
        command = rlang::expr(
          build_source_expected_files(
            list_source = !!list_source_files,
            path_data_storage = !!path_data_storage,
            data_server_sizes = !!expr_server_sizes
          )
        )
      ),
      # Downloads only with the flag set; then on every run, as it skips
      #   the files already present
      targets::tar_target_raw(
        name = vec_name[["downloads"]],
        command = rlang::expr(
          run_source_downloads(
            data_expected_files = !!list_sym[["expected_files"]],
            flag_download = !!flag_download,
            file_check = !!file_check,
            dir_download = !!dir_download,
            dir_records = !!dir_records
          )
        ),
        cue = targets::tar_cue(
          mode = dplyr::if_else(flag_download, "always", "thorough")
        )
      ),
      # Sizes and modification times on every run, after the downloads, so
      #   a replaced file reruns only its own check
      targets::tar_target_raw(
        name = vec_name[["file_inventory"]],
        command = rlang::expr(
          load_raster_file_inventory(
            data_expected_files = dplyr::select(
              !!list_sym[["expected_files"]],
              "product", "variable", "age", "file_name", "path_file",
              "expected_size_bytes"
            )
          )
        ),
        deps = c(
          vec_name[["expected_files"]],
          vec_name[["downloads"]],
          "load_raster_file_inventory"
        ),
        cue = targets::tar_cue(mode = "always")
      ),
      # Completeness of one file: size and TIFF block table or zip
      #   directory, never pixel data
      targets::tar_target_raw(
        name = vec_name[["file_checks"]],
        command = rlang::expr(
          diagnose_raw_file(
            path_file = purrr::chuck(
              !!list_sym[["file_inventory"]],
              "path_file"
            ),
            expected_size_bytes = purrr::chuck(
              !!list_sym[["file_inventory"]],
              "expected_size_bytes"
            ),
            file_check = !!file_check
          )
        ),
        pattern = rlang::expr(map(!!list_sym[["file_inventory"]]))
      ),
      # Checksums of present files without a record, only with the flag
      targets::tar_target_raw(
        name = vec_name[["checksums"]],
        command = rlang::expr(
          run_source_checksums(
            data_file_checks = !!list_sym[["file_checks"]],
            data_expected_files = !!list_sym[["expected_files"]],
            dir_records = !!dir_records,
            flag_record_checksums = !!flag_record_checksums
          )
        ),
        cue = targets::tar_cue(
          mode = dplyr::if_else(flag_record_checksums, "always", "thorough")
        )
      ),
      # Every record, read after this run wrote its own
      targets::tar_target_raw(
        name = vec_name[["download_records"]],
        command = rlang::expr(
          load_download_records(dir_records = !!dir_records)
        ),
        deps = c(
          vec_name[["downloads"]],
          vec_name[["checksums"]],
          "load_download_records"
        ),
        cue = targets::tar_cue(mode = "always")
      ),
      # Public: the complete and recorded raw files of the source
      targets::tar_target_raw(
        name = vec_name[["raw_files"]],
        command = rlang::expr(
          validate_source_raw_files(
            data_expected_files = !!list_sym[["expected_files"]],
            data_file_checks = !!list_sym[["file_checks"]],
            data_download_records = !!list_sym[["download_records"]],
            sel_source = !!sel_source
          )
        )
      )
    )

  list_targets_archives <-
    if (
      isTRUE(flag_archives)
    ) {
      list(
        # Public: the extracted members of the validated archives
        targets::tar_target_raw(
          name = vec_name[["extracted_files"]],
          command = rlang::expr(
            save_archive_members(
              data_raw_files = !!list_sym[["raw_files"]],
              list_archive_members = !!list_archive_members,
              dir_extract = !!base::file.path(
                path_data_storage,
                list_source[["dir"]]
              )
            )
          ),
          format = "file"
        )
      )
    } else {
      list()
    }

  res <-
    c(
      list_targets_sizes,
      list_targets_files,
      list_targets_archives
    )

  return(res)
}

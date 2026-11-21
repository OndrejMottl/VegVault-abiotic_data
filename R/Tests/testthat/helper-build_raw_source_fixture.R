#' @title Build small raw files on a local server folder for tests
#' @description
#' Write a folder of small files that tests can download through `file://`
#' addresses: two complete TIFF files of different sizes, a truncated TIFF
#' and a zip archive with two members.
#' @param dir_root
#' Character scalar. An empty temporary folder to write into.
#' @return
#' A list with `dir_server`; the paths `path_bio1`, `path_bio12`,
#' `path_truncated` and `path_archive`; their `file://` addresses
#' `url_bio1`, `url_bio12`, `url_truncated` and `url_archive`; their sizes
#' `size_bio1`, `size_bio12`, `size_truncated` and `size_archive`; and
#' `vec_members` (member names of the archive) with `vec_member_sizes`.
#' @details
#' The TIFF files have 8 columns and 4 (bio1) or 5 (bio12) rows in one-row
#' strips. The truncated TIFF keeps its header and its first strip only.
build_raw_source_fixture <- function(dir_root) {
  # Forward slashes, because the paths become file:// addresses
  dir_root_normalised <-
    base::normalizePath(dir_root, winslash = "/")

  dir_server <-
    base::file.path(dir_root_normalised, "Server")

  dir_members <-
    base::file.path(dir_root_normalised, "Members")

  base::dir.create(path = dir_server, recursive = TRUE)

  base::dir.create(path = dir_members, recursive = TRUE)

  path_bio1 <-
    base::file.path(dir_server, "neo_bio1.tif")

  path_bio12 <-
    base::file.path(dir_server, "neo_bio12.tif")

  path_truncated <-
    base::file.path(dir_server, "neo_truncated.tif")

  path_archive <-
    base::file.path(dir_server, "archive.zip")

  build_tiff_block_fixture(
    path_file = path_bio1,
    n_rows = 4L,
    n_cols = 8L
  )

  build_tiff_block_fixture(
    path_file = path_bio12,
    n_rows = 5L,
    n_cols = 8L
  )

  list_layout <-
    build_tiff_block_fixture(
      path_file = path_truncated,
      n_rows = 4L,
      n_cols = 8L
    )

  build_tiff_block_fixture(
    path_file = path_truncated,
    n_rows = 4L,
    n_cols = 8L,
    n_bytes_keep = list_layout[["n_bytes_header"]] +
      list_layout[["n_bytes_block"]]
  )

  vec_members <-
    c("db.mdb", "raster.bil")

  base::writeLines(
    text = "database",
    con = base::file.path(dir_members, "db.mdb")
  )

  base::writeLines(
    text = base::rep("raster values", 20),
    con = base::file.path(dir_members, "raster.bil")
  )

  zip::zip(
    zipfile = path_archive,
    files = vec_members,
    root = dir_members
  )

  vec_paths <-
    c(path_bio1, path_bio12, path_truncated, path_archive)

  vec_urls <-
    stringr::str_c(
      "file:///",
      stringr::str_remove(string = vec_paths, pattern = "^/")
    )

  vec_sizes <-
    base::file.size(vec_paths)

  res <-
    base::list(
      dir_server = dir_server,
      path_bio1 = path_bio1,
      path_bio12 = path_bio12,
      path_truncated = path_truncated,
      path_archive = path_archive,
      url_bio1 = vec_urls[1],
      url_bio12 = vec_urls[2],
      url_truncated = vec_urls[3],
      url_archive = vec_urls[4],
      size_bio1 = vec_sizes[1],
      size_bio12 = vec_sizes[2],
      size_truncated = vec_sizes[3],
      size_archive = vec_sizes[4],
      vec_members = vec_members,
      vec_member_sizes = base::file.size(
        base::file.path(dir_members, vec_members)
      )
    )

  return(res)
}

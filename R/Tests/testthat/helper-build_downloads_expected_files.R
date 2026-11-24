#' @title Build an expected-file table of the local test server
#' @description
#' Return the expected files of a two-file GeoTIFF source whose addresses
#' point to the server folder of `build_raw_source_fixture()`, in the
#' shape `build_source_expected_files()` returns.
#' @param list_server
#' The list returned by `build_raw_source_fixture()`.
#' @param dir_data
#' Character scalar. Folder the files are expected in.
#' @return
#' A tibble with the files `neo_bio1.tif` and `neo_bio12.tif` of product
#' `prod_neo`, their paths in `dir_data`, their `file://` addresses and
#' their server sizes.
build_downloads_expected_files <- function(
  list_server,
  dir_data
) {
  res <-
    tibble::tibble(
      product = "prod_neo",
      variable = c("bio1", "bio12"),
      age = 0,
      time_id = NA_integer_,
      file_name = c("neo_bio1.tif", "neo_bio12.tif"),
      path_file = base::file.path(
        dir_data,
        c("neo_bio1.tif", "neo_bio12.tif")
      ),
      url = c(list_server[["url_bio1"]], list_server[["url_bio12"]]),
      expected_size_bytes = c(
        list_server[["size_bio1"]],
        list_server[["size_bio12"]]
      )
    )

  return(res)
}

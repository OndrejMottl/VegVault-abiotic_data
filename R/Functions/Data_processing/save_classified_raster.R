#' @title Save a raster with its values replaced by classes
#' @description
#' Replace every value of a raster by its class from a table and save the
#' result; values without a class become missing.
#' @param path_input
#' Character scalar. Path of the raster to classify.
#' @param path_output
#' Character scalar. Path of the GeoTIFF to write; its folder is created
#' when missing. It must differ from `path_input`.
#' @param data_classes
#' A data frame with the numeric columns `from` (unique values of the
#' input) and `to` (the class of each).
#' @param datatype
#' Character scalar. `{terra}` storage type of the output, for example
#' `"INT1U"`.
#' @return
#' `path_output`.
#' @details
#' Uses `terra::classify()` with `others = NA`, so input values not in
#' `from` and missing input cells are missing in the output. The output is
#' written to a temporary file and moved into place, as in
#' `save_aggregated_raster()`.
#' @seealso [prepare_hwsd_soil_groups()], [save_resampled_raster()]
#' @examples
#' \dontrun{
#' save_classified_raster(
#'   path_input = "Data/Input/HWSD/HWSD2.bil",
#'   path_output = "Data/Processed/HWSD_v2.2/Intermediate/classes.tif",
#'   data_classes = data_classes,
#'   datatype = "INT1U"
#' )
#' }
save_classified_raster <- function(
  path_input,
  path_output,
  data_classes,
  datatype
) {
  assertthat::assert_that(
    rlang::is_string(path_input),
    base::file.exists(path_input),
    msg = "`path_input` must be the path to an existing raster."
  )

  assertthat::assert_that(
    rlang::is_string(path_output),
    base::nzchar(path_output),
    base::normalizePath(path_output, winslash = "/", mustWork = FALSE) !=
      base::normalizePath(path_input, winslash = "/"),
    msg = "`path_output` must be one path that differs from `path_input`."
  )

  assertthat::assert_that(
    base::is.data.frame(data_classes),
    base::all(c("from", "to") %in% base::colnames(data_classes)),
    base::is.numeric(dplyr::pull(data_classes, "from")),
    base::is.numeric(dplyr::pull(data_classes, "to")),
    !base::anyNA(dplyr::pull(data_classes, "from")),
    !base::anyDuplicated(dplyr::pull(data_classes, "from")),
    msg = stringr::str_c(
      "`data_classes` must be a data frame with unique numeric `from` and ",
      "numeric `to`."
    )
  )

  assertthat::assert_that(
    rlang::is_string(datatype),
    msg = "`datatype` must be one terra storage type."
  )

  mat_classes <-
    base::cbind(
      dplyr::pull(data_classes, "from"),
      dplyr::pull(data_classes, "to")
    )

  base::dir.create(
    path = base::dirname(path_output),
    recursive = TRUE,
    showWarnings = FALSE
  )

  # Written under a temporary name first, as in save_aggregated_raster()
  path_temporary <-
    stringr::str_c(path_output, ".part")

  terra::classify(
    x = terra::rast(path_input),
    rcl = mat_classes,
    others = NA,
    filename = path_temporary,
    overwrite = TRUE,
    wopt = base::list(filetype = "GTiff", datatype = datatype)
  )

  fs::file_move(path = path_temporary, new_path = path_output)

  res <-
    path_output

  return(res)
}

#' @title Save a raster with listed cells set to missing
#' @description
#' Copy a processed raster to its final path and set the listed cells to
#' missing; with no cell listed the copy is unchanged.
#' @param path_input
#' Character scalar. Path of the raster to copy.
#' @param path_output
#' Character scalar. Path of the GeoTIFF to write; its folder is created
#' when missing. It must differ from `path_input`.
#' @param vec_cells
#' Integer vector of cell numbers (as in `{terra}`) to set to missing; may
#' be empty.
#' @param datatype
#' Character scalar. `{terra}` storage type of the output. Default
#' `"FLT4S"`.
#' @return
#' `path_output`.
#' @details
#' The whole raster is read into memory, so use it for processed (coarse)
#' rasters only. Aborts when a cell number lies outside the raster. The
#' output is written to a temporary file and moved into place, as in
#' `save_aggregated_raster()`.
#' @seealso [prepare_processing_mask_cells()], [save_aggregated_raster()]
#' @examples
#' \dontrun{
#' save_masked_raster(
#'   path_input = "Data/Processed/CHELSA_V2.1/Aggregated/bio12.tif",
#'   path_output = "Data/Processed/CHELSA_V2.1/bio12.tif",
#'   vec_cells = c(1L, 2L)
#' )
#' }
save_masked_raster <- function(
  path_input,
  path_output,
  vec_cells,
  datatype = "FLT4S"
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
    base::is.numeric(vec_cells),
    !base::anyNA(vec_cells),
    base::all(vec_cells == base::round(vec_cells)),
    msg = "`vec_cells` must be whole cell numbers."
  )

  rast_input <-
    terra::rast(path_input)

  n_cells <-
    terra::ncell(rast_input)

  vec_cells_outside <-
    vec_cells[vec_cells < 1 | vec_cells > n_cells]

  if (
    base::length(vec_cells_outside) > 0L
  ) {
    cli::cli_abort(
      c(
        "A cell to mask lies outside the raster of {n_cells} cells.",
        "x" = "{.val {vec_cells_outside}}"
      )
    )
  }

  vec_values <-
    terra::values(rast_input, mat = FALSE)

  vec_values_masked <-
    base::replace(x = vec_values, list = vec_cells, values = NA)

  rast_masked <-
    terra::setValues(x = rast_input, values = vec_values_masked)

  base::dir.create(
    path = base::dirname(path_output),
    recursive = TRUE,
    showWarnings = FALSE
  )

  # Written under a temporary name first, as in save_aggregated_raster()
  path_temporary <-
    stringr::str_c(path_output, ".part")

  terra::writeRaster(
    x = rast_masked,
    filename = path_temporary,
    filetype = "GTiff",
    datatype = datatype,
    overwrite = TRUE
  )

  fs::file_move(path = path_temporary, new_path = path_output)

  res <-
    path_output

  return(res)
}

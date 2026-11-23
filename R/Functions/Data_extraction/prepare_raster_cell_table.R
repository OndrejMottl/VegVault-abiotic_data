#' @title Prepare the cell table of a raster
#' @description
#' Turn a single-layer raster into a table with one row per cell that has
#' a value: the coordinates of the cell centre and the value, as the v1
#' extraction did.
#' @param path_file
#' Character scalar. Path of an existing single-layer raster.
#' @return
#' A tibble with the columns `long` and `lat` (cell-centre coordinates in
#' the units of the raster, double) and `value` (double), one row per cell
#' with a value, in the raster's cell order: row by row from the
#' north-west corner. Missing cells have no row; a raster without any
#' value gives zero rows.
#' @details
#' The coordinates are not rounded and the values are those stored in the
#' file, read as double.
#' @seealso [save_raster_cell_table()]
#' @examples
#' \dontrun{
#' prepare_raster_cell_table(
#'   path_file = "Data/Processed/CHELSA_V2.1/CHELSA_bio1_1981-2010_V.2.1.tif"
#' )
#' }
prepare_raster_cell_table <- function(path_file) {
  assertthat::assert_that(
    rlang::is_string(path_file),
    base::file.exists(path_file),
    msg = "`path_file` must be the path to an existing raster."
  )

  rast_input <-
    terra::rast(path_file)

  assertthat::assert_that(
    terra::nlyr(rast_input) == 1L,
    msg = "`path_file` must be a raster with one layer."
  )

  vec_values <-
    terra::values(rast_input, mat = FALSE)

  vec_cells <-
    base::which(!base::is.na(vec_values))

  mat_coords <-
    terra::xyFromCell(object = rast_input, cell = vec_cells)

  res <-
    tibble::tibble(
      long = base::as.numeric(mat_coords[, "x"]),
      lat = base::as.numeric(mat_coords[, "y"]),
      value = base::as.numeric(vec_values[vec_cells])
    )

  return(res)
}

#' @title Build a small global raster file for the quality-check tests
#' @description
#' Write a one-layer GeoTIFF with a global extent (-180 to 180, -90 to 90)
#' whose cell values come from a matrix, so tests can place zeros, missing
#' values and type-limit values in known latitude bands.
#' @param path_file
#' Character scalar. Path of the GeoTIFF to write.
#' @param mat_values
#' Numeric matrix. Row 1 is the northernmost raster row, column 1 the
#' westernmost column. `NA` marks missing cells.
#' @param datatype
#' Character scalar. `{terra}` storage type, for example `"FLT4S"` or
#' `"INT2U"`. Default `"FLT4S"`.
#' @param scale
#' Numeric scalar. Scale factor written to the file. Default `1`.
#' @param offset
#' Numeric scalar. Offset written to the file. Default `0`.
#' @return
#' `path_file`, invisibly.
#' @details
#' Integer files are written without a NoData value, so their maximum
#' stored value (6553.5 at scale 0.1) stays a value; such files must not
#' contain `NA`. Strips are one row high, as in the CHELSA files.
build_quality_raster_fixture <- function(
  path_file,
  mat_values,
  datatype = "FLT4S",
  scale = 1,
  offset = 0
) {
  rast_template <-
    terra::rast(
      nrows = base::nrow(mat_values),
      ncols = base::ncol(mat_values),
      xmin = -180,
      xmax = 180,
      ymin = -90,
      ymax = 90,
      crs = "EPSG:4326"
    )

  # terra fills cells row by row, so the matrix is read row-wise
  rast_values <-
    terra::setValues(
      rast_template,
      base::as.vector(base::t(mat_values))
    )

  flag_integer <-
    stringr::str_detect(datatype, "^INT")

  if (
    isTRUE(flag_integer)
  ) {
    terra::writeRaster(
      x = rast_values,
      filename = path_file,
      datatype = datatype,
      scale = scale,
      offset = offset,
      NAflag = NA,
      overwrite = TRUE,
      gdal = c("BLOCKYSIZE=1", "TILED=NO")
    )
  } else {
    terra::writeRaster(
      x = rast_values,
      filename = path_file,
      datatype = datatype,
      overwrite = TRUE,
      gdal = c("BLOCKYSIZE=1", "TILED=NO")
    )
  }

  res <- path_file

  return(invisible(res))
}

#' @title Save a raster aggregated to the coarser processing grid
#' @description
#' Aggregate a raster by a whole factor in both directions with the median
#' or the mode and save the result as a GeoTIFF, as the v1 processing did,
#' so the coarse grid and its missing cells stay the same.
#' @param path_input
#' Character scalar. Path of the raster to aggregate.
#' @param path_output
#' Character scalar. Path of the GeoTIFF to write; its folder is created
#' when missing. It must differ from `path_input`.
#' @param aggregation_factor
#' Whole number, at least 2. Number of input cells per output cell in each
#' direction.
#' @param aggregation_fun
#' Character scalar, `"median"` or `"modal"`.
#' @param datatype
#' Character scalar. `{terra}` storage type of the output. Default
#' `"FLT4S"`, as in v1.
#' @return
#' `path_output`.
#' @details
#' Uses `terra::aggregate()` with its default handling of missing values:
#' a block with any missing input cell gives a missing output cell, and the
#' blocks that extend beyond the input (when the factor does not divide its
#' rows or columns) are missing too. With the mode, a tie goes to the lower
#' value. The output is written to a temporary file next to `path_output`
#' and then moved into place, so an interrupted run never leaves a partial
#' raster under the final name; an existing `path_output` is replaced, as
#' the stage writes only its own outputs.
#' @seealso [build_processing_files()], [save_masked_raster()]
#' @examples
#' \dontrun{
#' save_aggregated_raster(
#'   path_input = "Data/Input/Paleoclimate/CHELSA_TraCE21k_bio01_20_V1.0.tif",
#'   path_output = "Data/Processed/CHELSA_TraCE21k_V1.0/Aggregated/x.tif",
#'   aggregation_factor = 25,
#'   aggregation_fun = "median"
#' )
#' }
save_aggregated_raster <- function(
  path_input,
  path_output,
  aggregation_factor,
  aggregation_fun,
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
    base::is.numeric(aggregation_factor),
    base::length(aggregation_factor) == 1L,
    base::is.finite(aggregation_factor),
    aggregation_factor >= 2,
    aggregation_factor == base::round(aggregation_factor),
    msg = "`aggregation_factor` must be a whole number of at least 2."
  )

  assertthat::assert_that(
    rlang::is_string(aggregation_fun),
    aggregation_fun %in% c("median", "modal"),
    msg = "`aggregation_fun` must be \"median\" or \"modal\"."
  )

  assertthat::assert_that(
    rlang::is_string(datatype),
    msg = "`datatype` must be one terra storage type."
  )

  rast_input <-
    terra::rast(path_input)

  # terra's default: a block with any missing cell is missing, as in v1
  rast_aggregated <-
    terra::aggregate(
      x = rast_input,
      fact = aggregation_factor,
      fun = aggregation_fun
    )

  base::dir.create(
    path = base::dirname(path_output),
    recursive = TRUE,
    showWarnings = FALSE
  )

  # Written under a temporary name first, so an interrupted run never
  #   leaves a partial raster under the final name
  path_temporary <-
    stringr::str_c(path_output, ".part")

  terra::writeRaster(
    x = rast_aggregated,
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

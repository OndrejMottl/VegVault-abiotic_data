#' @title Save a raster resampled to the grid of a template
#' @description
#' Resample a raster to the rows, columns and extent of a template raster
#' and save the result, as the v1 processing did with the soil map and the
#' CHELSA V2.1 grid.
#' @param path_input
#' Character scalar. Path of the raster to resample.
#' @param path_template
#' Character scalar. Path of the raster whose grid the output takes.
#' @param path_output
#' Character scalar. Path of the GeoTIFF to write; its folder is created
#' when missing. It must differ from both other paths.
#' @param method
#' Character scalar. `terra::resample()` method; default `"near"`, which
#' keeps classes.
#' @param datatype
#' Character scalar. `{terra}` storage type of the output, for example
#' `"INT1U"`.
#' @return
#' `path_output`.
#' @details
#' Aborts when the output does not get the template's dimensions, extent
#' and origin. The output is written to a temporary file and moved into
#' place, as in `save_aggregated_raster()`.
#' @seealso [save_classified_raster()], [save_aggregated_raster()]
#' @examples
#' \dontrun{
#' save_resampled_raster(
#'   path_input = "Data/Processed/HWSD_v2.2/Intermediate/classes.tif",
#'   path_template = "Data/Input/Neoclimate/CHELSA_V2.1/bio1.tif",
#'   path_output = "Data/Processed/HWSD_v2.2/Intermediate/resampled.tif",
#'   datatype = "INT1U"
#' )
#' }
save_resampled_raster <- function(
  path_input,
  path_template,
  path_output,
  method = "near",
  datatype
) {
  assertthat::assert_that(
    rlang::is_string(path_input),
    base::file.exists(path_input),
    msg = "`path_input` must be the path to an existing raster."
  )

  assertthat::assert_that(
    rlang::is_string(path_template),
    base::file.exists(path_template),
    msg = "`path_template` must be the path to an existing raster."
  )

  vec_paths_in <-
    base::normalizePath(c(path_input, path_template), winslash = "/")

  assertthat::assert_that(
    rlang::is_string(path_output),
    base::nzchar(path_output),
    !(base::normalizePath(path_output, winslash = "/", mustWork = FALSE) %in%
      vec_paths_in),
    msg = stringr::str_c(
      "`path_output` must be one path that differs from `path_input` and ",
      "`path_template`."
    )
  )

  assertthat::assert_that(
    rlang::is_string(method),
    rlang::is_string(datatype),
    msg = "`method` and `datatype` must be single strings."
  )

  rast_template <-
    terra::rast(path_template)

  base::dir.create(
    path = base::dirname(path_output),
    recursive = TRUE,
    showWarnings = FALSE
  )

  # Written under a temporary name first, as in save_aggregated_raster()
  path_temporary <-
    stringr::str_c(path_output, ".part")

  rast_resampled <-
    terra::resample(
      x = terra::rast(path_input),
      y = rast_template,
      method = method,
      filename = path_temporary,
      overwrite = TRUE,
      wopt = base::list(filetype = "GTiff", datatype = datatype)
    )

  flag_same_grid <-
    base::identical(base::dim(rast_resampled), base::dim(rast_template)) &&
    base::isTRUE(
      base::all.equal(
        base::as.vector(terra::ext(rast_resampled)),
        base::as.vector(terra::ext(rast_template))
      )
    )

  if (
    !flag_same_grid
  ) {
    base::file.remove(path_temporary)

    cli::cli_abort(
      "The resampled raster does not get the grid of {.file {path_template}}."
    )
  }

  fs::file_move(path = path_temporary, new_path = path_output)

  res <-
    path_output

  return(res)
}

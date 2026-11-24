#' @title Build the cells of a precipitation artefact to mask
#' @description
#' Find the aggregated cells and ages where a detector variable is
#' inflated against its own values in the other ages: more than a ratio of
#' the cell's median over all ages and more than a fixed amount above it,
#' inside a region and in listed ages.
#' @param data_processing_files
#' A data frame as returned by `build_processing_files()`, holding every
#' age of the detector variable, with the columns `variable`, `age` and
#' `path_aggregated`.
#' @param list_artefact_mask
#' A list with `detector` (variable name), `ages` (numeric vector of the
#' affected ages), `region` (a list with `lon_min`, `lon_max`, `lat_min`
#' and `lat_max` of the cell centres), `ratio_min` and `increase_min`
#' (positive numbers).
#' @return
#' A tibble with the columns `age`, `cell` (integer), `long`, `lat`,
#' `value` and `median_all_ages`, one row per flagged cell and age, sorted
#' by `age` and `cell`.
#' @details
#' The median of a cell is taken over every age of the detector in
#' `data_processing_files`, ignoring missing values. A cell is flagged in
#' an affected age when its value is above `ratio_min` times the median
#' (the median taken as at least 1) and above the median plus
#' `increase_min`. Aborts when the detector misses an affected age or its
#' rasters do not share one grid.
#' @seealso [prepare_processing_mask_cells()], [save_masked_raster()]
#' @examples
#' \dontrun{
#' build_artefact_mask_cells(
#'   data_processing_files = data_processing_files,
#'   list_artefact_mask = list(
#'     detector = "bio12",
#'     ages = c(12000, 12500),
#'     region = list(lon_min = -80, lon_max = -35, lat_min = -25,
#'       lat_max = 5),
#'     ratio_min = 2,
#'     increase_min = 100
#'   )
#' )
#' }
build_artefact_mask_cells <- function(
  data_processing_files,
  list_artefact_mask
) {
  vec_columns_required <-
    c("variable", "age", "path_aggregated")

  assertthat::assert_that(
    base::is.data.frame(data_processing_files),
    base::all(
      vec_columns_required %in% base::colnames(data_processing_files)
    ),
    msg = stringr::str_c(
      "`data_processing_files` must be a data frame with the columns ",
      stringr::str_c(vec_columns_required, collapse = ", "),
      "."
    )
  )

  assertthat::assert_that(
    base::is.list(list_artefact_mask),
    rlang::is_string(list_artefact_mask[["detector"]]),
    base::is.numeric(list_artefact_mask[["ages"]]),
    base::length(list_artefact_mask[["ages"]]) > 0L,
    msg = stringr::str_c(
      "`list_artefact_mask` needs `detector` as one variable name and ",
      "`ages` as numbers."
    )
  )

  list_region <-
    list_artefact_mask[["region"]]

  vec_region_fields <-
    c("lon_min", "lon_max", "lat_min", "lat_max")

  assertthat::assert_that(
    base::is.list(list_region),
    base::all(
      purrr::map_lgl(
        .x = vec_region_fields,
        .f = ~ base::is.numeric(list_region[[.x]]) &&
          base::length(list_region[[.x]]) == 1L &&
          base::is.finite(list_region[[.x]])
      )
    ),
    msg = stringr::str_c(
      "The `region` of `list_artefact_mask` needs `lon_min`, `lon_max`, ",
      "`lat_min` and `lat_max` as numbers."
    )
  )

  assertthat::assert_that(
    list_region[["lon_min"]] < list_region[["lon_max"]],
    list_region[["lat_min"]] < list_region[["lat_max"]],
    msg = "The `region` of `list_artefact_mask` must have min below max."
  )

  purrr::walk(
    .x = c("ratio_min", "increase_min"),
    .f = ~ assertthat::assert_that(
      base::is.numeric(list_artefact_mask[[.x]]),
      base::length(list_artefact_mask[[.x]]) == 1L,
      base::is.finite(list_artefact_mask[[.x]]),
      list_artefact_mask[[.x]] > 0,
      msg = stringr::str_c(
        "`", .x, "` of `list_artefact_mask` must be a positive number."
      )
    )
  )

  data_detector <-
    data_processing_files |>
    dplyr::filter(
      .data[["variable"]] == list_artefact_mask[["detector"]]
    ) |>
    dplyr::arrange(.data[["age"]])

  vec_ages_missing <-
    base::setdiff(
      list_artefact_mask[["ages"]],
      dplyr::pull(data_detector, "age")
    )

  if (
    base::length(vec_ages_missing) > 0L
  ) {
    cli::cli_abort(
      c(
        stringr::str_c(
          "The detector {.val {list_artefact_mask[['detector']]}} has no ",
          "file for an age the mask lists."
        ),
        "x" = "{.val {vec_ages_missing}}"
      )
    )
  }

  vec_paths <-
    dplyr::pull(data_detector, "path_aggregated")

  list_rasters <-
    purrr::map(
      .x = vec_paths,
      .f = ~ terra::rast(.x)
    )

  vec_flag_same_grid <-
    purrr::map_lgl(
      .x = list_rasters,
      .f = ~ base::identical(base::dim(.x), base::dim(list_rasters[[1]])) &&
        base::isTRUE(
          base::all.equal(
            base::as.vector(terra::ext(.x)),
            base::as.vector(terra::ext(list_rasters[[1]]))
          )
        )
    )

  if (
    !base::all(vec_flag_same_grid)
  ) {
    cli::cli_abort(
      "The detector rasters do not share one grid: {.file {vec_paths}}."
    )
  }

  rast_stack <-
    terra::rast(list_rasters)

  # The median of every cell over all ages, ignoring missing values
  vec_median <-
    terra::app(x = rast_stack, fun = "median", na.rm = TRUE) |>
    terra::values(mat = FALSE)

  mat_xy <-
    terra::xyFromCell(
      list_rasters[[1]],
      base::seq_len(terra::ncell(list_rasters[[1]]))
    )

  vec_flag_region <-
    mat_xy[, 1] >= list_region[["lon_min"]] &
    mat_xy[, 1] <= list_region[["lon_max"]] &
    mat_xy[, 2] >= list_region[["lat_min"]] &
    mat_xy[, 2] <= list_region[["lat_max"]]

  vec_ages_masked <-
    list_artefact_mask[["ages"]]

  # One column per listed age, one row per cell; the cell vectors recycle
  #   down every column
  mat_values <-
    terra::values(
      rast_stack[[
        base::match(vec_ages_masked, dplyr::pull(data_detector, "age"))
      ]],
      mat = TRUE
    )

  mat_flag <-
    vec_flag_region &
    !base::is.na(mat_values) &
    !base::is.na(vec_median) &
    mat_values >
      list_artefact_mask[["ratio_min"]] * base::pmax(vec_median, 1) &
    mat_values > vec_median + list_artefact_mask[["increase_min"]]

  mat_index <-
    base::which(mat_flag, arr.ind = TRUE)

  vec_cells <-
    base::as.integer(mat_index[, 1])

  res <-
    tibble::tibble(
      age = base::as.numeric(vec_ages_masked[mat_index[, 2]]),
      cell = vec_cells,
      long = mat_xy[vec_cells, 1],
      lat = mat_xy[vec_cells, 2],
      value = mat_values[mat_index],
      median_all_ages = vec_median[vec_cells]
    ) |>
    dplyr::arrange(.data[["age"]], .data[["cell"]])

  return(res)
}

#' @title Compute the aggregated cells that hold a raw fill value
#' @description
#' Find the raw cells south of a latitude that hold a fill value and return
#' the cells of the aggregated grid whose blocks contain them. Setting
#' those aggregated cells to missing equals setting the raw cells to
#' missing before an aggregation that leaves a block with any missing cell
#' missing, as `save_aggregated_raster()` does.
#' @param path_file
#' Character scalar. Path of the raw raster.
#' @param fill_value
#' Numeric scalar. The fill value, compared with a relative tolerance of
#' `1e-6`.
#' @param lat_max
#' Numeric scalar. Only raster rows whose centre lies south of this
#' latitude are read.
#' @param aggregation_factor
#' Whole number, at least 2, as in `save_aggregated_raster()`.
#' @return
#' A tibble with the columns `cell` (integer, cell number of the
#' aggregated grid) and `n_raw_cells` (integer, raw fill cells in its
#' block), sorted by `cell`; zero rows when no cell holds the fill value.
#' @details
#' Reads only the rows south of `lat_max`. The aggregated grid has
#' `ceiling(n_row / aggregation_factor)` rows and
#' `ceiling(n_col / aggregation_factor)` columns, numbered by row from the
#' north-west corner, as in `{terra}`.
#' @seealso [save_aggregated_raster()], [prepare_processing_mask_cells()]
#' @examples
#' \dontrun{
#' compute_fill_value_cells(
#'   path_file = "Data/Input/Neoclimate/CHELSA_V2.1/bio12.tif",
#'   fill_value = 6553.5,
#'   lat_max = -89.975,
#'   aggregation_factor = 25
#' )
#' }
compute_fill_value_cells <- function(
  path_file,
  fill_value,
  lat_max,
  aggregation_factor
) {
  assertthat::assert_that(
    rlang::is_string(path_file),
    base::file.exists(path_file),
    msg = "`path_file` must be the path to an existing raster."
  )

  assertthat::assert_that(
    base::is.numeric(fill_value),
    base::length(fill_value) == 1L,
    base::is.finite(fill_value),
    msg = "`fill_value` must be one finite number."
  )

  assertthat::assert_that(
    base::is.numeric(lat_max),
    base::length(lat_max) == 1L,
    base::is.finite(lat_max),
    msg = "`lat_max` must be one finite latitude."
  )

  assertthat::assert_that(
    base::is.numeric(aggregation_factor),
    base::length(aggregation_factor) == 1L,
    base::is.finite(aggregation_factor),
    aggregation_factor >= 2,
    aggregation_factor == base::round(aggregation_factor),
    msg = "`aggregation_factor` must be a whole number of at least 2."
  )

  rast_raw <-
    terra::rast(path_file)

  n_row <-
    terra::nrow(rast_raw)

  n_col <-
    terra::ncol(rast_raw)

  vec_lat_rows <-
    terra::yFromRow(rast_raw, base::seq_len(n_row))

  vec_rows_south <-
    base::which(vec_lat_rows < lat_max)

  data_none <-
    tibble::tibble(
      cell = base::integer(0),
      n_raw_cells = base::integer(0)
    )

  if (
    base::length(vec_rows_south) == 0L
  ) {
    return(data_none)
  }

  row_first <-
    base::min(vec_rows_south)

  n_rows_read <-
    n_row - row_first + 1L

  # Only the rows south of lat_max, as one block of the file
  vec_values <-
    terra::values(
      rast_raw,
      row = row_first,
      nrows = n_rows_read,
      mat = FALSE
    )

  vec_index_fill <-
    base::which(
      !base::is.na(vec_values) &
        base::abs(vec_values - fill_value) <=
          1e-6 * base::max(1, base::abs(fill_value))
    )

  if (
    base::length(vec_index_fill) == 0L
  ) {
    return(data_none)
  }

  # The values of the block run by row
  vec_row_raw <-
    row_first + (vec_index_fill - 1L) %/% n_col

  vec_col_raw <-
    (vec_index_fill - 1L) %% n_col + 1L

  n_col_aggregated <-
    base::ceiling(n_col / aggregation_factor)

  vec_cell_aggregated <-
    base::as.integer(
      (base::ceiling(vec_row_raw / aggregation_factor) - 1) *
        n_col_aggregated +
        base::ceiling(vec_col_raw / aggregation_factor)
    )

  res <-
    tibble::tibble(cell = vec_cell_aggregated) |>
    dplyr::summarise(
      n_raw_cells = base::as.integer(dplyr::n()),
      .by = "cell"
    ) |>
    dplyr::arrange(.data[["cell"]])

  return(res)
}

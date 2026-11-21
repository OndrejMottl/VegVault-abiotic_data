#' @title Compute a cell-by-cell comparison of two rasters per band
#' @description
#' Read two one-layer rasters on the same grid in blocks of rows and count,
#' per latitude band, the cells where the first raster exceeds the
#' reference raster and the cells that are missing in one raster but not
#' in the other. Used for rules between variables, for example that the
#' precipitation of a quarter (bio18) cannot exceed the annual
#' precipitation (bio12) and is not missing where the annual value exists.
#' @param path_file
#' Character scalar. Path of the raster to check.
#' @param path_file_reference
#' Character scalar. Path of the reference raster on the same grid.
#' @param band_width
#' Positive number. Height of a latitude band in degrees, with band edges
#' at multiples of `band_width`, as in `compute_raster_band_summary()`.
#' Default `1`.
#' @param n_rows_block
#' Whole number, at least 1. Number of raster rows read at a time.
#' Default `240`.
#' @param value_tolerance
#' Positive number. A cell exceeds the reference when its value is larger
#' than the reference by more than `value_tolerance` times the larger of 1
#' and the absolute reference value. Default `1e-6`.
#' @return
#' A tibble with one row per latitude band, from north to south, and the
#' columns: `file_name`, `file_name_reference` (base names), `band_south`,
#' `band_north`, `n_cells`, `n_both_valid`, `n_exceeding`,
#' `n_missing_only` (missing in the file, present in the reference),
#' `n_reference_missing_only`, `share_exceeding` (of the cells valid in
#' both; `NA` when there are none), `share_missing_only` (of the cells
#' valid in the reference; `NA` when there are none) and `read_error`.
#' @details
#' The two rasters must have the same number of rows and columns and the
#' same extent (within 1e-9 degrees). A missing or unreadable file, a file
#' with more than one layer, or different grids do not abort: the function
#' returns one row with `read_error` set and the band columns missing, so
#' the quality check fails closed on that pair.
#' @seealso [compute_raster_band_summary()], [diagnose_raster_pairs()]
#' @examples
#' \dontrun{
#' compute_raster_pair_comparison(
#'   path_file = "CHELSA_bio18_1981-2010_V.2.1.tif",
#'   path_file_reference = "CHELSA_bio12_1981-2010_V.2.1.tif",
#'   band_width = 1
#' )
#' }
compute_raster_pair_comparison <- function(
  path_file,
  path_file_reference,
  band_width = 1,
  n_rows_block = 240L,
  value_tolerance = 1e-6
) {
  assertthat::assert_that(
    base::is.character(path_file),
    base::length(path_file) == 1L,
    !base::is.na(path_file),
    msg = "`path_file` must be a single file path."
  )

  assertthat::assert_that(
    base::is.character(path_file_reference),
    base::length(path_file_reference) == 1L,
    !base::is.na(path_file_reference),
    msg = "`path_file_reference` must be a single file path."
  )

  assertthat::assert_that(
    base::is.numeric(band_width),
    base::length(band_width) == 1L,
    base::is.finite(band_width),
    band_width > 0,
    msg = "`band_width` must be a positive number."
  )

  assertthat::assert_that(
    base::is.numeric(n_rows_block),
    base::length(n_rows_block) == 1L,
    !base::is.na(n_rows_block),
    n_rows_block >= 1,
    n_rows_block == base::round(n_rows_block),
    msg = "`n_rows_block` must be a whole number of at least 1."
  )

  assertthat::assert_that(
    base::is.numeric(value_tolerance),
    base::length(value_tolerance) == 1L,
    base::is.finite(value_tolerance),
    value_tolerance > 0,
    msg = "`value_tolerance` must be a positive number."
  )

  # Row returned when the pair cannot be compared
  data_unread <-
    tibble::tibble(
      file_name = base::basename(path_file),
      file_name_reference = base::basename(path_file_reference),
      band_south = NA_real_,
      band_north = NA_real_,
      n_cells = NA_real_,
      n_both_valid = NA_real_,
      n_exceeding = NA_real_,
      n_missing_only = NA_real_,
      n_reference_missing_only = NA_real_,
      share_exceeding = NA_real_,
      share_missing_only = NA_real_,
      read_error = NA_character_
    )

  vec_missing_files <-
    c(path_file, path_file_reference)[
      !base::file.exists(c(path_file, path_file_reference))
    ]

  if (
    base::length(vec_missing_files) > 0L
  ) {
    res <-
      dplyr::mutate(
        data_unread,
        read_error = stringr::str_c(
          "The file does not exist: ",
          stringr::str_c(base::basename(vec_missing_files), collapse = ", "),
          "."
        )
      )

    return(res)
  }

  # A pair that cannot be compared is reported, not raised, so a check
  #   over many pairs fails closed on this one pair
  res <-
    tryCatch(
      expr = {
        rast_file <-
          terra::rast(path_file)

        rast_reference <-
          terra::rast(path_file_reference)

        vec_n_layers <-
          c(terra::nlyr(rast_file), terra::nlyr(rast_reference))

        if (
          base::any(vec_n_layers != 1L)
        ) {
          cli::cli_abort("Both rasters must have one layer.")
        }

        vec_extent_difference <-
          base::abs(
            base::as.vector(terra::ext(rast_file)) -
              base::as.vector(terra::ext(rast_reference))
          )

        if (
          terra::ncol(rast_file) != terra::ncol(rast_reference) ||
            terra::nrow(rast_file) != terra::nrow(rast_reference) ||
            base::any(vec_extent_difference > 1e-9)
        ) {
          cli::cli_abort("The two rasters are not on the same grid.")
        }

        n_col <-
          terra::ncol(rast_file)

        n_row <-
          terra::nrow(rast_file)

        vec_block_starts <-
          base::seq(from = 1, to = n_row, by = n_rows_block)

        # Counts per raster row, one block of rows of both rasters at a time
        data_rows <-
          vec_block_starts |>
          purrr::map(
            .f = ~ {
              row_start <- .x

              n_rows_read <-
                base::min(n_rows_block, n_row - row_start + 1)

              mat_values <-
                terra::values(
                  rast_file,
                  row = row_start,
                  nrows = n_rows_read,
                  mat = FALSE
                ) |>
                base::matrix(nrow = n_col)

              mat_reference <-
                terra::values(
                  rast_reference,
                  row = row_start,
                  nrows = n_rows_read,
                  mat = FALSE
                ) |>
                base::matrix(nrow = n_col)

              mat_valid <-
                !base::is.na(mat_values)

              mat_reference_valid <-
                !base::is.na(mat_reference)

              mat_exceeding <-
                mat_valid &
                mat_reference_valid &
                (mat_values - mat_reference) >
                  value_tolerance * base::pmax(1, base::abs(mat_reference))

              tibble::tibble(
                row = row_start + base::seq_len(n_rows_read) - 1,
                n_cells = n_col,
                n_both_valid = base::colSums(mat_valid & mat_reference_valid),
                n_exceeding = base::colSums(mat_exceeding, na.rm = TRUE),
                n_missing_only = base::colSums(
                  !mat_valid & mat_reference_valid
                ),
                n_reference_missing_only = base::colSums(
                  mat_valid & !mat_reference_valid
                ),
                n_reference_valid = base::colSums(mat_reference_valid)
              )
            }
          ) |>
          purrr::list_rbind()

        # Fixed band edges at multiples of band_width, as in
        #   compute_raster_band_summary()
        data_rows |>
          dplyr::mutate(
            lat = terra::yFromRow(rast_file, .data[["row"]]),
            band_south = base::round(
              base::floor(.data[["lat"]] / band_width) * band_width,
              digits = 10
            ),
            band_north = base::round(
              .data[["band_south"]] + band_width,
              digits = 10
            )
          ) |>
          dplyr::group_by(.data[["band_south"]], .data[["band_north"]]) |>
          dplyr::summarise(
            dplyr::across(
              .cols = c(
                "n_cells", "n_both_valid", "n_exceeding", "n_missing_only",
                "n_reference_missing_only", "n_reference_valid"
              ),
              .fns = base::sum
            ),
            .groups = "drop"
          ) |>
          dplyr::arrange(dplyr::desc(.data[["band_north"]])) |>
          dplyr::mutate(
            file_name = base::basename(path_file),
            file_name_reference = base::basename(path_file_reference),
            share_exceeding = dplyr::if_else(
              .data[["n_both_valid"]] > 0,
              .data[["n_exceeding"]] / .data[["n_both_valid"]],
              NA_real_
            ),
            share_missing_only = dplyr::if_else(
              .data[["n_reference_valid"]] > 0,
              .data[["n_missing_only"]] / .data[["n_reference_valid"]],
              NA_real_
            ),
            read_error = NA_character_
          ) |>
          dplyr::select(dplyr::all_of(base::colnames(data_unread)))
      },
      error = function(error_condition) {
        dplyr::mutate(
          data_unread,
          read_error = stringr::str_squish(
            base::conditionMessage(error_condition)
          )
        )
      }
    )

  return(res)
}

#' @title Compute value statistics of a raster file per latitude band
#' @description
#' Read a one-layer raster file in blocks of rows and summarise its cell
#' values per latitude band: how many cells are missing, exactly zero, or
#' at a storage-type limit, and the range of values. The table is the input
#' of the raster quality rules.
#' @param path_file
#' Character scalar. Path of a one-layer raster file readable by `{terra}`.
#' @param band_width
#' Positive number. Height of a latitude band in degrees. Bands have fixed
#' edges at multiples of `band_width`, so the same band covers the same
#' latitudes in every raster, whatever its resolution. Default `1`.
#' @param vec_type_limit_values
#' Numeric vector of storage-type limit values to count, from
#' `build_raster_type_limit_values()`.
#' @param n_rows_block
#' Whole number, at least 1. Number of raster rows read at a time.
#' Default `240`.
#' @param value_tolerance
#' Positive number. Relative tolerance for matching a type-limit value,
#' large enough for values stored as 32-bit floats. Default `1e-6`.
#' @return
#' A tibble with one row per latitude band, from north to south, and the
#' columns: `file_name` (base name), `n_col`, `n_row`, `x_min`, `x_max`,
#' `y_min`, `y_max`, `data_type` (the `{terra}` storage type, `NA` when it
#' cannot be determined), `scale`, `offset`, `file_size_bytes`,
#' `band_south`, `band_north`, `n_cells`, `n_missing`, `n_zero`,
#' `n_type_limit` (cells equal to any type-limit value), `n_at_file_max`
#' and `n_at_file_min` (cells equal to the largest and the smallest value
#' of the whole file), `value_min`, `value_max`, `share_missing` (of all
#' cells), `share_zero` and `share_type_limit` (of the non-missing cells;
#' `NA` when the band has none) and `read_error` (`NA`, or why the file
#' could not be read).
#' @details
#' Values are read with the file's scale and offset applied, so a raster
#' stored as 16-bit integers with scale 0.1 and one stored as floats give
#' the same statistics for the same values. `n_type_limit` is descriptive:
#' small type-limit values such as 25.5 are also ordinary values, so the
#' quality rule uses the cells at the file's maximum and minimum instead
#' (`diagnose_raster_bands()`). A row belongs to the band that
#' contains the latitude of its centre. The raster is never read as a
#' whole. A missing file, a file that cannot be opened or read, or a file
#' with more than one layer does not abort: it returns one row with
#' `read_error` set and the band columns missing, so a quality check over
#' many files fails closed on that file. File names, not paths, are
#' reported, so the table holds no machine-specific path.
#' @seealso [build_raster_type_limit_values()],
#'   [diagnose_raster_bands()]
#' @examples
#' \dontrun{
#' compute_raster_band_summary(
#'   path_file = "Data/Processed/Neoclimate/CHELSA_bio_04.tif",
#'   band_width = 1,
#'   vec_type_limit_values = build_raster_type_limit_values(
#'     vec_scales = c(1, 0.1, 0.01)
#'   )
#' )
#' }
compute_raster_band_summary <- function(
  path_file,
  band_width = 1,
  vec_type_limit_values,
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
    base::is.numeric(band_width),
    base::length(band_width) == 1L,
    base::is.finite(band_width),
    band_width > 0,
    msg = "`band_width` must be a positive number."
  )

  assertthat::assert_that(
    base::is.numeric(vec_type_limit_values),
    base::all(base::is.finite(vec_type_limit_values)),
    msg = "`vec_type_limit_values` must be a vector of finite numbers."
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

  # Row returned when the file cannot be read; band columns stay missing
  data_unread <-
    tibble::tibble(
      file_name = base::basename(path_file),
      n_col = NA_real_,
      n_row = NA_real_,
      x_min = NA_real_,
      x_max = NA_real_,
      y_min = NA_real_,
      y_max = NA_real_,
      data_type = NA_character_,
      scale = NA_real_,
      offset = NA_real_,
      file_size_bytes = NA_real_,
      band_south = NA_real_,
      band_north = NA_real_,
      n_cells = NA_real_,
      n_missing = NA_real_,
      n_zero = NA_real_,
      n_type_limit = NA_real_,
      n_at_file_max = NA_real_,
      n_at_file_min = NA_real_,
      value_min = NA_real_,
      value_max = NA_real_,
      share_missing = NA_real_,
      share_zero = NA_real_,
      share_type_limit = NA_real_,
      read_error = NA_character_
    )

  if (
    isFALSE(base::file.exists(path_file))
  ) {
    res <-
      dplyr::mutate(
        data_unread,
        read_error = "The file does not exist."
      )

    return(res)
  }

  size_file_bytes <-
    base::file.size(path_file)

  # A file that cannot be read is reported, not raised, so a check over
  #   many files fails closed on this one file
  res <-
    tryCatch(
      expr = {
        rast_file <-
          terra::rast(path_file)

        n_layers <-
          terra::nlyr(rast_file)

        if (
          n_layers != 1L
        ) {
          cli::cli_abort("The raster has {n_layers} layers; expected 1.")
        }

        n_col <-
          terra::ncol(rast_file)

        n_row <-
          terra::nrow(rast_file)

        vec_extent <-
          base::as.vector(terra::ext(rast_file))

        data_type_raw <-
          terra::datatype(rast_file)[1]

        mat_scale_offset <-
          terra::scoff(rast_file)

        vec_block_starts <-
          base::seq(from = 1, to = n_row, by = n_rows_block)

        # Statistics per raster row; values come with scale and offset
        #   applied, one block of rows at a time
        data_rows <-
          vec_block_starts |>
          purrr::map(
            .f = ~ {
              row_start <- .x

              n_rows_read <-
                base::min(n_rows_block, n_row - row_start + 1)

              vec_values <-
                terra::values(
                  rast_file,
                  row = row_start,
                  nrows = n_rows_read,
                  mat = FALSE
                )

              # Each column of the matrix is one raster row
              mat_values <-
                base::matrix(vec_values, nrow = n_col)

              vec_n_valid <-
                base::colSums(!base::is.na(mat_values))

              vec_n_zero <-
                base::colSums(mat_values == 0, na.rm = TRUE)

              vec_range_valid <-
                base::range(vec_values, na.rm = TRUE, finite = TRUE) |>
                base::suppressWarnings()

              # Only limits inside the value range of the block can occur
              vec_candidates <-
                vec_type_limit_values[
                  vec_type_limit_values >=
                    vec_range_valid[1] -
                      value_tolerance * base::abs(vec_type_limit_values) &
                    vec_type_limit_values <=
                      vec_range_valid[2] +
                        value_tolerance * base::abs(vec_type_limit_values)
                ]

              vec_n_type_limit <-
                vec_candidates |>
                purrr::map(
                  .f = ~ base::colSums(
                    base::abs(mat_values - .x) <=
                      value_tolerance * base::abs(.x),
                    na.rm = TRUE
                  )
                ) |>
                purrr::reduce(
                  .f = `+`,
                  .init = base::rep(0, n_rows_read)
                )

              vec_row_max <-
                base::seq_len(n_rows_read) |>
                purrr::map_dbl(
                  .f = ~ base::max(c(-Inf, mat_values[, .x]), na.rm = TRUE)
                )

              vec_row_min <-
                base::seq_len(n_rows_read) |>
                purrr::map_dbl(
                  .f = ~ base::min(c(Inf, mat_values[, .x]), na.rm = TRUE)
                )

              vec_n_at_row_max <-
                base::colSums(
                  mat_values == base::rep(vec_row_max, each = n_col),
                  na.rm = TRUE
                )

              vec_n_at_row_min <-
                base::colSums(
                  mat_values == base::rep(vec_row_min, each = n_col),
                  na.rm = TRUE
                )

              tibble::tibble(
                row = row_start + base::seq_len(n_rows_read) - 1,
                n_cells = n_col,
                n_missing = n_col - vec_n_valid,
                n_zero = vec_n_zero,
                n_type_limit = vec_n_type_limit,
                row_min = vec_row_min,
                row_max = vec_row_max,
                n_at_row_max = vec_n_at_row_max,
                n_at_row_min = vec_n_at_row_min
              )
            }
          ) |>
          purrr::list_rbind()

        # Fixed band edges at multiples of band_width; rounding removes
        #   floating-point noise from the edges
        data_rows_banded <-
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
          )

        data_bands <-
          data_rows_banded |>
          dplyr::group_by(.data[["band_south"]], .data[["band_north"]]) |>
          dplyr::summarise(
            band_max = base::max(c(-Inf, .data[["row_max"]])),
            n_at_band_max = base::sum(
              .data[["n_at_row_max"]][
                .data[["row_max"]] == base::max(c(-Inf, .data[["row_max"]]))
              ]
            ),
            n_at_band_min = base::sum(
              .data[["n_at_row_min"]][
                .data[["row_min"]] == base::min(c(Inf, .data[["row_min"]]))
              ]
            ),
            band_min = base::min(c(Inf, .data[["row_min"]])),
            n_cells = base::sum(.data[["n_cells"]]),
            n_missing = base::sum(.data[["n_missing"]]),
            n_zero = base::sum(.data[["n_zero"]]),
            n_type_limit = base::sum(.data[["n_type_limit"]]),
            .groups = "drop"
          ) |>
          dplyr::arrange(dplyr::desc(.data[["band_north"]]))

        file_max <-
          base::max(c(-Inf, dplyr::pull(data_bands, "band_max")))

        file_min <-
          base::min(c(Inf, dplyr::pull(data_bands, "band_min")))

        data_type <-
          dplyr::if_else(
            base::nzchar(data_type_raw),
            data_type_raw,
            NA_character_
          )

        data_bands |>
          dplyr::mutate(
            file_name = base::basename(path_file),
            n_col = n_col,
            n_row = n_row,
            x_min = vec_extent[1],
            x_max = vec_extent[2],
            y_min = vec_extent[3],
            y_max = vec_extent[4],
            data_type = data_type,
            scale = mat_scale_offset[1, 1],
            offset = mat_scale_offset[1, 2],
            file_size_bytes = size_file_bytes,
            n_at_file_max = dplyr::if_else(
              .data[["band_max"]] == file_max &
                base::is.finite(.data[["band_max"]]),
              .data[["n_at_band_max"]],
              0
            ),
            n_at_file_min = dplyr::if_else(
              .data[["band_min"]] == file_min &
                base::is.finite(.data[["band_min"]]),
              .data[["n_at_band_min"]],
              0
            ),
            value_max = dplyr::na_if(.data[["band_max"]], -Inf),
            value_min = dplyr::na_if(.data[["band_min"]], Inf),
            n_valid = .data[["n_cells"]] - .data[["n_missing"]],
            share_missing = .data[["n_missing"]] / .data[["n_cells"]],
            share_zero = dplyr::if_else(
              .data[["n_valid"]] > 0,
              .data[["n_zero"]] / .data[["n_valid"]],
              NA_real_
            ),
            share_type_limit = dplyr::if_else(
              .data[["n_valid"]] > 0,
              .data[["n_type_limit"]] / .data[["n_valid"]],
              NA_real_
            ),
            read_error = NA_character_
          ) |>
          dplyr::select(dplyr::all_of(base::colnames(data_unread)))
      },
      error = function(error_condition) {
        dplyr::mutate(
          data_unread,
          file_size_bytes = size_file_bytes,
          read_error = stringr::str_squish(
            base::conditionMessage(error_condition)
          )
        )
      }
    )

  return(res)
}

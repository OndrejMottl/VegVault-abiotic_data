#' @title Build a minimal TIFF file with a known block layout
#' @description
#' Write an uncompressed 8-bit TIFF whose header and block table come
#' first and whose pixel blocks follow in order, as in the CHELSA files,
#' and optionally cut the file short, so tests know exactly which blocks
#' and rows lie beyond the end of the file.
#' @param path_file
#' Character scalar. Path of the TIFF to write.
#' @param n_rows
#' Integer scalar. Image length in rows.
#' @param n_cols
#' Integer scalar. Image width in columns.
#' @param rows_per_block
#' Integer scalar. Rows per strip, or the tile length and width when
#' `flag_tiled` is `TRUE` (a multiple of 16). Default `1`.
#' @param flag_bigtiff
#' Logical. Write a BigTIFF instead of a classic TIFF. Default `FALSE`.
#' @param flag_tiled
#' Logical. Use square tiles instead of strips. Default `FALSE`.
#' @param n_bytes_keep
#' Numeric scalar or `NULL`. Keep only the first `n_bytes_keep` bytes.
#' Default `NULL` keeps the whole file.
#' @return
#' A list with `path_file`, `n_bytes_header` (the offset of the first
#' block), `n_bytes_block` (bytes per block), `n_blocks` and
#' `n_bytes_total` (size of the complete file), invisibly.
#' @details
#' Tags are written in increasing order with `LONG` values (`LONG8` block
#' offsets in a BigTIFF). The block offset and byte-count arrays are always
#' stored outside the IFD, so use at least three blocks (the arrays of one
#' or two blocks would have to be stored inline). Block contents are filled
#' with byte value 1.
build_tiff_block_fixture <- function(
  path_file,
  n_rows,
  n_cols,
  rows_per_block = 1L,
  flag_bigtiff = FALSE,
  flag_tiled = FALSE,
  n_bytes_keep = NULL
) {
  n_blocks_across <-
    dplyr::if_else(
      flag_tiled,
      base::ceiling(n_cols / rows_per_block),
      1
    )

  n_blocks <-
    base::ceiling(n_rows / rows_per_block) * n_blocks_across

  n_bytes_block <-
    dplyr::if_else(
      flag_tiled,
      rows_per_block * rows_per_block,
      rows_per_block * n_cols
    )

  # Sizes of the IFD fields: classic TIFF and BigTIFF
  n_bytes_entry <-
    dplyr::if_else(flag_bigtiff, 20, 12)

  n_bytes_count <-
    dplyr::if_else(flag_bigtiff, 8, 2)

  n_bytes_pointer <-
    dplyr::if_else(flag_bigtiff, 8, 4)

  n_bytes_start <-
    dplyr::if_else(flag_bigtiff, 16, 8)

  vec_tags <-
    c(256, 257, 258, 259, 262, 273, 277, 278, 279)

  vec_tags_tiled <-
    c(256, 257, 258, 259, 262, 277, 322, 323, 324, 325)

  vec_tags_selected <-
    base::switch(
      base::as.character(flag_tiled),
      "TRUE" = vec_tags_tiled,
      "FALSE" = vec_tags
    )

  n_entries <-
    base::length(vec_tags_selected)

  # IFD (count, entries, next-IFD pointer), then the two block arrays
  offset_array_offsets <-
    n_bytes_start +
    n_bytes_count +
    n_entries * n_bytes_entry +
    n_bytes_pointer

  offset_array_counts <-
    offset_array_offsets + n_blocks * n_bytes_pointer

  n_bytes_header <-
    offset_array_counts + n_blocks * 4

  vec_block_offsets <-
    n_bytes_header + (base::seq_len(n_blocks) - 1) * n_bytes_block

  list_values <-
    list(
      `256` = n_cols,
      `257` = n_rows,
      `258` = 8,
      `259` = 1,
      `262` = 1,
      `273` = offset_array_offsets,
      `277` = 1,
      `278` = rows_per_block,
      `279` = offset_array_counts,
      `322` = rows_per_block,
      `323` = rows_per_block,
      `324` = offset_array_offsets,
      `325` = offset_array_counts
    )

  con <-
    base::file(path_file, open = "wb")

  base::writeBin(base::charToRaw("II"), con)

  if (
    isTRUE(flag_bigtiff)
  ) {
    base::writeBin(c(43L, 8L, 0L), con, size = 2, endian = "little")
    base::writeBin(c(16L, 0L), con, size = 4, endian = "little")
    base::writeBin(
      c(base::as.integer(n_entries), 0L),
      con,
      size = 4,
      endian = "little"
    )
  } else {
    base::writeBin(42L, con, size = 2, endian = "little")
    base::writeBin(8L, con, size = 4, endian = "little")
    base::writeBin(
      base::as.integer(n_entries),
      con,
      size = 2,
      endian = "little"
    )
  }

  purrr::walk(
    .x = vec_tags_selected,
    .f = ~ {
      flag_array <-
        .x %in% c(273, 279, 324, 325)

      n_values <-
        dplyr::if_else(flag_array, n_blocks, 1)

      # Block offsets are LONG8 in a BigTIFF; everything else is LONG
      type_tag <-
        dplyr::if_else(
          flag_bigtiff && .x %in% c(273, 324),
          16L,
          4L
        )

      value_field <-
        purrr::chuck(list_values, base::as.character(.x))

      base::writeBin(
        c(base::as.integer(.x), type_tag),
        con,
        size = 2,
        endian = "little"
      )

      # Count and value: one LONG8 each in a BigTIFF, one LONG each
      #   otherwise (the high halves of the LONG8 values are zero)
      vec_fields <-
        base::as.integer(c(n_values, value_field))

      if (
        isTRUE(flag_bigtiff)
      ) {
        base::writeBin(
          c(vec_fields[1], 0L, vec_fields[2], 0L),
          con,
          size = 4,
          endian = "little"
        )
      } else {
        base::writeBin(vec_fields, con, size = 4, endian = "little")
      }
    }
  )

  # Next-IFD pointer: zero, there is only one image
  base::writeBin(
    base::rep(0L, n_bytes_pointer / 4),
    con,
    size = 4,
    endian = "little"
  )

  vec_offsets_written <-
    if (
      isTRUE(flag_bigtiff)
    ) {
      base::as.vector(
        base::rbind(base::as.integer(vec_block_offsets), 0L)
      )
    } else {
      base::as.integer(vec_block_offsets)
    }

  base::writeBin(vec_offsets_written, con, size = 4, endian = "little")

  base::writeBin(
    base::rep(base::as.integer(n_bytes_block), n_blocks),
    con,
    size = 4,
    endian = "little"
  )

  base::writeBin(
    base::rep(base::as.raw(1), n_blocks * n_bytes_block),
    con
  )

  base::close(con)

  n_bytes_total <-
    n_bytes_header + n_blocks * n_bytes_block

  if (
    !base::is.null(n_bytes_keep)
  ) {
    vec_bytes <-
      base::readBin(path_file, what = "raw", n = n_bytes_total)

    base::writeBin(vec_bytes[base::seq_len(n_bytes_keep)], path_file)
  }

  res <-
    list(
      path_file = path_file,
      n_bytes_header = n_bytes_header,
      n_bytes_block = n_bytes_block,
      n_blocks = n_blocks,
      n_bytes_total = n_bytes_total
    )

  return(invisible(res))
}

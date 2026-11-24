#' @title Compute whether the blocks of a TIFF file fit inside the file
#' @description
#' Read the first image directory of a TIFF or BigTIFF file and compare
#' the end of every data block (strip or tile), as the block table
#' declares it, with the size of the file on disk. A truncated download
#' keeps its header but loses the blocks at its end; GDAL then reads the
#' missing blocks as zeros or fails.
#' @param path_file
#' Character scalar. Path of the TIFF file.
#' @return
#' A one-row tibble with the columns `file_name` (base name),
#' `file_size_bytes`, `tiff_format` (`"classic"` or `"bigtiff"`),
#' `block_layout` (`"strip"` or `"tile"`), `image_width`, `image_length`,
#' `rows_per_block` (rows per strip, or the tile length), `n_blocks`,
#' `declared_end_bytes` (the largest block offset plus byte count),
#' `n_blocks_beyond_end` (blocks that end after the end of the file),
#' `first_affected_row` (the first image row, counted from 0, of the first
#' such block; `NA` when there is none) and `read_error` (`NA`, or the
#' reason the header could not be read).
#' @details
#' Only the header and the block tables are read, never pixel data, so the
#' check is cheap even for multi-gigabyte rasters. Blocks with a byte count
#' of 0 (sparse files) are ignored. A missing file, a file that is not a
#' TIFF, or a header that cannot be read does not abort: it returns a row
#' with `read_error` set, so a quality check that maps over many files
#' fails closed on that file instead of stopping. Files whose block table
#' sits at the end of the file (as GDAL writes them by default) lose it
#' when truncated, which also gives a `read_error`.
#' @seealso [load_tiff_unsigned_values()]
#' @examples
#' \dontrun{
#' compute_tiff_block_extent(
#'   path_file = "Data/Input/Neoclimate/CHELSA_V2.1/bio4.tif"
#' )
#' }
compute_tiff_block_extent <- function(path_file) {
  assertthat::assert_that(
    base::is.character(path_file),
    base::length(path_file) == 1L,
    !base::is.na(path_file),
    msg = "`path_file` must be a single file path."
  )

  # Row returned when the header cannot be read
  data_unread <-
    tibble::tibble(
      file_name = base::basename(path_file),
      file_size_bytes = NA_real_,
      tiff_format = NA_character_,
      block_layout = NA_character_,
      image_width = NA_real_,
      image_length = NA_real_,
      rows_per_block = NA_real_,
      n_blocks = NA_real_,
      declared_end_bytes = NA_real_,
      n_blocks_beyond_end = NA_real_,
      first_affected_row = NA_real_,
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

  con <-
    base::file(path_file, open = "rb")

  base::on.exit(base::close(con), add = TRUE)

  # A file that cannot be parsed is reported, not raised, so a check over
  #   many files fails closed on this one file
  res <-
    tryCatch(
      expr = {
        vec_byte_order <-
          base::readBin(con = con, what = "raw", n = 2)

        byte_order <-
          base::rawToChar(vec_byte_order)

        if (
          !byte_order %in% c("II", "MM")
        ) {
          cli::cli_abort("The file is not a TIFF file (no byte-order mark).")
        }

        endian <-
          dplyr::if_else(byte_order == "II", "little", "big")

        tiff_magic <-
          load_tiff_unsigned_values(
            con = con,
            n_values = 1,
            n_bytes = 2,
            endian = endian
          )

        if (
          !tiff_magic %in% c(42, 43)
        ) {
          cli::cli_abort("The file is not a TIFF file (unknown version).")
        }

        flag_bigtiff <-
          tiff_magic == 43

        # BigTIFF: offset size and a reserved zero precede the IFD offset
        if (
          isTRUE(flag_bigtiff)
        ) {
          load_tiff_unsigned_values(
            con = con,
            n_values = 2,
            n_bytes = 2,
            endian = endian
          )
        }

        n_bytes_pointer <-
          dplyr::if_else(flag_bigtiff, 8, 4)

        n_bytes_count <-
          dplyr::if_else(flag_bigtiff, 8, 2)

        offset_ifd <-
          load_tiff_unsigned_values(
            con = con,
            n_values = 1,
            n_bytes = n_bytes_pointer,
            endian = endian
          )

        base::seek(con = con, where = offset_ifd)

        n_entries <-
          load_tiff_unsigned_values(
            con = con,
            n_values = 1,
            n_bytes = n_bytes_count,
            endian = endian
          )

        # Tag, type and count of every entry, and where its value field is
        data_entries <-
          base::seq_len(n_entries) |>
          purrr::map(
            .f = ~ {
              vec_tag_type <-
                load_tiff_unsigned_values(
                  con = con,
                  n_values = 2,
                  n_bytes = 2,
                  endian = endian
                )

              n_count <-
                load_tiff_unsigned_values(
                  con = con,
                  n_values = 1,
                  n_bytes = n_bytes_pointer,
                  endian = endian
                )

              position_value <-
                base::seek(con = con)

              base::seek(con = con, where = position_value + n_bytes_pointer)

              tibble::tibble(
                tag = vec_tag_type[1],
                type = vec_tag_type[2],
                count = n_count,
                position_value = position_value
              )
            }
          ) |>
          purrr::list_rbind()

        # Bytes per value of the TIFF types BYTE, SHORT, LONG and LONG8
        vec_type_sizes <-
          c(`1` = 1, `3` = 2, `4` = 4, `16` = 8)

        vec_tags_needed <-
          c(256, 257, 278, 273, 279, 322, 323, 324, 325)

        list_tag_values <-
          vec_tags_needed |>
          rlang::set_names() |>
          purrr::map(
            .f = ~ {
              tag_current <- .x

              data_entry <-
                dplyr::filter(data_entries, .data[["tag"]] == tag_current)

              if (
                base::nrow(data_entry) == 0L
              ) {
                return(NULL)
              }

              n_bytes_value <-
                vec_type_sizes[
                  base::as.character(dplyr::pull(data_entry, "type")[1])
                ]

              if (
                base::is.na(n_bytes_value)
              ) {
                cli::cli_abort(
                  "TIFF tag {tag_current} has an unsupported type."
                )
              }

              n_count <-
                dplyr::pull(data_entry, "count")[1]

              base::seek(
                con = con,
                where = dplyr::pull(data_entry, "position_value")[1]
              )

              # Values that fit the value field are stored in it; larger
              #   arrays are stored at the offset the field holds
              if (
                n_count * n_bytes_value > n_bytes_pointer
              ) {
                offset_values <-
                  load_tiff_unsigned_values(
                    con = con,
                    n_values = 1,
                    n_bytes = n_bytes_pointer,
                    endian = endian
                  )

                base::seek(con = con, where = offset_values)
              }

              res_values <-
                load_tiff_unsigned_values(
                  con = con,
                  n_values = n_count,
                  n_bytes = base::unname(n_bytes_value),
                  endian = endian
                )

              return(res_values)
            }
          )

        image_width <-
          purrr::chuck(list_tag_values, "256")

        image_length <-
          purrr::chuck(list_tag_values, "257")

        flag_tiled <-
          !base::is.null(purrr::pluck(list_tag_values, "324"))

        if (
          isFALSE(flag_tiled) &&
            base::is.null(purrr::pluck(list_tag_values, "273"))
        ) {
          cli::cli_abort("The TIFF header has no strip or tile table.")
        }

        vec_offsets <-
          if (
            isTRUE(flag_tiled)
          ) {
            purrr::chuck(list_tag_values, "324")
          } else {
            purrr::chuck(list_tag_values, "273")
          }

        vec_counts <-
          if (
            isTRUE(flag_tiled)
          ) {
            purrr::chuck(list_tag_values, "325")
          } else {
            purrr::chuck(list_tag_values, "279")
          }

        if (
          base::length(vec_offsets) != base::length(vec_counts)
        ) {
          cli::cli_abort(
            "The TIFF block offsets and byte counts differ in length."
          )
        }

        # Without RowsPerStrip the whole image is one strip
        rows_per_block <-
          if (
            isTRUE(flag_tiled)
          ) {
            purrr::chuck(list_tag_values, "323")
          } else {
            purrr::pluck(list_tag_values, "278", .default = image_length) |>
              base::min(image_length)
          }

        n_blocks_across <-
          if (
            isTRUE(flag_tiled)
          ) {
            base::ceiling(image_width / purrr::chuck(list_tag_values, "322"))
          } else {
            1
          }

        # Sparse files may leave blocks empty (byte count 0)
        vec_block_ends <-
          vec_offsets + vec_counts

        vec_used <-
          vec_counts > 0

        vec_beyond <-
          vec_used & vec_block_ends > size_file_bytes

        index_first_beyond <-
          base::which(vec_beyond)[1]

        first_affected_row <-
          ((index_first_beyond - 1) %/% n_blocks_across) * rows_per_block

        declared_end_bytes <-
          if (
            base::any(vec_used)
          ) {
            base::max(vec_block_ends[vec_used])
          } else {
            NA_real_
          }

        tibble::tibble(
          file_name = base::basename(path_file),
          file_size_bytes = size_file_bytes,
          tiff_format = dplyr::if_else(flag_bigtiff, "bigtiff", "classic"),
          block_layout = dplyr::if_else(flag_tiled, "tile", "strip"),
          image_width = image_width,
          image_length = image_length,
          rows_per_block = rows_per_block,
          n_blocks = base::length(vec_offsets),
          declared_end_bytes = declared_end_bytes,
          n_blocks_beyond_end = base::sum(vec_beyond),
          first_affected_row = base::as.numeric(first_affected_row),
          read_error = NA_character_
        )
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

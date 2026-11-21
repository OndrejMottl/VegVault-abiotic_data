#' @title Load unsigned integers from a binary connection
#' @description
#' Read `n_values` unsigned integers of 1, 2, 4 or 8 bytes from the current
#' position of a binary connection, as stored in a TIFF header.
#' @param con
#' An open binary connection, for example from `file(path, "rb")`.
#' @param n_values
#' Whole number, at least 0. Number of values to read.
#' @param n_bytes
#' Whole number: 1, 2, 4 or 8. Size of one value in bytes.
#' @param endian
#' Character scalar, `"little"` or `"big"`: the byte order of the file.
#' @return
#' Numeric (double) vector of length `n_values`.
#' @details
#' R has no unsigned 32- or 64-bit integer type, so values are returned as
#' doubles. 8-byte values are combined from two 4-byte halves and are exact
#' up to 2^53 bytes, far beyond any raster file. The connection advances
#' by `n_values * n_bytes` bytes. Aborts when the connection ends before
#' `n_values` values were read.
#' @examples
#' \dontrun{
#' con <- file("raster.tif", "rb")
#' load_tiff_unsigned_values(
#'   con = con,
#'   n_values = 1,
#'   n_bytes = 4,
#'   endian = "little"
#' )
#' }
load_tiff_unsigned_values <- function(
  con,
  n_values,
  n_bytes,
  endian
) {
  assertthat::assert_that(
    base::inherits(con, "connection"),
    msg = "`con` must be an open binary connection."
  )

  assertthat::assert_that(
    base::is.numeric(n_values),
    base::length(n_values) == 1L,
    !base::is.na(n_values),
    n_values >= 0,
    n_values == base::round(n_values),
    msg = "`n_values` must be a whole number of at least 0."
  )

  assertthat::assert_that(
    base::is.numeric(n_bytes),
    base::length(n_bytes) == 1L,
    !base::is.na(n_bytes),
    n_bytes %in% c(1, 2, 4, 8),
    msg = "`n_bytes` must be 1, 2, 4 or 8."
  )

  assertthat::assert_that(
    base::is.character(endian),
    base::length(endian) == 1L,
    !base::is.na(endian),
    endian %in% c("little", "big"),
    msg = "`endian` must be \"little\" or \"big\"."
  )

  if (
    n_values == 0
  ) {
    res <-
      base::numeric(0)

    return(res)
  }

  # Unsigned values of up to 2 bytes fit R integers; larger values are read
  #   as signed 4-byte integers and shifted into the unsigned range
  n_bytes_read <-
    base::min(n_bytes, 4)

  n_reads <-
    n_values * n_bytes / n_bytes_read

  vec_raw_values <-
    base::readBin(
      con = con,
      what = "integer",
      n = n_reads,
      size = n_bytes_read,
      signed = n_bytes_read == 4,
      endian = endian
    )

  if (
    base::length(vec_raw_values) < n_reads
  ) {
    cli::cli_abort(
      c(
        "Reached the end of the file while reading a TIFF header.",
        "i" = "Expected {n_values} value{?s} of {n_bytes} byte{?s}."
      )
    )
  }

  vec_unsigned <-
    base::as.numeric(vec_raw_values) +
    dplyr::if_else(vec_raw_values < 0, 2^32, 0)

  if (
    n_bytes < 8
  ) {
    res <- vec_unsigned

    return(res)
  }

  # An 8-byte value is two 4-byte halves; their order follows the byte order
  vec_first <-
    vec_unsigned[base::seq(1, n_reads, by = 2)]

  vec_second <-
    vec_unsigned[base::seq(2, n_reads, by = 2)]

  res <-
    if (
      endian == "little"
    ) {
      vec_first + vec_second * 2^32
    } else {
      vec_first * 2^32 + vec_second
    }

  return(res)
}

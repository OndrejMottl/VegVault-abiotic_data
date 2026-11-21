#' @title Build the storage-type limit values of a raster quality check
#' @description
#' Build the candidate values at which a raster may be capped by its
#' storage type: the minimum and maximum of each integer storage type,
#' multiplied by each scale factor and shifted by each offset.
#' @param vec_scales
#' Numeric vector of positive scale factors, for example `c(1, 0.1, 0.01)`.
#' @param vec_offsets
#' Numeric vector of offsets added after scaling, for example
#' `c(0, -273.15)`. Default `0`.
#' @param vec_integer_types
#' Character vector of integer storage types, named as in `{terra}`:
#' `"INT1S"`, `"INT1U"`, `"INT2S"`, `"INT2U"`, `"INT4S"` and `"INT4U"`
#' (signed and unsigned 8-, 16- and 32-bit integers). Default: all six.
#' @return
#' Numeric vector of the unique candidate values, sorted in increasing
#' order. A value of exactly 0 is left out.
#' @details
#' A candidate is `limit * scale + offset` for the minimum and maximum of
#' every type. Zero is left out because zeros are checked by the zero-share
#' rules of the quality check; with an offset of 0 the minimum of every
#' unsigned type would otherwise flag every legitimate zero. A raster stored
#' as floats can still carry an integer cap from its source, so the
#' candidates are used for every raster, whatever its own storage type.
#' @examples
#' \dontrun{
#' build_raster_type_limit_values(
#'   vec_scales = c(1, 0.1, 0.01),
#'   vec_offsets = c(0, -273.15)
#' )
#' }
build_raster_type_limit_values <- function(
  vec_scales,
  vec_offsets = 0,
  vec_integer_types = c(
    "INT1S", "INT1U", "INT2S", "INT2U", "INT4S", "INT4U"
  )
) {
  # Minimum and maximum of each integer storage type
  list_type_limits <-
    list(
      INT1S = c(-128, 127),
      INT1U = c(0, 255),
      INT2S = c(-32768, 32767),
      INT2U = c(0, 65535),
      INT4S = c(-2147483648, 2147483647),
      INT4U = c(0, 4294967295)
    )

  assertthat::assert_that(
    base::is.numeric(vec_scales),
    base::length(vec_scales) > 0L,
    base::all(base::is.finite(vec_scales)),
    base::all(vec_scales > 0),
    msg = "`vec_scales` must be a non-empty vector of positive numbers."
  )

  assertthat::assert_that(
    base::is.numeric(vec_offsets),
    base::length(vec_offsets) > 0L,
    base::all(base::is.finite(vec_offsets)),
    msg = "`vec_offsets` must be a non-empty vector of finite numbers."
  )

  assertthat::assert_that(
    base::is.character(vec_integer_types),
    base::length(vec_integer_types) > 0L,
    base::all(vec_integer_types %in% base::names(list_type_limits)),
    msg = stringr::str_c(
      "`vec_integer_types` must hold only ",
      stringr::str_c(base::names(list_type_limits), collapse = ", "),
      "."
    )
  )

  vec_limits <-
    list_type_limits[base::unique(vec_integer_types)] |>
    base::unlist(use.names = FALSE)

  data_candidates <-
    tidyr::expand_grid(
      limit = vec_limits,
      scale = vec_scales,
      offset = vec_offsets
    ) |>
    dplyr::mutate(
      value = .data[["limit"]] * .data[["scale"]] + .data[["offset"]]
    )

  # Zeros are checked by the zero-share rules, not as a type limit
  res <-
    data_candidates |>
    dplyr::pull("value") |>
    base::unique() |>
    purrr::discard(.p = ~ .x == 0) |>
    base::sort()

  return(res)
}

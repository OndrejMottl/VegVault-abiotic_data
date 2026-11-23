#' @title Split a table into consecutive batches of equal size
#' @description
#' Split the rows of a table, in their order, into a fixed number of
#' consecutive batches of equal size, the partitions of a batched export
#' artefact.
#' @param data_export
#' A data frame.
#' @param n_batches
#' Whole number of at least 1. The number of batches.
#' @return
#' A list of `n_batches` data frames, the consecutive row ranges of
#' `data_export`. Each batch holds `ceiling(n / n_batches)` rows of the
#' `n` rows, except the last, which holds the rest.
#' @details
#' The rule is the one of `save_in_batches()` of VegVault 1.0.0, so a
#' batch may start or end inside a time slice. Every row is in exactly one
#' batch, and the batches bound in order give `data_export` back. Aborts
#' when a batch would be empty, which happens when `data_export` has too
#' few rows for `n_batches`.
#' @seealso [save_cell_table_export()]
#' @examples
#' \dontrun{
#' prepare_export_batches(data_export = data_cells, n_batches = 4)
#' }
prepare_export_batches <- function(
  data_export,
  n_batches
) {
  assertthat::assert_that(
    base::is.data.frame(data_export),
    msg = "`data_export` must be a data frame."
  )

  assertthat::assert_that(
    rlang::is_scalar_integerish(n_batches, finite = TRUE) &&
      n_batches >= 1,
    msg = "`n_batches` must be one whole number of at least 1."
  )

  n_rows <-
    base::nrow(data_export)

  n_batches_integer <-
    base::as.integer(n_batches)

  n_rows_batch <-
    base::as.integer(base::ceiling(n_rows / n_batches_integer))

  vec_first_rows <-
    (base::seq_len(n_batches_integer) - 1L) * n_rows_batch + 1L

  if (
    n_rows == 0L || base::any(vec_first_rows > n_rows)
  ) {
    cli::cli_abort(
      c(
        "Splitting {n_rows} row{?s} into {n_batches_integer} batch{?es}
        leaves a batch empty.",
        "i" = "Every batch must hold at least one row."
      )
    )
  }

  vec_last_rows <-
    base::pmin(vec_first_rows + n_rows_batch - 1L, n_rows)

  res <-
    purrr::map2(
      .x = vec_first_rows,
      .y = vec_last_rows,
      .f = ~ dplyr::slice(data_export, .x:.y)
    )

  return(res)
}

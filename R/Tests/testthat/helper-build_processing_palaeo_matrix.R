#' @title Build the values of one palaeoclimate test raster
#' @description
#' Return the 8 by 16 cell values of one variable and age of the palaeo
#' product of `build_processing_pipeline_fixture()`.
#' @param sel_variable
#' Character scalar, `"bio12"` or `"bio15"`.
#' @param sel_age
#' Numeric scalar, 0, 500 or 1000.
#' @return
#' A numeric matrix with 8 rows and 16 columns: bio15 is 50 everywhere;
#' bio12 is 100, except 600 in rows 3 and 4, columns 5 to 8 and 13 to 14
#' at age 500, and in rows 5 and 6, columns 5 and 6 at age 1000.
build_processing_palaeo_matrix <- function(
  sel_variable,
  sel_age
) {
  mat_base <-
    base::matrix(
      dplyr::if_else(sel_variable == "bio15", 50, 100),
      nrow = 8,
      ncol = 16
    )

  # Row and column of every inflated cell, by age
  mat_index_inflated <-
    if (
      sel_variable == "bio12" && sel_age == 500
    ) {
      base::cbind(
        base::rep(c(3, 4), times = 6),
        base::rep(c(5, 6, 7, 8, 13, 14), each = 2)
      )
    } else if (
      sel_variable == "bio12" && sel_age == 1000
    ) {
      base::cbind(c(5, 6, 5, 6), c(5, 5, 6, 6))
    } else {
      base::matrix(0, nrow = 0, ncol = 2)
    }

  res <-
    base::replace(
      x = mat_base,
      list = mat_index_inflated,
      values = 600
    )

  return(res)
}

#' @title Build small cell tables for tests of the export stage
#' @description
#' Write the cell tables of three small products, as the extraction stage
#' saves them, and return their extraction tables.
#' @param dir_root
#' Character scalar. The folder to write into; created when missing.
#' @return
#' A list with `neo`, `palaeo` and `hwsd`, each a tibble like the public
#' table of the extraction stage (`product`, `variable`, `age`, `time_id`,
#' `file_name`, `path_file`), and `vec_variables`, the seven variables of
#' the climate contracts.
#' @details
#' Every table has the cells of a grid of 2 rows and 4 columns of 90
#' degrees: centres at 135 W, 45 W, 45 E and 135 E, first at 45 N, then at
#' 45 S. `neo` has one table per variable (`long`, `lat`, `value`,
#' `var_name`) with the values 1 to 8. `palaeo` has two tables per
#' variable (`long`, `lat`, `time_id`, `value`): `time_id` 20 (age 0) with
#' the values 1 to 8, and `time_id` 15 (age 500) with 11 to 18 and no row
#' for cell 3; its extraction table lists age 500 before age 0. `hwsd` has
#' one table (`long`, `lat`, `value`, `var_name`) with the codes 1, 19, 1,
#' 19, 19 and 1 in the cells 1, 2, 4, 6, 7 and 8. All values lie inside
#' the ranges of the VegVault contracts.
build_export_cell_tables_fixture <- function(dir_root) {
  # The folder must exist before its path is normalised
  base::dir.create(path = dir_root, recursive = TRUE, showWarnings = FALSE)

  dir_cell_tables <-
    base::file.path(
      base::normalizePath(dir_root, winslash = "/"),
      "Cell_tables"
    )

  base::dir.create(path = dir_cell_tables, recursive = TRUE)

  data_cells <-
    tibble::tibble(
      long = base::rep(c(-135, -45, 45, 135), times = 2),
      lat = base::rep(c(45, -45), each = 4)
    )

  vec_variables <-
    c("bio1", "bio4", "bio6", "bio12", "bio15", "bio18", "bio19")

  vec_tokens <-
    c("01", "04", "06", "12", "15", "18", "19")

  # Present-day tables, one per variable
  data_neo <-
    tibble::tibble(
      product = "prod_neo",
      variable = vec_variables,
      age = 0,
      time_id = NA_integer_,
      file_name = stringr::str_c("neo_", vec_variables, ".tif"),
      path_file = base::file.path(
        dir_cell_tables,
        stringr::str_c("neo_", vec_variables, ".qs2")
      )
    )

  purrr::walk2(
    .x = dplyr::pull(data_neo, "path_file"),
    .y = vec_tokens,
    .f = ~ qs2::qs_save(
      object = dplyr::mutate(
        data_cells,
        value = base::as.numeric(1:8),
        var_name = stringr::str_c("bio_", .y)
      ),
      file = .x
    )
  )

  # Palaeoclimate tables, two ages per variable, age 500 listed first
  data_palaeo <-
    tidyr::expand_grid(
      variable = vec_variables,
      time_id = c(15L, 20L)
    ) |>
    dplyr::mutate(
      product = "prod_palaeo",
      age = -.data[["time_id"]] * 100 + 2000,
      file_name = stringr::str_c(
        "palaeo_", .data[["variable"]], "_", .data[["time_id"]], ".tif"
      ),
      path_file = base::file.path(
        dir_cell_tables,
        stringr::str_c(
          "palaeo_", .data[["variable"]], "_", .data[["time_id"]], ".qs2"
        )
      )
    ) |>
    dplyr::select(
      "product", "variable", "age", "time_id", "file_name", "path_file"
    )

  purrr::walk2(
    .x = dplyr::pull(data_palaeo, "path_file"),
    .y = dplyr::pull(data_palaeo, "time_id"),
    .f = ~ qs2::qs_save(
      object = if (
        .y == 20L
      ) {
        dplyr::mutate(
          data_cells,
          time_id = .y,
          value = base::as.numeric(1:8)
        )
      } else {
        dplyr::mutate(
          data_cells,
          time_id = .y,
          value = base::as.numeric(11:18)
        ) |>
          dplyr::slice(-3)
      },
      file = .x
    )
  )

  # Soil groups; cells 3 and 5 have no soil group
  data_hwsd <-
    tibble::tibble(
      product = "prod_hwsd",
      variable = "soil_group",
      age = 0,
      time_id = NA_integer_,
      file_name = "hwsd_soil_groups.tif",
      path_file = base::file.path(dir_cell_tables, "hwsd_soil_groups.qs2")
    )

  qs2::qs_save(
    object = data_cells |>
      dplyr::mutate(
        value = c(1, 19, NA, 1, NA, 19, 19, 1),
        var_name = "HWSD2"
      ) |>
      dplyr::filter(!base::is.na(.data[["value"]])),
    file = dplyr::pull(data_hwsd, "path_file")
  )

  res <-
    base::list(
      neo = data_neo,
      palaeo = data_palaeo,
      hwsd = data_hwsd,
      vec_variables = vec_variables
    )

  return(res)
}

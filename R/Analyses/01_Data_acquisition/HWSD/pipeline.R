#----------------------------------------------------------#
#
#
#                     Abiotic data
#
#             Data acquisition: HWSD v2.2 pipeline
#
#
#                       O. Mottl
#                         2026
#
#----------------------------------------------------------#
# Defines the target graph that obtains and checks the HWSD v2.2 archives
#   (source `hwsd` in `config.yml`), extracts the raster and the Access
#   database next to them, and provides the soil-unit and soil-group
#   tables as tracked CSV files, so later stages need no Access driver.
# With every flag false (the default) nothing is downloaded or read from
#   the database, and a missing file or table stops the run with a message
#   that names the flag to set. The tables are written once, with
#   `flag_read_access_database`, which needs the Microsoft Access ODBC
#   driver (Windows only).
# Run with:
#   R/Analyses/01_Data_acquisition/00_run.R
# Sourcing this script only declares targets; it does not execute them.


#----------------------------------------------------------#
# 0. Configure pipeline -----
#----------------------------------------------------------#

library(here)

source(
  here::here("R/___setup_project___.R")
)

config_data_acquisition <-
  load_config_value(value = "data_acquisition")

config_hwsd_class_tables <-
  purrr::chuck(config_data_acquisition, "hwsd_class_tables")

path_hwsd_class_tables <-
  base::file.path(
    path_data_storage,
    purrr::chuck(config_hwsd_class_tables, "dir")
  )

targets::tar_option_set(seed = config_seed)


#----------------------------------------------------------#
# 1. Define targets -----
#----------------------------------------------------------#

list(
  # Expected archives, flag-gated downloads and checksums, the archive
  #   checks, the validated archives and their extracted members
  build_source_acquisition_targets(
    sel_source = "hwsd",
    list_data_sources = load_config_value(value = "data_sources"),
    list_data_acquisition = config_data_acquisition,
    path_data_storage = path_data_storage
  ),
  # The class tables read from the Access database, only with the flag
  targets::tar_target_raw(
    name = "hwsd_class_tables_written",
    command = rlang::expr(
      save_hwsd_class_tables(
        vec_extracted_files = hwsd_extracted_files,
        dir_tables = !!path_hwsd_class_tables,
        flag_read_access_database = !!purrr::chuck(
          config_hwsd_class_tables,
          "flag_read_access_database"
        )
      )
    )
  ),
  # Public: the tracked class tables, checked after any writing
  targets::tar_target_raw(
    name = "hwsd_class_tables",
    command = rlang::expr(
      validate_hwsd_class_tables(dir_tables = !!path_hwsd_class_tables)
    ),
    deps = c("hwsd_class_tables_written", "validate_hwsd_class_tables"),
    format = "file"
  )
)

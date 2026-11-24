#----------------------------------------------------------#
#
#
#                     Abiotic data
#
#                 Export: artefact pipeline
#
#
#                       O. Mottl
#                         2026
#
#----------------------------------------------------------#
# Defines the target graph that saves the cell tables of the extraction
#   stage as the artefacts VegVault reads (`data_export` in `config.yml`):
#   one file per CHELSA V2.1 variable, four batches per CHELSA-TraCE21k
#   variable and one soil file, named and placed as the project's copy of
#   the VegVault contracts (`Data/Contracts/`) prescribes; the check of
#   every contract folder and file; and the checksum manifest, the summary
#   per variable and the legend of the soil-group codes.
# Run it with explicit approval.
# Run with:
#   R/Analyses/05_Export/00_run.R
# Sourcing this script only declares targets; it does not execute them.


#----------------------------------------------------------#
# 0. Configure pipeline -----
#----------------------------------------------------------#

library(here)

source(
  here::here("R/___setup_project___.R")
)

targets::tar_option_set(seed = config_seed)

# The HWSD lookup of the soil-group names, saved by the download stage
path_wrb_lookup <-
  base::file.path(
    path_data_storage,
    load_config_value(
      value = c("data_acquisition", "hwsd_class_tables", "dir")
    ),
    "d_wrb2.csv"
  )


#----------------------------------------------------------#
# 1. Define targets -----
#----------------------------------------------------------#

# Imports of the extraction stage and the quality check, the export files
#   per product with their checks, and the tables
build_export_targets(
  list_data_export = load_config_value(value = "data_export"),
  path_data_storage = path_data_storage,
  path_project = here::here(),
  list_soil_group_codes = load_config_value(
    value = c("data_processing", "hwsd", "soil_group_codes")
  ),
  path_wrb_lookup = path_wrb_lookup
)

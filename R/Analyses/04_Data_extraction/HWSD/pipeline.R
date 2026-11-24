#----------------------------------------------------------#
#
#
#                     Abiotic data
#
#            Data extraction: HWSD v2.2 pipeline
#
#
#                       O. Mottl
#                         2026
#
#----------------------------------------------------------#
# Defines the target graph that turns the processed HWSD v2.2 soil-group
#   raster into a cell table (`hwsd` of `data_extraction` in
#   `config.yml`): the soil-group raster of the processing stage and its
#   cell table in `Data/Processed/HWSD_v2.2/Cell_tables/`, with the columns
#   of the VegVault contract `hwsd_soil`.
# Run it with explicit approval.
# Run with:
#   R/Analyses/04_Data_extraction/00_run.R
# Sourcing this script only declares targets; it does not execute them.


#----------------------------------------------------------#
# 0. Configure pipeline -----
#----------------------------------------------------------#

library(here)

source(
  here::here("R/___setup_project___.R")
)

targets::tar_option_set(seed = config_seed)


#----------------------------------------------------------#
# 1. Define targets -----
#----------------------------------------------------------#

# Import of the soil-group raster and its cell table
build_hwsd_extraction_targets(
  list_data_extraction = load_config_value(value = "data_extraction"),
  path_data_storage = path_data_storage
)

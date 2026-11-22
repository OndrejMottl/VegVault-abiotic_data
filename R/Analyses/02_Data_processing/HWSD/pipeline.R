#----------------------------------------------------------#
#
#
#                     Abiotic data
#
#            Data processing: HWSD v2.2 pipeline
#
#
#                       O. Mottl
#                         2026
#
#----------------------------------------------------------#
# Defines the target graph that turns the HWSD v2.2 soil-unit raster into
#   the soil-group raster on the processed CHELSA grid (`hwsd` of
#   `data_processing` in `config.yml`): the class of the dominant component
#   of every soil unit, the classes at full resolution, resampled to the
#   raw CHELSA V2.1 grid and aggregated by the mode, the soil-group raster
#   in `Data/Processed/HWSD_v2.2/` and its summary in `Outputs/Tables/`.
# Classifying, resampling and aggregating the global soil raster is raster
#   processing: run it only with explicit approval.
# Run with:
#   R/Analyses/02_Data_processing/00_run.R
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

# Imports, soil-unit classes, the class rasters, the summary and the
#   soil-group raster
build_hwsd_processing_targets(
  list_data_processing = load_config_value(value = "data_processing"),
  path_data_storage = path_data_storage,
  dir_tables = here::here("Outputs", "Tables")
)

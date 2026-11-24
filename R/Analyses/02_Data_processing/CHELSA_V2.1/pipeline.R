#----------------------------------------------------------#
#
#
#                     Abiotic data
#
#          Data processing: CHELSA V2.1 pipeline
#
#
#                       O. Mottl
#                         2026
#
#----------------------------------------------------------#
# Defines the target graph that aggregates the raw CHELSA V2.1
#   climatologies to the processed grid (product `chelsa_v21` of
#   `data_processing` in `config.yml`): the validated raw files of the
#   download stage, one aggregation per file, the polar fill-value cells,
#   the final rasters in `Data/Processed/CHELSA_V2.1/` and the counts of
#   masked cells in `Outputs/Tables/`.
# Aggregating the 7 global rasters is raster processing: run it only with
#   explicit approval.
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

config_data_processing <-
  load_config_value(value = "data_processing")

targets::tar_option_set(
  seed = config_seed,
  controller = build_crew_controller(
    n_workers = purrr::chuck(
      config_data_processing,
      "climate_products", "chelsa_v21", "n_workers"
    )
  )
)


#----------------------------------------------------------#
# 1. Define targets -----
#----------------------------------------------------------#

# Import of the raw files, aggregation, masks, final rasters and counts
build_climate_processing_targets(
  sel_product = "chelsa_v21",
  list_data_processing = config_data_processing,
  path_data_storage = path_data_storage,
  dir_tables = here::here("Outputs", "Tables")
)

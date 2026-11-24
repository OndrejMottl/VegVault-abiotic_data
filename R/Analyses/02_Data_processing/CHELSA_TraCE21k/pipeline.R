#----------------------------------------------------------#
#
#
#                     Abiotic data
#
#        Data processing: CHELSA-TraCE21k pipeline
#
#
#                       O. Mottl
#                         2026
#
#----------------------------------------------------------#
# Defines the target graph that aggregates the raw CHELSA-TraCE21k files
#   to the processed grid (product `chelsa_trace21k` of `data_processing`
#   in `config.yml`): the validated raw files of the download stage, one
#   aggregation per file, the cells of the South American precipitation
#   artefact, the final rasters in `Data/Processed/CHELSA_TraCE21k_V1.0/`
#   and the counts of masked cells in `Outputs/Tables/`.
# Aggregating the 259 global rasters is raster processing (about 10 s and
#   3.6 GB of memory per file, on local worker processes): run it only with
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

# The files are aggregated in parallel; memory, not cores, limits the
#   number of workers
targets::tar_option_set(
  seed = config_seed,
  controller = build_crew_controller(
    n_workers = purrr::chuck(
      config_data_processing,
      "climate_products", "chelsa_trace21k", "n_workers"
    )
  )
)


#----------------------------------------------------------#
# 1. Define targets -----
#----------------------------------------------------------#

# Import of the raw files, aggregation, masks, final rasters and counts
build_climate_processing_targets(
  sel_product = "chelsa_trace21k",
  list_data_processing = config_data_processing,
  path_data_storage = path_data_storage,
  dir_tables = here::here("Outputs", "Tables")
)

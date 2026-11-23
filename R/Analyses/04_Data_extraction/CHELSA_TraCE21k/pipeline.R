#----------------------------------------------------------#
#
#
#                     Abiotic data
#
#        Data extraction: CHELSA-TraCE21k pipeline
#
#
#                       O. Mottl
#                         2026
#
#----------------------------------------------------------#
# Defines the target graph that turns the processed CHELSA-TraCE21k
#   rasters into cell tables (product `chelsa_trace21k` of
#   `data_extraction` in `config.yml`): the processed rasters of the
#   processing stage and one cell table per raster in
#   `Data/Processed/CHELSA_TraCE21k_V1.0/Cell_tables/`, with the columns of
#   the VegVault contract `chelsa_trace21k_palaeoclimate`.
# Extracting the 259 global rasters (about 2 s per file, on local worker
#   processes) reads processed rasters only: run it with explicit approval.
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

config_data_extraction <-
  load_config_value(value = "data_extraction")

targets::tar_option_set(
  seed = config_seed,
  controller = build_crew_controller(
    n_workers = purrr::chuck(
      config_data_extraction,
      "climate_products", "chelsa_trace21k", "n_workers"
    )
  )
)


#----------------------------------------------------------#
# 1. Define targets -----
#----------------------------------------------------------#

# Import of the processed rasters and one cell table per raster
build_climate_extraction_targets(
  sel_product = "chelsa_trace21k",
  list_data_extraction = config_data_extraction,
  path_data_storage = path_data_storage
)

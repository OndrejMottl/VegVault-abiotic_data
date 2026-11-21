#----------------------------------------------------------#
#
#
#                     Abiotic data
#
#               Run stage 01: data acquisition
#
#
#                       O. Mottl
#                         2026
#
#----------------------------------------------------------#
# Builds the pipelines that obtain and check the raw data sources, each in
#   its own store under `<data root>/Targets_data/`, so the 134 GB of
#   CHELSA-TraCE21k never share a store with the small sources.
# With every flag in `data_sources` and `data_acquisition` false (the
#   default), no download starts and nothing is read from the HWSD
#   database: the run reads only file sizes, TIFF headers and zip
#   directories, and stops with a message naming the flag when a file is
#   missing, incomplete or without a download record. Setting a flag is
#   for one approved run; set it back afterwards.


#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)

source(
  here::here("R/___setup_project___.R")
)

flag_visualise_pipeline <- FALSE


#----------------------------------------------------------#
# 1. Run pipelines -----
#----------------------------------------------------------#

run_target_pipeline(
  script = here::here("R/Analyses/01_Data_acquisition/CHELSA_V2.1/pipeline.R"),
  store = resolve_pipeline_store_path(
    path_data_storage = path_data_storage,
    store_relative_path = "data_acquisition_chelsa_v21"
  ),
  visualise = flag_visualise_pipeline
)

run_target_pipeline(
  script = here::here("R/Analyses/01_Data_acquisition/HWSD/pipeline.R"),
  store = resolve_pipeline_store_path(
    path_data_storage = path_data_storage,
    store_relative_path = "data_acquisition_hwsd"
  ),
  visualise = flag_visualise_pipeline
)

run_target_pipeline(
  script = here::here(
    "R/Analyses/01_Data_acquisition/CHELSA_TraCE21k/pipeline.R"
  ),
  store = resolve_pipeline_store_path(
    path_data_storage = path_data_storage,
    store_relative_path = "data_acquisition_chelsa_trace21k"
  ),
  visualise = flag_visualise_pipeline
)

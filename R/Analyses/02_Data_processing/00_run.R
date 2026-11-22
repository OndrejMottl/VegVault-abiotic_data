#----------------------------------------------------------#
#
#
#                     Abiotic data
#
#               Run stage 02: data processing
#
#
#                       O. Mottl
#                         2026
#
#----------------------------------------------------------#
# Builds the pipelines that turn the raw rasters into the processed
#   rasters, each in its own store under `<data root>/Targets_data/`. Each
#   pipeline imports the validated raw files of the download stage
#   (`R/Analyses/01_Data_acquisition/00_run.R`) and stops when that stage
#   has not run or failed. Up-to-date targets are skipped, and a replaced
#   raw file reruns only its own branch.
# The first run processes every global raster (about 15 minutes with the
#   configured workers) and needs explicit approval. It writes only into the
#   git-ignored folders `Data/Processed/CHELSA_V2.1/`,
#   `Data/Processed/CHELSA_TraCE21k_V1.0/` and `Data/Processed/HWSD_v2.2/`,
#   and the tables in `Outputs/Tables/`; the tracked v1 rasters stay.


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
  script = here::here("R/Analyses/02_Data_processing/CHELSA_V2.1/pipeline.R"),
  store = resolve_pipeline_store_path(
    path_data_storage = path_data_storage,
    store_relative_path = "data_processing_chelsa_v21"
  ),
  visualise = flag_visualise_pipeline
)

run_target_pipeline(
  script = here::here("R/Analyses/02_Data_processing/HWSD/pipeline.R"),
  store = resolve_pipeline_store_path(
    path_data_storage = path_data_storage,
    store_relative_path = "data_processing_hwsd"
  ),
  visualise = flag_visualise_pipeline
)

run_target_pipeline(
  script = here::here(
    "R/Analyses/02_Data_processing/CHELSA_TraCE21k/pipeline.R"
  ),
  store = resolve_pipeline_store_path(
    path_data_storage = path_data_storage,
    store_relative_path = "data_processing_chelsa_trace21k"
  ),
  visualise = flag_visualise_pipeline
)

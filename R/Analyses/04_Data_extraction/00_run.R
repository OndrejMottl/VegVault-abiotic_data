#----------------------------------------------------------#
#
#
#                     Abiotic data
#
#              Run stage 04: data extraction
#
#
#                       O. Mottl
#                         2026
#
#----------------------------------------------------------#
# Builds the pipelines that turn every processed raster into a cell table,
#   each in its own store under `<data root>/Targets_data/`. Each pipeline
#   imports the processed rasters of the processing stage
#   (`R/Analyses/02_Data_processing/00_run.R`) and stops when that stage
#   has not run or failed. Up-to-date targets are skipped, and a changed
#   processed raster reruns only its own cell table.
# The first run reads every processed raster (about 2 to 3 minutes with
#   the configured workers) and needs explicit approval. It writes only
#   into the git-ignored folders `Cell_tables/` of
#   `Data/Processed/CHELSA_V2.1/`, `Data/Processed/CHELSA_TraCE21k_V1.0/`
#   and `Data/Processed/HWSD_v2.2/`.


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
  script = here::here("R/Analyses/04_Data_extraction/CHELSA_V2.1/pipeline.R"),
  store = resolve_pipeline_store_path(
    path_data_storage = path_data_storage,
    store_relative_path = "data_extraction_chelsa_v21"
  ),
  visualise = flag_visualise_pipeline
)

run_target_pipeline(
  script = here::here("R/Analyses/04_Data_extraction/HWSD/pipeline.R"),
  store = resolve_pipeline_store_path(
    path_data_storage = path_data_storage,
    store_relative_path = "data_extraction_hwsd"
  ),
  visualise = flag_visualise_pipeline
)

run_target_pipeline(
  script = here::here(
    "R/Analyses/04_Data_extraction/CHELSA_TraCE21k/pipeline.R"
  ),
  store = resolve_pipeline_store_path(
    path_data_storage = path_data_storage,
    store_relative_path = "data_extraction_chelsa_trace21k"
  ),
  visualise = flag_visualise_pipeline
)

#----------------------------------------------------------#
#
#
#                     Abiotic data
#
#                Run stage 03: quality checks
#
#
#                       O. Mottl
#                         2026
#
#----------------------------------------------------------#
# Builds the pipelines of the raster quality check, each in its own store
#   under `<data root>/Targets_data/`. Up-to-date targets are skipped, and
#   a replaced raster reruns only its own checks.
# The processed rasters run first: they are small, and their tables are
#   the comparison with VegVault 1.0.0. The raw rasters run second; their
#   first run reads about 154 GB and needs explicit approval.
# A required check set with a failing finding stops the run after all
#   tables of its pipeline are written to `Outputs/Tables/`.


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
  script = here::here(
    "R/Analyses/03_Quality_checks/Processed_rasters/pipeline.R"
  ),
  store = resolve_pipeline_store_path(
    path_data_storage = path_data_storage,
    store_relative_path = "quality_checks_processed_rasters"
  ),
  visualise = flag_visualise_pipeline
)

run_target_pipeline(
  script = here::here("R/Analyses/03_Quality_checks/Raw_rasters/pipeline.R"),
  store = resolve_pipeline_store_path(
    path_data_storage = path_data_storage,
    store_relative_path = "quality_checks_raw_rasters"
  ),
  visualise = flag_visualise_pipeline
)

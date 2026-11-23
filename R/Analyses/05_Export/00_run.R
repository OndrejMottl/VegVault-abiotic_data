#----------------------------------------------------------#
#
#
#                     Abiotic data
#
#                  Run stage 05: export
#
#
#                       O. Mottl
#                         2026
#
#----------------------------------------------------------#
# Builds the export pipeline in its own store under
#   `<data root>/Targets_data/` (`store` of `data_export` in `config.yml`).
#   It imports the cell tables of the extraction stage
#   (`R/Analyses/04_Data_extraction/00_run.R`) and the passed quality check
#   of the processed rasters (`R/Analyses/03_Quality_checks/00_run.R`), and
#   stops when either has not run or failed. Up-to-date targets are
#   skipped, and a changed cell table reruns only the export of its
#   variable.
# The `default` profile writes the release artefacts into
#   `Outputs/Data/Neoclimate/`, `Outputs/Data/Palaoclimate/` and
#   `Outputs/Data/HWSD/` and the tables into `Outputs/Tables/`; it stops
#   while those folders hold any other file, such as a legacy v1 output.
#   The `dev` profile writes the same layout into the git-ignored
#   `Outputs/Data_temp/`:
#   R_CONFIG_ACTIVE=dev Rscript R/Analyses/05_Export/00_run.R
# An export never overwrites an earlier version of an artefact unless
#   `flag_replace_previous` is true. A full run reads every cell table and
#   needs explicit approval.


#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)

source(
  here::here("R/___setup_project___.R")
)

flag_visualise_pipeline <- FALSE


#----------------------------------------------------------#
# 1. Run pipeline -----
#----------------------------------------------------------#

run_target_pipeline(
  script = here::here("R/Analyses/05_Export/pipeline.R"),
  store = resolve_pipeline_store_path(
    path_data_storage = path_data_storage,
    store_relative_path = load_config_value(value = c("data_export", "store"))
  ),
  visualise = flag_visualise_pipeline
)

#----------------------------------------------------------#
#
#
#                     Abiotic data
#
#             Quality checks: raw rasters pipeline
#
#
#                       O. Mottl
#                         2026
#
#----------------------------------------------------------#
# Defines the target graph of the raster quality check of the raw
#   downloads: the current CHELSA V2.1 and CHELSA-TraCE21k files, which
#   must pass, and the legacy CHELSA V2.1 files of 2023, kept as a
#   comparison. The check sets and their rules are in `config.yml`
#   (`quality_checks`).
# Reading every raw file is global raster processing (about 154 GB, one
#   hour or more with 8 workers): run it only with explicit approval.
# Run with:
#   R/Analyses/03_Quality_checks/00_run.R
# Sourcing this script only declares targets; it does not execute them.


#----------------------------------------------------------#
# 0. Configure pipeline -----
#----------------------------------------------------------#

library(here)

source(
  here::here("R/___setup_project___.R")
)

config_quality_checks <-
  load_config_value(value = "quality_checks")

config_pipeline <-
  purrr::chuck(config_quality_checks, "pipelines", "raw_rasters")

# Each raw file takes about 100 s, so the files run in parallel on local
#   worker processes
targets::tar_option_set(
  seed = config_seed,
  controller = build_crew_controller(
    n_workers = purrr::chuck(config_pipeline, "n_workers")
  )
)


#----------------------------------------------------------#
# 1. Define targets -----
#----------------------------------------------------------#

# Per check set: expected files, one statistics branch per file and pair,
#   findings, summary and the tables in `Outputs/Tables/`; then the
#   validation of the sets that must pass
build_raster_quality_targets(
  vec_check_sets = base::unlist(
    purrr::chuck(config_pipeline, "check_sets")
  ),
  list_quality_checks = config_quality_checks,
  path_data_storage = path_data_storage,
  dir_tables = here::here("Outputs", "Tables")
)

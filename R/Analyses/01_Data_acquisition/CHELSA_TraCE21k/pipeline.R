#----------------------------------------------------------#
#
#
#                     Abiotic data
#
#        Data acquisition: CHELSA-TraCE21k pipeline
#
#
#                       O. Mottl
#                         2026
#
#----------------------------------------------------------#
# Defines the target graph that obtains and checks the raw CHELSA-TraCE21k
#   palaeoclimate files (source `chelsa_trace21k` in `config.yml`, 7
#   variables by 37 ages): the expected files, the downloads behind
#   `flag_download`, one completeness check per file, the checksum record
#   behind `flag_record_checksums` and the validated file table
#   `chelsa_trace21k_raw_files`.
# With every flag false (the default) nothing is downloaded, and a missing,
#   incomplete or unrecorded file stops the run with a message that names
#   the flag to set. A full download is about 144 GB and the checksum
#   record reads about 134 GB; both need explicit approval.
# Run with:
#   R/Analyses/01_Data_acquisition/00_run.R
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

# Expected files, flag-gated downloads and checksums, one completeness
#   check per file and the validated raw files
build_source_acquisition_targets(
  sel_source = "chelsa_trace21k",
  list_data_sources = load_config_value(value = "data_sources"),
  list_data_acquisition = load_config_value(value = "data_acquisition"),
  path_data_storage = path_data_storage
)

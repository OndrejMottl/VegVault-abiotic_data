#----------------------------------------------------------#
#
#
#                     Abiotic data
#
#                    Run all stages
#
#
#                       O. Mottl
#                         2026
#
#----------------------------------------------------------#
# Runs the stage runners in order. Each stage builds its pipelines in their
#   own stores; up-to-date targets are skipped. Each runner sources the
#   project setup itself. Add every new stage runner to the list below.
# The stages process global rasters, so never run this script without
#   explicit approval (see .ai/repository-contract.md).
# The legacy scripts in R/01_Data_processing/ and R/02_Main_analyses/ stay
#   until the stages replace them.
# Run with:
#   Rscript R/Analyses/00_run_main.R


#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)

# The template conversion adds the other stages in order
vec_stage_runners <-
  c(
    "R/Analyses/01_Data_acquisition/00_run.R",
    "R/Analyses/02_Data_processing/00_run.R",
    "R/Analyses/03_Quality_checks/00_run.R"
  )


#----------------------------------------------------------#
# 1. Run stages -----
#----------------------------------------------------------#

purrr::walk(
  .x = vec_stage_runners,
  .f = ~ base::source(
    file = here::here(.x)
  )
)

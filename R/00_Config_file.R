#----------------------------------------------------------#
#
#
#                     Abiotic data
#
#                     Config file
#
#
#                       O. Mottl
#                         2023
#
#----------------------------------------------------------#
# Compatibility entry point for the legacy scripts in R/01_Data_processing/
#   and R/02_Main_analyses/, which source this file. It sources the project
#   setup and keeps the seed these scripts used. New code sources
#   R/___setup_project___.R directly. Remove this file when the template
#   stages replace the legacy scripts.


#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)

# Loads every function in R/Functions/, attaches `packages: attach` of
#   config.yml and resolves the data root. Packages are restored from
#   renv.lock by R/___Init_project___.R, no longer here. The legacy scripts
#   call every other package with its namespace or attach it themselves
source(
  here::here("R/___setup_project___.R")
)

# The seed of the legacy scripts; the setup sets `seed` of config.yml
set.seed(1234)

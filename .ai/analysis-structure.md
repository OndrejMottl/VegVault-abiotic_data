# Analysis structure

This repository follows the layout of `OndrejMottl/Project_template` (its `.ai/analysis-structure.md`). The layout is a contract: change it deliberately, update this file in the same change, and check every caller. The binding data and execution safeguards are in `.ai/repository-contract.md`.

## Layout

```text
config.yml                 project configuration, read via load_config_value()
secrets.yaml               git-ignored machine-local values (see secrets_example.yaml)
Data/                      default data root (`data_storage_path` in config.yml)
  Input/                   raw inputs; large rasters and downloads are git-ignored
  Processed/               processed rasters; the template folders are git-ignored
  Temp/                    disposable scratch, staging downloads (git-ignored)
  Targets_data/            one {targets} store per pipeline (git-ignored)
Outputs/
  Data/                    exported artefacts that VegVault reads
  Tables/                  raster quality report and other tables
R/
  ___Init_project___.R     restores renv.lock once per checkout or worktree
  ___setup_project___.R    shared setup sourced by every script and pipeline
  package_dependencies.R   generated; do not edit
  Functions/<Area>/        one function per file, named after the function
  Analyses/
    00_run_main.R          runs every stage runner in order
    <NN>_<Stage>/
      00_run.R             runs the pipelines of the stage
      pipeline.R           declares one target graph (or <Operation>/pipeline.R)
  Tests/
    Run_tests.R            full suite runner
    testthat/test-<function_name>.R
```

## Stages

The stages replace the legacy script cascade one by one. Planned order (agreed with Ondřej on 2026-11-20):

| Stage | Content |
| --- | --- |
| `01_Data_acquisition` | Downloads behind flags that default to `FALSE`; a download is accepted only after a completeness check against the server size; a tracked download record (URL, date, size, SHA-256). With every flag `FALSE` no download starts, and a missing raw file stops with a message naming the flag. |
| `02_Data_processing` | Aggregation of the CHELSA V2.1 and CHELSA-TraCE21k rasters and the HWSD soil-group raster. |
| `03_Quality_checks` | The raster quality check: fails closed and writes the band and summary tables to `Outputs/Tables/`. |
| `04_Data_extraction` | Raster to cell tables, as targets; no "latest file by date" lookups. |
| `05_Export` | Runs only after a passing quality check; writes the complete named set of `qs2` artefacts into a clean folder, validates it against the VegVault producer contracts and writes a checksum manifest. |

## Stages and pipelines

- Each stage has a `00_run.R` that sources the setup and calls `run_target_pipeline()` for each of its pipelines. `00_run_main.R` sources the stage runners in order; add each new stage runner to its list.
- A `pipeline.R` only declares targets. It sources the setup, sets `targets::tar_option_set(seed = config_seed)`, and returns a list of targets. Sourcing it never builds targets.
- Each pipeline owns one store, resolved with `resolve_pipeline_store_path()` under `<data root>/Targets_data/`. Never share a store between pipelines, and never run the same pipeline from two checkouts at once.
- Target names are quoted strings. Above each target, a short plain comment says what it provides and why it is in the graph.
- When a target command is more than one function call, move the logic into a function in `R/Functions/`.

## Cross-pipeline contracts

- A downstream pipeline imports only public targets of an upstream store, through a `fingerprint_<stage>` target (`compute_target_store_fingerprint()`, `cue = targets::tar_cue(mode = "always")`) and `load_target_store_value()`.
- A missing upstream store or target, or an upstream run that failed, is an error that names the runner to execute first (`validate_target_store_contract()`). It never triggers an expensive rebuild.

## Configuration and data root

- Analysis values live in `config.yml` and are read only through `load_config_value()`. Profiles other than `default` are selected with `R_CONFIG_ACTIVE`; `validate_config_profile()` stops on an unknown profile name.
- The data root comes from `resolve_data_storage_path()`: `Data` in `secrets.yaml` if present, otherwise `data_storage_path` in `config.yml`. A worktree sets `Data` to the main checkout's `Data` folder, so raw inputs are reconnected, never copied.
- The template pipeline writes processed rasters to new folders (`Data/Processed/CHELSA_V2.1/`, `Data/Processed/CHELSA_TraCE21k_V1.0/`, `Data/Processed/HWSD_v2.2/`). The tracked legacy rasters in `Data/Processed/Neoclimate/`, `Paleoclimate/` and `WoSIS/` stay untouched as the comparison baseline.

## Setup and packages

- Every stage runner, pipeline, and test run starts with `library(here)` and `source(here::here("R/___setup_project___.R"))`. The legacy scripts source `R/00_Config_file.R`, which now only sources the setup and sets their old seed.
- The lockfile is `renv.lock`, restored by `Rscript R/___Init_project___.R`. Package changes follow `../VegVault/.ai/r-environment.md`: approval first, `renv::install()`, list attached packages in `config.yml`, snapshot with CRAN-only repositories.

## Tests

- Every function in `R/Functions/<Area>/` has `R/Tests/testthat/test-<function_name>.R`. Shared fixtures go in `helper-*.R` files.
- `Rscript R/Tests/Run_tests.R` validates the function files (`validate_function_names()`) and runs the suite in a clean subprocess (`run_project_tests()`). The name check covers the subfolders of `R/Functions/`; the legacy functions directly in `R/Functions/` are skipped until the conversion moves them into subfolders under approved verbs.
- Focused run: source the setup, then `run_test_file(test_file = ...)`.

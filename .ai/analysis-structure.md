# Analysis structure

This repository follows the layout of `OndrejMottl/Project_template` (its `.ai/analysis-structure.md`). The layout is a contract: change it deliberately, update this file in the same change, and check every caller. The binding data and execution safeguards are in `.ai/repository-contract.md`.

## Layout

```text
config.yml                 project configuration, read via load_config_value()
secrets.yaml               git-ignored machine-local values (see secrets_example.yaml)
Data/                      default data root (`data_storage_path` in config.yml)
  Contracts/               copy of the VegVault contracts (tracked; read from the project)
  Input/                   raw inputs; large rasters and downloads are git-ignored
  Processed/               processed rasters; the template folders are git-ignored
  Temp/                    disposable scratch, staging downloads (git-ignored)
  Targets_data/            one {targets} store per pipeline (git-ignored)
Outputs/
  Data/                    exported artefacts that VegVault reads
  Data_temp/               test exports of the `dev` profile (git-ignored)
  Tables/                  raster quality report, export manifest and other tables
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
| `01_Data_acquisition` | Built. Three pipelines with their own stores (`CHELSA_V2.1`, `CHELSA_TraCE21k`, `HWSD`), declared by `build_source_acquisition_targets()`. Downloads behind `flag_download`, which defaults to `FALSE`; a download is accepted only after a completeness check (size against the server-size record and the TIFF block table, or, for the HWSD zip archives whose server announces no size, an archive that opens); existing files are never overwritten. A tracked download record per run (URL, date, size, SHA-256); `flag_record_checksums` records files obtained before the stage existed. The HWSD class tables are saved once from the Access database (`flag_read_access_database`) as tracked CSV files. With every flag `FALSE` no download starts, and a missing, incomplete or unrecorded file stops with a message naming the flag. |
| `02_Data_processing` | Built. Three pipelines with their own stores (`CHELSA_V2.1`, `CHELSA_TraCE21k`, `HWSD`) that import the validated raw files of the download stage. Climate rasters: aggregation by factor 25 with the median and `terra`'s default handling of missing cells, as in v1 (`build_climate_processing_targets()`), then the configured masks: polar fill values of CHELSA V2.1 and the South American precipitation artefact of CHELSA-TraCE21k; counts of masked cells in `Outputs/Tables/`. Soil (`build_hwsd_processing_targets()`): the dominant component of each HWSD soil unit mapped by its two-letter WRB code to the codes 1 to 32 of the VegVault specification, resampled to the raw CHELSA V2.1 grid, aggregated by the mode over all classes; a cell whose dominant class is not a soil is missing. Outputs only in the three new processed folders. |
| `03_Quality_checks` | The raster quality check: fails closed and writes the band and summary tables to `Outputs/Tables/`. |
| `04_Data_extraction` | Built. Three pipelines with their own stores (`CHELSA_V2.1`, `CHELSA_TraCE21k`, `HWSD`) that import the processed rasters of the processing stage and track each raster as a file, so a changed raster is extracted again (`build_climate_extraction_targets()`, `build_hwsd_extraction_targets()`). One cell table per processed raster, saved as a `qs2` file in the git-ignored folder `Cell_tables/` of its processed folder: one row per cell with a value, in raster cell order, with the cell-centre coordinates and the stored value, as in v1, and the columns of the VegVault producer contract in its order (`columns` in `config.yml`). No "latest file by date" lookups. |
| `05_Export` | Built. One pipeline with one store (`build_export_targets()`; `data_export`, or `data_export_dev` in the `dev` profile) that imports the cell tables of the extraction stage and waits for the passed quality check of the processed rasters (`raster_quality_validated`). It saves the artefacts that VegVault reads, named and placed as the project's copy of the VegVault contracts in `Data/Contracts/` prescribes: one file per CHELSA V2.1 variable, four batches per CHELSA-TraCE21k variable (time slices from age 0, split into batches of equal row count as in v1) and one soil file, as `qs2` with the date of the run and `rlang::hash()` in the name. Each contract folder must hold exactly the exported files (a leftover file stops the run), and every file is checked with `validate_producer_artefact()`. An earlier version of an artefact stops the run unless `flag_replace_previous` is true. Tables in `Outputs/Tables/`: `export_manifest.csv` (size, SHA-256 and rows per file, with the VegVault commit of the contract copy), `export_summary.csv` (counts and value range per variable) and `hwsd_soil_group_legend.csv`. The `dev` profile writes the same layout into the git-ignored `Outputs/Data_temp/`. |

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

- Analysis values live in `config.yml` and are read only through `load_config_value()`. Profiles other than `default` are selected with `R_CONFIG_ACTIVE`; `validate_config_profile()` stops on an unknown profile name. The `dev` profile changes only the export: its artefacts and tables go to `Outputs/Data_temp/`, with their own store.
- The VegVault contracts and their validator are a copy taken from one VegVault commit with VegVault's `save_contract_copy()`: the contracts in `Data/Contracts/` (with the record `contract_copy.yml`) and the functions in `R/Functions/Contracts/`. Never edit them here; make the copy again from an up-to-date VegVault checkout. `test-validate_contract_copy.R` checks that the copy is intact and, when `../VegVault` exists, current. `Data/Contracts/` is read from the project, not from the data root, so a worktree uses its own tracked copy.
- The data root comes from `resolve_data_storage_path()`: `Data` in `secrets.yaml` if present, otherwise `data_storage_path` in `config.yml`. A worktree sets `Data` to the main checkout's `Data` folder, so raw inputs are reconnected, never copied.
- Raw data sources are defined once, in `data_sources` of `config.yml`: the download stage reads them, and the raw-raster quality-check sets name them with `source` (`prepare_quality_check_sets()`). Tracked inputs under the data root: `Input/Server_file_sizes/` (server-size records), `Input/Download_records/` (one CSV per run and product, never overwritten) and `Input/HWSD_tables/` (the HWSD class tables); the extracted HWSD files in `Input/HWSD/` are git-ignored.
- The template pipeline writes processed rasters to new folders (`Data/Processed/CHELSA_V2.1/`, `Data/Processed/CHELSA_TraCE21k_V1.0/`, `Data/Processed/HWSD_v2.2/`), and the extraction stage writes its cell tables into their `Cell_tables/` subfolders. The tracked legacy rasters in `Data/Processed/Neoclimate/`, `Paleoclimate/` and `WoSIS/` stay untouched as the comparison baseline.

## Setup and packages

- Every stage runner, pipeline, and test run starts with `library(here)` and `source(here::here("R/___setup_project___.R"))`. The legacy scripts source `R/00_Config_file.R`, which now only sources the setup and sets their old seed.
- The lockfile is `renv.lock`, restored by `Rscript R/___Init_project___.R`. Package changes follow `../VegVault/.ai/r-environment.md`: approval first, `renv::install()`, list attached packages in `config.yml`, snapshot with CRAN-only repositories.

## Tests

- Every function in `R/Functions/<Area>/` has `R/Tests/testthat/test-<function_name>.R`. Shared fixtures go in `helper-*.R` files.
- `Rscript R/Tests/Run_tests.R` validates the function files (`validate_function_names()`) and runs the suite in a clean subprocess (`run_project_tests()`). The name check covers the subfolders of `R/Functions/`; the legacy functions directly in `R/Functions/` are skipped until the conversion moves them into subfolders under approved verbs.
- Focused run: source the setup, then `run_test_file(test_file = ...)`.

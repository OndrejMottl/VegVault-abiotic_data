# VegVault: abiotic data

This repository prepares the climate and soil data of [VegVault](https://github.com/OndrejMottl/VegVault), a database that links vegetation plots, fossil pollen records and plant traits with the abiotic conditions of their place and time. It is part of the [BIODYNAMICS project](https://ondrejmottl.github.io/projects/BIODYNAMICS/).

The repository turns three global raster products into tables of grid cells that VegVault imports:

| Product | Content | Time |
| --- | --- | --- |
| CHELSA V2.1 | bioclimatic variables bio1, bio4, bio6, bio12, bio15, bio18 and bio19 | 1981 to 2010 |
| CHELSA-TraCE21k V1.0 | the same seven bioclimatic variables | 37 time slices, every 500 years from 0 to 18,000 years before present |
| HWSD v2.2 | the WRB 2022 Reference Soil Group of the dominant soil | present |

The version, download dates, checksums, licence and citation of each source are recorded in VegVault's source manifest (`Data/Contracts/source_manifest.csv` in the VegVault repository).

## What the pipeline does

The work runs as five stages of [`{targets}`](https://docs.ropensci.org/targets/) pipelines in `R/Analyses/`:

1. **Data acquisition** downloads the raw rasters and the HWSD data only when a download flag in `config.yml` is set, and accepts a file only after a completeness check (its size against the size the server reports). Every download is recorded with its date, size and SHA-256 checksum in `Data/Input/Download_records/`.
2. **Data processing** aggregates the climate rasters by a factor of 25 with the median (about 0.21 degrees, 1,728 by 836 cells), masks known artefacts of the sources, and maps each HWSD soil unit to the code of its dominant soil group on the same grid.
3. **Quality checks** test the raw and processed rasters (file sizes, grid, storage limits, latitude bands, quarterly against annual precipitation) and stop the pipeline when a required set fails.
4. **Data extraction** turns every processed raster into a table with one row per grid cell that has a value.
5. **Export** saves the tables as the files VegVault reads, checks them against the VegVault data contracts and writes a checksum manifest.

## Outputs

The release files are `qs2` files (read them with `qs2::qs_read()`, although the extension is `.qs`):

| Folder | Files | Columns |
| --- | --- | --- |
| `Outputs/Data/Neoclimate/` | `CHELSA_bio_<NN>_<date>__<hash>__.qs`, one per variable | `long`, `lat`, `value`, `var_name` |
| `Outputs/Data/Palaoclimate/` | `bio<NN>_batch_<1-4>_<date>__<hash>__.qs`, four batches per variable | `long`, `lat`, `time_id`, `value` |
| `Outputs/Data/HWSD/` | `hwsd_data_<date>__<hash>__.qs` | `long`, `lat`, `value`, `var_name` |

`long` and `lat` are the cell centres in decimal degrees (WGS84). `time_id` is the CHELSA-TraCE21k time slice; the age in years before present is `-time_id * 100 + 2000`. The folder name `Palaoclimate` is kept from earlier releases because VegVault reads it.

Points to keep in mind:

- A cell without a value, or masked as an artefact of the source, has no row.
- In CHELSA V2.1 `bio12`, the value 6553.5 is the storage cap of the source product: it means at least 6553.5 mm per year.
- `bio4` is in hundredths of a degree Celsius.
- The soil codes 1 to 32 are listed with their WRB code and name in `Outputs/Tables/hwsd_soil_group_legend.csv`.

`Outputs/Tables/` also holds the raster quality report (`raster_quality_*`), the counts of masked cells and soil units (`processing_*`), the checksum manifest of the release files (`export_manifest.csv`) and the counts and value ranges per variable (`export_summary.csv`).

## Running the pipeline

The pipeline needs R 4.6 and about 140 GiB of raw data (134 GiB of it CHELSA-TraCE21k), which is not stored in git.

1. Restore the packages once: `Rscript R/___Init_project___.R`.
2. Copy `secrets_example.yaml` to `secrets.yaml` and set the data root that holds the raw inputs and the target stores.
3. Run all stages: `Rscript R/Analyses/00_run_main.R`. Up-to-date targets are skipped, nothing is downloaded unless a flag is set, and the export never overwrites an earlier file.
4. Run the tests: `Rscript R/Tests/Run_tests.R`.

The profile `dev` (`R_CONFIG_ACTIVE=dev`) writes the export into the git-ignored `Outputs/Data_temp/` for test runs. The layout of the repository, the stages and the configuration are described in `.ai/analysis-structure.md`.

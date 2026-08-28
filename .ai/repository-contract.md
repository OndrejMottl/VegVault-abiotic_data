# VegVault-abiotic_data Repository Contract

## Role and outputs

This repository owns acquisition and processing of CHELSA v2.1 neoclimate, CHELSA-TraCE21k palaeoclimate, and WoSIS soil data. Its reviewed products under `Outputs/Data/Neoclimate/`, `Outputs/Data/Palaoclimate/`, and `Outputs/Data/WoSIS/` are consumed by VegVault.

Treat filenames, coordinate reference systems, spatial resolution, time slices, variable names, units, missing-value conventions, and table/raster structure as release interfaces.

## Safety

- Climate and soil downloads, raster extraction, reprojection, resampling, mosaicking, and batch processing can be expensive in time, memory, storage, and network use. Do not run them broadly without explicit user authorization.
- Preserve `rewrite_files = FALSE` defaults. Do not enable overwrite behavior without identifying and confirming every affected path.
- Never add ignored downloads, raster caches, temporary extracts, or large generated products to Git.
- Verify CRS, resolution, extent, time basis, and units on small representative inputs before any authorized batch run.
- Keep debugging artifacts in ignored temporary locations and preserve existing partial/download-resume behavior.

## Change and validation contract

For changes to output shape or meaning, trace consumers in `../VegVault/R/02_Main_analyses/07_Import_chelsa_neo_climate_data.R`, `../VegVault/R/02_Main_analyses/08_Import_chelsa_trace21k_climate_data.R`, and `../VegVault/R/02_Main_analyses/09_Import_wosis_soil_data.R`. Coordinate producer documentation and a reviewed tag, then update the pinned integration reference. Validate representative files and metadata rather than rerunning the full raster workflow solely for documentation changes.

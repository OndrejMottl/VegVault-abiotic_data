# VegVault-abiotic_data Repository Contract

## Role and outputs

This repository acquires and processes the CHELSA V2.1 present-day climate, the CHELSA-TraCE21k V1.0 palaeoclimate and the HWSD v2.2 soil data for VegVault. Its release artefacts are the files in `Outputs/Data/Neoclimate/`, `Outputs/Data/Palaoclimate/` and `Outputs/Data/HWSD/`, written by the export stage (`R/Analyses/05_Export/`) and described by the VegVault contracts `chelsa_neoclimate`, `chelsa_trace21k_palaeoclimate` and `hwsd_soil` (the project's copy is in `Data/Contracts/`). The tables in `Outputs/Tables/` (raster quality report, processing counts, export manifest, export summary and soil-group legend) belong to each release.

Treat file names, folders, batches, coordinate reference system, grid, time slices, variable names, units, codes, missing-value conventions and table structure as release interfaces.

## Safety

- Downloads, raster processing, extraction, quality checks of global rasters and the export can be expensive in time, memory, storage and network use. Do not run them broadly without explicit user authorization.
- Keep the no-overwrite defaults: the download flags in `config.yml` default to `FALSE`, an existing download is never overwritten, and `flag_replace_previous` of the export is `false`. Do not change them without identifying and confirming every affected path.
- A download is accepted only after its completeness check against the server sizes recorded in `Data/Input/Server_file_sizes/` (or, for an archive without an announced size, an archive that opens), never by a minimum file size.
- Never add ignored downloads, raster caches, cell tables, temporary extracts or test exports to Git. Test exports go to the git-ignored `Outputs/Data_temp/` (profile `dev`); only the release artefacts in `Outputs/Data/` are tracked, because VegVault fetches them by tag.
- Verify grid, resolution, extent, time basis and units on small representative inputs before any authorized batch run.
- Keep debugging artefacts in ignored temporary locations.
- Never edit the copy of the VegVault contracts (`Data/Contracts/`, `R/Functions/Contracts/`); make it again with VegVault's `save_contract_copy()`.

## Release checks

- A release needs a run of `R/Analyses/03_Quality_checks/` in which every required set passes, with the summary and findings tables of every set committed in `Outputs/Tables/`. The band and pair tables are git-ignored and kept with the release evidence.
- The export validates every artefact against the project's copy of the contracts and stops unless each contract folder holds exactly the exported files. Before tagging, run `test-validate_contract_copy.R` against an up-to-date VegVault checkout on the release branch, make the copy again if a contract changed, and state the VegVault commit of the copy (in `Outputs/Tables/export_manifest.csv`) and the contract versions in the release notes.
- The release notes state the documented source properties: in CHELSA V2.1 `bio12`, 6553.5 is the storage cap of the source and means at least 6553.5 mm per year; masked or missing cells have no row; the soil codes 1 to 32 follow `Outputs/Tables/hwsd_soil_group_legend.csv`.

## Change and validation contract

A change to the shape or meaning of an output starts as a contract change in VegVault (see `Data/Contracts/README.md` there); then make the copy here again and adapt the export. Trace the users that each contract lists under `consumers` and VegVault's producer import stage, which validates every pinned file against its contract. VegVault pins the release files by tag and checksum in `Data/Producers/producer_manifest.yml`, so a release hands the checksums of `export_manifest.csv` to VegVault. Validate representative files and metadata rather than rerunning the full raster workflow solely for documentation changes.

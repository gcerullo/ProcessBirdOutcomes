# ProcessBirdOutcomes

Wrangles Borneo point-count data, fits multi-species Bayesian occupancy models, and evaluates scenario outcomes through time.

## Core Input Folders

- `RawData/`: raw point-count datasets used to build the combined bird table.
- `Inputs/`: scenario objects, scenario parameter script, trait/crosswalk/LiDAR covariates.
- `Outputs/`: intermediate files consumed by downstream scripts.

## Core Output Folders

- `Outputs/`: primary intermediate and scenario-result files.
- `Figures/`: exported figures from plotting scripts.
- `FinalPerformanceOutput/`: final outcome summary RDS files.
- `Outputs/NR2/`: folder used by `Scripts/Nature_Revision_2/`.

## Legacy Scripts (`Scripts/`)

- `01_Combine_Bird_Datasets.R`: builds combined point-count dataset with covariates.
- `02_FormatBirdForFlockr.R`: reshapes bird detections/covariates into flocker format.
- `03_FitModel.R`: fits occupancy model.
- `03B_ExtractThinnedDraws.R`: extracts thinned posterior occupancy draws.
- `04_AssessScenarioOutcomes.R`: propagates occupancy uncertainty through scenarios.
- `05_spp_example_error_propagation.R`: worked step-by-step uncertainty example.
- `06_scenario_uncertainty_plots.R`: plots uncertainty outputs.

## Nature_Revision_2 Scripts (`Scripts/Nature_Revision_2/`)

These are full copies of the core scripts, kept separate for revision work.

- `00_config.R`: defines helper functions and creates `Outputs/NR2` subfolders.
- `01_combine_bird_datasets.R`
- `02_format_bird_for_flockr.R`
- `03_fit_model.R`
- `03B_extract_thinned_draws.R`
- `04_assess_scenario_outcomes.R`
- `05_spp_example_error_propagation.R`
- `06_scenario_uncertainty_plots.R`
- `run_all.R`: sequential runner for the NR2 scripts.

## Required Inputs By Nature_Revision_2 Script

### `00_config.R`

- No file inputs required.

### `01_combine_bird_datasets.R`

- `RawData/DE_2008_2011_forest_pointCounts.csv`
- `RawData/plantationPCs_2022.csv`
- `RawData/SimonMitchellSelectedPointCounts.csv`
- All CSV files in `RawData/all_PC_forest_site_CSVs_2022/`
- `Inputs/SimonDaveSpeciesNamesCrossWalk.csv`
- `Inputs/GEE_AllPointCountsABC_CTH_50m_100m_buffer.csv`
- `Inputs/PC_Dates_Times_2022.csv`
- `Inputs/TimesDatesDavePointCounts.csv`

### `02_format_bird_for_flockr.R`

- `Outputs/NR2/rds/birdDataForFlockr.csv` (created by script 01)

### `03_fit_model.R`

- `Outputs/NR2/models/fd_28-05-24.rds` (created by script 02)

### `03B_extract_thinned_draws.R`

- Preferred: `Outputs/NR2/models/fit.rds` and `Outputs/NR2/models/fd_28-05-24.rds`
- Fallback accepted by script: `outputs/fit_2024-07-12.rds` and `outputs/fd_2024-07-12.rds`

### `04_assess_scenario_outcomes.R`

- `Inputs/ScenarioParams.R`
- `Inputs/MasterAllScenarios.rds`
- All CSV files in `Inputs/ScenariosWithDelaysCSVs/`
- `Inputs/occ500drawsSept24.rds`
- `Inputs/AllBorneoSpeciesTraits.csv`
- During execution, this script reads files it creates earlier in the same run under `Outputs/NR2/rds/` (for example `processedOccBirdsSept24.rds`, `sppCategoriesSept24.rds`, `SLoccOutputs/`, and `occ60PerScenarioIterationJan25/`).

### `05_spp_example_error_propagation.R`

- `Inputs/ScenarioParams.R`
- `Inputs/MasterAllScenarios.rds`
- All CSV files in `Inputs/ScenariosWithDelaysCSVs/`
- `Outputs/NR2/rds/processedOccBirdsSept24.rds` (typically produced by script 04)

### `06_scenario_uncertainty_plots.R`

- All RDS files in `Outputs/NR2/rds/BestScenarioUncertainty/`
- `Outputs/NR2/rds/sppCategoriesSept24.rds`

## Recommended Execution Order (Nature_Revision_2)

From project root in R:

1. `source("Scripts/Nature_Revision_2/00_config.R")`
2. `nr2_init(".", verbose = TRUE)`
3. `source("Scripts/Nature_Revision_2/01_combine_bird_datasets.R")`
4. `source("Scripts/Nature_Revision_2/02_format_bird_for_flockr.R")`
5. `source("Scripts/Nature_Revision_2/03_fit_model.R")`
6. `source("Scripts/Nature_Revision_2/03B_extract_thinned_draws.R")`
7. `source("Scripts/Nature_Revision_2/04_assess_scenario_outcomes.R")`
8. `source("Scripts/Nature_Revision_2/05_spp_example_error_propagation.R")`
9. `source("Scripts/Nature_Revision_2/06_scenario_uncertainty_plots.R")`
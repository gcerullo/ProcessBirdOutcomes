# Nature_Revision_2 Scripts

This folder contains the `Nature_Revision_2` workflow scripts.

- Original scripts in `Scripts/` are not modified.
- `Nature_Revision_2` scripts write to `Outputs/NR2/` locations.

## NR2 Output Structure

- `Outputs/NR2/models`: model objects and model-related files.
- `Outputs/NR2/rds`: tabular/intermediate/final RDS and CSV outputs.
- `Outputs/NR2/figures`: figure files.

## Run Order

1. `00_config.R`
2. `01_combine_bird_datasets.R`
3. `02_format_bird_for_flockr.R`
4. `03_fit_model.R`
5. `03B_extract_thinned_draws.R`
6. `04_assess_scenario_outcomes.R`
7. `05_spp_example_error_propagation.R`
8. `06_scenario_uncertainty_plots.R`

`run_all.R` executes the sequence above.

---

## Script-by-Script Purpose, Inputs, and Outputs

### `00_config.R`

- **Main purpose:** Define shared NR2 paths and helper functions; create NR2 output directories.
- **Required inputs:** None.
- **Outputs created:**
  - `Outputs/NR2/`
  - `Outputs/NR2/models/`
  - `Outputs/NR2/rds/`
  - `Outputs/NR2/figures/`

### `01_combine_bird_datasets.R`

- **Main purpose:** Merge and harmonize raw bird point-count datasets with site/species covariates and export combined datasets.
- **Required inputs:**
  - `RawData/DE_2008_2011_forest_pointCounts.csv`
  - `RawData/plantationPCs_2022.csv`
  - `RawData/SimonMitchellSelectedPointCounts.csv`
  - All CSV files in `RawData/all_PC_forest_site_CSVs_2022/`
  - `Inputs/SimonDaveSpeciesNamesCrossWalk.csv`
  - `Inputs/GEE_AllPointCountsABC_CTH_50m_100m_buffer.csv`
  - `Inputs/PC_Dates_Times_2022.csv`
  - `Inputs/TimesDatesDavePointCounts.csv`
  - `Inputs/Plantation_Habitat_Structure.csv`
  - `Inputs/SppForForIntegrityIUCN.csv`
- **Outputs created:**
  - `Outputs/NR2/rds/allDanielDaveSimonDetectionsWithLidar.csv`
  - `Outputs/NR2/rds/birdDataForFlockr.csv`

### `02_format_bird_for_flockr.R`

- **Main purpose:** Convert bird detections and covariates into flocker model input objects.
- **Required inputs:**
  - `Outputs/NR2/rds/birdDataForFlockr.csv`
- **Outputs created:**
  - `Outputs/NR2/models/fd_28-05-24.rds`
  - `Outputs/NR2/models/fd_no_visit_cov_28-05-24.rds`

### `03_fit_model.R`

- **Main purpose:** Fit the multi-species occupancy model.
- **Required inputs:**
  - `Outputs/NR2/models/fd_28-05-24.rds`
- **Outputs created:**
  - `Outputs/NR2/models/fit.rds`
  - `Outputs/NR2/models/fit_backup.rds`
  - Additional cmdstan outputs in `Outputs/NR2/models/`

### `03B_extract_thinned_draws.R`

- **Main purpose:** Generate thinned posterior occupancy draws and key model-response figures.
- **Required inputs:**
  - Preferred:
    - `Outputs/NR2/models/fit.rds` (or `Outputs/NR2/models/fit_backup.rds`)
    - `Outputs/NR2/models/fd_28-05-24.rds`
  - Fallback supported in script:
    - `outputs/fit_2024-07-12.rds`
    - `outputs/fd_2024-07-12.rds`
- **Outputs created:**
  - `Outputs/NR2/rds/predicted_occupancy_500_draws.rds`
  - `Outputs/NR2/rds/predicted_occupancy_500_draws_summarised.rds`
  - `Outputs/NR2/figures/plantation_age_estimates.png`
  - `Outputs/NR2/figures/all_birs_curves.png`

### `04_assess_scenario_outcomes.R`

- **Main purpose:** Process occupancy draws through scenario calculations, produce relative occupancy outputs, and generate final bird outcome summaries.
- **Required inputs:**
  - `Inputs/ScenarioParams.R` (must define `all_start_landscape`, `bird_CF`, `total_bird_pts`)
  - `Inputs/MasterAllScenarios.rds`
  - All CSV files in `Inputs/ScenariosWithDelaysCSVs/`
  - `Inputs/AllBorneoSpeciesTraits.csv`
  - `Outputs/NR2/rds/predicted_occupancy_500_draws.rds`
- **Outputs created:**
  - `Outputs/NR2/rds/processedOccBirds.rds`
  - `Outputs/NR2/rds/sppCategories.rds`
  - `Outputs/NR2/rds/SLoccOutputs/SL_occ60yr_perIteration.rds`
  - `Outputs/NR2/rds/occ60PerScenarioIteration/*.rds`
  - `Outputs/NR2/rds/Rel_Occ_PerIteration/*.rds`
  - `Outputs/NR2/rds/BestScenarioUncertainty/*.rds`
  - `Outputs/NR2/rds/OG_baseline_birds.rds`
  - `Outputs/NR2/rds/OG_baseline_birdsIUCN.rds`
  - `Outputs/NR2/rds/species_level_relative_occ.rds`

### `05_spp_example_error_propagation.R`

- **Main purpose:** Diagnostic step-by-step script to inspect uncertainty propagation logic.
- **Required inputs:**
  - `Inputs/ScenarioParams.R`
  - `Inputs/MasterAllScenarios.rds`
  - All CSV files in `Inputs/ScenariosWithDelaysCSVs/`
  - `Outputs/NR2/rds/processedOccBirdsSept24.rds`
- **Outputs created:** No explicit saved files (interactive checks/objects in session).
- **Naming note:** This script currently expects `processedOccBirdsSept24.rds`, while script `04_assess_scenario_outcomes.R` writes `processedOccBirds.rds`.

### `06_scenario_uncertainty_plots.R`

- **Main purpose:** Build and export uncertainty figures comparing when selective logging vs plantations perform best, including threshold-specific plots.
- **Required inputs:**
  - All RDS files in `Outputs/NR2/rds/BestScenarioUncertainty/`
  - `Outputs/NR2/rds/sppCategories.rds`
- **Outputs created:**
  - `Outputs/NR2/figures/loser_bird_uncertainty_plot.pdf`
  - `Outputs/NR2/figures/int1lgrp_bird_uncertainty_plot.pdf`
  - `Outputs/NR2/figures/all_sp_uncertainty_plot.pdf`
  - `Outputs/NR2/figures/all_sp_chunk1.pdf`
  - `Outputs/NR2/figures/all_sp_chunk2.pdf`
  - `Outputs/NR2/figures/loser_bird_uncertainty_plot.png`
  - `Outputs/NR2/figures/int1lgrp_bird_uncertainty_plot.png`
  - `Outputs/NR2/figures/all_sp_uncertainty_plot.png`
  - `Outputs/NR2/figures/all_sp_chunk1.png`
  - `Outputs/NR2/figures/all_sp_chunk2.png`
  - `Outputs/NR2/figures/loser_uncertainty_multiple_production_targets.pdf`
  - `Outputs/NR2/figures/loser_uncertainty_multiple_production_targets.png`
  - `Outputs/NR2/figures/loser_uncertainty_all_scenarios.pdf`
  - `Outputs/NR2/figures/loser_uncertainty_all_scenarios.png`
  - `Outputs/NR2/figures/loser_uncertainty_production_gt_05.pdf`
  - `Outputs/NR2/figures/loser_uncertainty_production_gt_05.png`
  - `Outputs/NR2/figures/loser_uncertainty_production_gt_075.pdf`
  - `Outputs/NR2/figures/loser_uncertainty_production_gt_075.png`

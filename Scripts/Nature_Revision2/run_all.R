# =============================================================================
# Self-notes — NR2 bird pipeline
# =============================================================================
# What I'm doing: I run my full Borneo bird → flocker → scenarios workflow in order so I
#   don't forget a step when I re-build `Outputs/NR2`.
#
# What I need (inputs): Everything downstream expects (raw point counts, `Inputs/ScenarioParams.R`,
#   scenario CSVs, etc.); see each sourced script.
#
# What I produce (outputs): Whatever each step writes under `Outputs/NR2` (RDS, models, figures).
# =============================================================================

# NR2 master runner
# Executes the full NR2 workflow in script order.

source("Scripts/Nature_Revision_2/00_config.R")
nr2_init(".", verbose = TRUE)

# Core pipeline
source("Scripts/Nature_Revision_2/01_combine_bird_datasets.R")
source("Scripts/Nature_Revision_2/02_format_bird_for_flockr.R")
source("Scripts/Nature_Revision_2/03_fit_model.R")
source("Scripts/Nature_Revision_2/03B_extract_thinned_draws.R")
source("Scripts/Nature_Revision_2/04_assess_scenario_outcomes.R")
source("Scripts/Nature_Revision_2/05_scenario_uncertainty_plots.R")

message("NR2 pipeline finished. Review Outputs/NR2 for all copied outputs.")

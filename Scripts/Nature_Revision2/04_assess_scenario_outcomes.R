# =============================================================================
# Self-notes — NR2 bird pipeline
# =============================================================================
# What I'm doing: I'm propagating predicted occupancy through landscape scenarios—starting-landscape
#   and scenario cumulative occupancy over time, species categories, relative occupancy vs all-primary,
#   delta abundance with uncertainty, and RDS for downstream summaries and best-scenario uncertainty.
#
# What I need (inputs): `Inputs/ScenarioParams.R`, predicted draws RDS, scenario CSVs, and I write/read
#   intermediates under `Outputs/NR2/rds` (see body for canonical names).
#
# What I produce (outputs): Processed birds, SL/scenario occ RDS, `Rel_Occ_PerIteration/OGbaseline_*.rds`,
#   `BestScenarioUncertainty/`, `OG_baseline_birds.rds`, IUCN and species-level RDS, etc.
# =============================================================================

#GC 11/06/24

source("Scripts/Nature_Revision_2/00_config.R")
nr2_paths <- nr2_init(".", verbose = FALSE)
nr2_rds_dir <- nr2_paths$rds_dir
sl_occ_dir <- file.path(nr2_rds_dir, "SLoccOutputs")
scenario_occ_dir <- file.path(nr2_rds_dir, "occ60PerScenarioIteration")
rel_occ_dir <- file.path(nr2_rds_dir, "Rel_Occ_PerIteration")
best_scenario_dir <- file.path(nr2_rds_dir, "BestScenarioUncertainty")

# Canonical NR2 file names for this script (no date suffixes)
processed_birds_file <- file.path(nr2_rds_dir, "processedOccBirds.rds")
spp_categories_file <- file.path(nr2_rds_dir, "sppCategories.rds")
predicted_draws_file <- file.path(nr2_rds_dir, "predicted_occupancy_500_draws.rds")
sl_occ_file <- file.path(sl_occ_dir, "SL_occ60yr_perIteration.rds")
final_output_file <- file.path(nr2_rds_dir, "OG_baseline_birds.rds")
final_iucn_output_file <- file.path(nr2_rds_dir, "OG_baseline_birdsIUCN.rds")
species_level_output_file <- file.path(nr2_rds_dir, "species_level_relative_occ.rds")

dir.create(sl_occ_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(scenario_occ_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(rel_occ_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(best_scenario_dir, recursive = TRUE, showWarnings = FALSE)

library(tidyverse)
library(ggplot2)
library(data.table)
library(dplyr)
library(ggpubr)
library(stringr) 
library(cowplot)
library(foreach)
library(doParallel)
library(purrr)
library(profvis)
install.packages("bayestestR", repos = "https://easystats.r-universe.dev")
library(bayestestR)

#read in the scenario parametres containing conversion factors for converting from point to parcel/entire landscape  
source("Inputs/ScenarioParams.R")
if (!exists("all_start_landscape") || !exists("bird_CF") || !exists("total_bird_pts")) {
  stop("Missing required objects from Inputs/ScenarioParams.R: all_start_landscape, bird_CF, total_bird_pts")
}

#this allows table joins to be 100 million rows (instead of 50,000,000 )
options(datatable.cautious = 100e6)

# ── Inputs ────────────────────────────────────────────────────────────────  


#-----read in scenarios without delays to get scenario composition -------
scenarios <- readRDS("Inputs/MasterAllScenarios.rds")
scenario_composition <- rbindlist(scenarios, use.names=TRUE) # get scenario composition
starting_scenario_structure <- scenario_composition %>% 
  select(production_target, index, scenarioStart) %>% unique()
rm(scenarios)

#read in scenarios WITH delays, where every scenarioType is a single csv
#NB this is the same as MasterAllScenarios_withDelays.rds, except that all list elements are csvs


#get the csv file name for each scenario 
csv_folder <- "Inputs/ScenariosWithDelaysCSVs"
csv_files <- list.files(csv_folder, pattern = "*.csv", full.names = TRUE)

#----read in properly thinned occ500draws for each bird 
if (!file.exists(predicted_draws_file)) {
  stop("Missing required NR2 input file: Outputs/NR2/rds/predicted_occupancy_500_draws.rds")
}
birds <- readRDS(predicted_draws_file)
#----read in Bird IUCN information 
IUCN_classification <- read.csv("Inputs/AllBorneoSpeciesTraits.csv") %>% 
  select(spp, redlistCategory) %>% unique() %>% as.data.table() %>%  
  # Update `redlistCategory` for specific species
  mutate(redlistCategory = case_when(
    spp == "Ferruginous Babbler" ~ "Least Concern",
    spp == "White-chested Babbler" ~ "Near Threatened",
    spp == "Rufous-tailed Shama" ~ "Near Threatened",
    spp == "Chestnut-necklaced Partridge" ~ "Vulnerable",
    spp == "Short-tailed Babbler" ~ "Near Threatened",
    spp == "White-rumped Shama" ~ "Least Concern",
    spp == "Olive-backed Woodpecker" ~ "Near Threatened",
    spp == "Blue-banded Pitta" ~ "Least Concern",
    spp == "Banded Kingfisher" ~ "Least Concern",
    spp == "Banded Woodpecker" ~ "Least Concern",
    TRUE ~ redlistCategory # Keep original values
  )) %>% 
  mutate(threatened = case_when(
    redlistCategory != "Least Concern" & redlistCategory != "" ~ "Y",
    TRUE ~ "N"
  )) %>% 
   rename(species = spp)


# ------ PROCESS BIRD OCCUPANCY DATA ACROSS POSTERIOR DRAWS  ------------
#shows birds separately for each posterior draw iteration;
birds_raw <- birds %>% 
  mutate(habitat = case_when(
    habitat == "Albizia_falcataria" ~ "albizia_current",
    habitat == "Eucalyptus_pellita" ~ "eucalyptus_current",
    habitat == "Twice_logged" ~ "twice-logged",
    habitat == "Once_logged" ~ "once-logged",
    habitat == "Restored" ~ "restored",
    habitat == "Primary" ~ "primary",
    TRUE ~ habitat)) %>%
  pivot_longer(cols = 25:last_col(), 
               names_to = "iteration", 
               values_to = "occ") %>% 
  select(species,habitat, dependency, time_since_logging, plantation_age, occ, iteration) %>% 
  
  mutate(functionalhabAge = coalesce(time_since_logging, plantation_age)) %>% 
  select(-c(time_since_logging, plantation_age)) %>% 
  
  
  mutate(functionalhabAge = case_when(
    habitat == "primary" ~ 0,
    habitat == "twice-logged" ~ 0,
    TRUE ~ functionalhabAge
  )) 

#missing years
birds_raw %>% filter(habitat == "once-logged") %>% select(functionalhabAge) %>% unique %>% arrange
birds_raw %>% filter(habitat == "restored") %>% select(functionalhabAge) %>% unique %>% arrange

#---- process bird data ----

#define missing field data years where we want to replace model-derived estimates
ages <- data.frame(functionalhabAge = rep(seq(0,60))) 
#early years of once-logged and restored 
missing1L_R_rs <- data.frame(functionalhabAge = rep(seq(0,18))) 
#late years of restored
missing20yrsR <- data.frame(functionalhabAge = rep(seq(41,60))) 


#function to process raw bird data across posterior distributions
process_birds_data <- function(x) {
  library(dplyr)
  
  # Step 1: Filter and prepare data for "primary" and "twice-logged" habitats
  P_2L <- x %>% 
    filter(habitat == "primary"|habitat == "twice-logged") %>% 
    select(-functionalhabAge) %>%
    group_by(species,habitat,iteration) %>% 
    crossing(ages) 
  
  # Remove "primary" and "twice-logged" data and add back in for ages 0-60
  birds <- x %>%
    filter(!(habitat %in% c("primary", "twice-logged"))) %>%
    bind_rows(P_2L) 
  
  
  # # Step 2: Add "improved" plantation varieties
  # improved <- birds %>%
  #   filter(habitat %in% c("eucalyptus_current", "albizia_current")) %>%
  #   mutate(
  #     habitat = case_when(
  #       habitat == "eucalyptus_current" ~ "eucalyptus_improved",
  #       habitat == "albizia_current" ~ "albizia_improved",
  #       TRUE ~ habitat
  #     )
  #   )
  #  birds <- bind_rows(birds, improved)
  
  # Step 3: Add missing years and assume 19 years of recovery for "once-logged" and "restored"
  
  #assume each year before functionalhabAge 19 has the same occupancy as yr 19 (i.e immediate recovery to yr 19 levels) 
  birds_1L_19 <- birds %>% 
    filter(habitat == "once-logged" & functionalhabAge == 19) %>% 
    select(-functionalhabAge) %>%
    group_by(species,habitat,iteration) %>% 
    crossing(missing1L_R_rs) 
  
  birds_R_19 <- birds %>% 
    filter(habitat == "restored" & functionalhabAge == 19) %>% 
    select(-functionalhabAge) %>%
    group_by(species,habitat,iteration) %>% 
    crossing(missing1L_R_rs) 
  
  #remove 1L and restored model-interpolated yrs and add yr 19 levels
  birds <- birds %>%
    filter(!(habitat %in% c("once-logged", "restored") & functionalhabAge < 19 )) %>%
    bind_rows(birds_1L_19) %>%  
    bind_rows(birds_R_19)
  
  
  # Step 4: Assume no further recovery for "restored" habitat beyond ~40 years
  birds_R_40 <- birds %>% 
    filter(habitat == "restored" & functionalhabAge == 40) %>% 
    select(-functionalhabAge) %>%
    group_by(species,habitat,iteration) %>% 
    crossing(missing20yrsR) 
  
  #remove restored beyond 40 yrs and add in plateud data 
  birds <- birds %>%
    filter(!(habitat %in% c("restored") & functionalhabAge > 40 )) %>%
    bind_rows(birds_R_40) 
  
  # Step 5: Remove data for "eucalyptus" habitat beyond 6 years
  birds <- birds %>%
    filter(!(habitat %in% c("eucalyptus_current", "eucalyptus_improved") & functionalhabAge > 6))
  
  # Step 6: Interpolate deforested data from early plantation years
  birds_deforested <-  birds %>%
    filter((habitat %in% c("eucalyptus_current", "albizia_current")) & (functionalhabAge %in% c(0, 1, 2))) %>% 
    select(-c(functionalhabAge,habitat)) %>%
    group_by(species,iteration) %>% 
    mutate(occ = mean(occ)) %>%  
    crossing(ages) %>% 
    cbind(habitat = "deforested")
  
  birds<- birds %>% rbind(birds_deforested) %>% 
    filter(functionalhabAge < 62)
  
  return(birds)
}

# Call the function to process your 'birds' data frame
processed_birds <- process_birds_data(birds_raw)
processed_birds <- as.data.table(processed_birds)

#---- save processed birds ----
saveRDS(processed_birds, processed_birds_file)

#----can start HERE ----
processed_birds <- readRDS(processed_birds_file)

#remove improved improved yields
processed_birds <- as.data.table(processed_birds)
# Filter out rows where 'habitat' contains the string "improved" (case-insensitive)
processed_birds <- processed_birds[!grepl("improved", habitat, ignore.case = TRUE)]


#----summarise across posterior draws (for calculating spp categories only) ----

#calculate confidence intervals 
birds <- processed_birds[, .(occ = mean(occ), 
                             occ_lwr = quantile(occ, 0.2), 
                             occ_upr = quantile(occ, 0.8)),
                         by = .(species, habitat, functionalhabAge, dependency)]

#-----calculate species categories ----- 
losers <- birds %>% 
  filter(functionalhabAge < 30) %>%  
  group_by(species)  %>%  
  filter(occ == max(occ)) %>% 
  mutate(spp_category = case_when(habitat =="primary" ~"loser", TRUE ~ NA_character_)) %>% 
  filter(spp_category == "loser") %>% select(species,spp_category) %>% unique()


intermediates1L <- birds %>%
  filter(functionalhabAge < 30) %>%  
  group_by(species) %>% filter(occ == max(occ)) %>% 
  mutate(spp_category = case_when(habitat =="once-logged" ~"intermediate1L",
                                  habitat == "restored" ~ "intermediate1L",
                                  TRUE ~ NA_character_)) %>% 
  filter(spp_category == "intermediate1L") %>% select(species,spp_category) %>% unique

intermediates2L <- birds %>%
  filter(functionalhabAge < 30) %>%  
  group_by(species) %>% filter(occ == max(occ)) %>% 
  mutate(spp_category = case_when(habitat =="twice-logged" ~"intermediate2L",
                                  TRUE ~ NA_character_)) %>% 
  filter(spp_category == "intermediate2L") %>% select(species,spp_category) %>% unique

winners <- birds %>%
  filter(functionalhabAge < 30) %>%  
  group_by(species) %>%
  filter(occ == max(occ)) %>%
  mutate(spp_category = case_when(
    habitat == "albizia_current" ~ "winner",
    habitat == "eucalyptus_current" ~ "winner",
    TRUE ~ NA_character_
  )) %>% 
  filter(spp_category == "winner") %>% select(species,spp_category) %>% unique()

#spp categories 
sppCategories <- rbind(losers,intermediates1L,intermediates2L,winners) %>% ungroup
saveRDS(sppCategories, spp_categories_file)


# #=============  CALCULATE starting LANDSCAPE OCCUPANCY THRU TIME UNCERTAINTY ===========
#calculate the error about the summing of landscape_occ thru to give occ_60yrs across bootstraps 

species <- processed_birds %>% select(species) %>%  unique() %>% pull()
all_start_landscape_scaled <- all_start_landscape %>%
  mutate(num_points = num_parcels*bird_CF) %>% select(-num_parcels) %>% 
  as.data.table()

unique_SL <- all_start_landscape_scaled %>% select(scenarioStart) %>% unique() %>% as.vector()
unique_spp <- processed_birds %>% select(species) %>%  unique() %>% as.vector()
combinations_SL <- as.data.table(expand.grid(scenarioStart = unique_SL$scenarioStart, species = unique_spp$species))


function_SL_60yr_uncertainty <- function(single_scenario_i, processed_birds_i) {
  
  bird_join <- processed_birds_i[single_scenario_i, on = .(habitat), allow.cartesian = TRUE]
  
  #calculate hab_occ
  result <- bird_join[, hab_occ := occ * num_points]
  
  #[Across habitat type transitions (e.g for ALL hab_parcel transitions) in a scenario, calculate occupancy for a given year]
  result <- bird_join[, .(landscape_occ = sum(hab_occ) / total_bird_pts), 
                      by = .(species, scenarioStart, iteration, functionalhabAge)]
  
 # Calculate occ_60yr for each iteration and species
  result <- result[, .(occ_60yr = sum(landscape_occ)), 
                   by = .(species, scenarioStart, iteration)]
}

execute_SL_fun <-function(zeta) {
  single_scenario_name <- combinations_SL$scenarioStart[zeta]
  processed_birds_name <- combinations_SL$species[zeta]
  
  # Print progress
  print(paste("Processing scenario:", single_scenario_name, "with species:", processed_birds_name, "(", zeta, "of", nrow(combinations_SL), ")"))
  
  
  single_scenario_i <- all_start_landscape_scaled[scenarioStart == single_scenario_name]
  processed_birds_i <- processed_birds[species == processed_birds_name]
  
  result <- function_SL_60yr_uncertainty(single_scenario_i, processed_birds_i)
  
  return(result)
}

#apply the starting landscape function for each starting landscape and species in combinations SL
#do this to calculate occ_60yr for each species, scenarioStart, and posterior draw iteration 
result_list_SL <- lapply(1:nrow(combinations_SL), execute_SL_fun) 

saveRDS(result_list_SL, sl_occ_file)

# ---- Calculate the SCENARIO OCCUPANCY THRU TIME UNCERTAINTY ------

# # Define a function for calculating 0-60yr occupancy per species and scenario for each posterior draw

##....... Faster data-table function ........................

function_scenario_60yr_uncertainty <- function(single_scenario_i, processed_birds_i) {
  # Step 1: Perform a left join and calculate hab_occ 
  # [join birds and scenarios and get num point for a given habitat type and stagger]
  
  
  bird_join <- merge(single_scenario_i, processed_birds_i, by.x = c("functional_habitat", "functionalhabAge"), 
                     by.y = c("habitat", "functionalhabAge"), all.x = TRUE, allow.cartesian = TRUE)
  bird_join[, hab_occ := occ * num_points]
  bird_join[, parcel_occ_stag := hab_occ / harvest_window]
  
  # Step 2: Group and summarize occupancy for specific year and habitat transition
  #[for each true year and habitat transition, calculate occupancy combined across the staggered
  # harvesting schedule (i.e. the occupancy in a given habitat transition for a given year)]
  
  result <- bird_join[, .(occ_hab_year = sum(parcel_occ_stag)), 
                      by = .(species, index, iteration, production_target, true_year, original_habitat, habitat)]
  
  # Step 3: Calculate landscape occupancy
  #[Across habitat type transitions (e.g for ALL hab_parcel transitions) in a scenario, calculate occupancy for a given year]
  result <- result[, .(landscape_occ = sum(occ_hab_year) / total_bird_pts), 
                   by = .(species, index, iteration, production_target, true_year)]
  
  # Step 4: Calculate occ_60yr for each iteration and species
  #[calculate occ60 for each iteration and species]
  result <- result[, .(occ_60yr = sum(landscape_occ)), 
                      by = .(species, index, iteration, production_target)]
                   
  return(result)
}


#-----read in scenario group and define harvest delay ------

#DEFINE DELAY FILTER ####
#(we  subset only a few delay schedules to improve computational efficiency)
#delay filter (availalbe 0-29 in 1 year increments basically allow each scenario to be delayed in its first conversion by the delay filter)
delayFilters <- c("delay 0", "delay 29")
harvest_window <-  length(delayFilters)##how many harvest delays?

#set a folder for saving outputs, showing for each species and scenario and iteration, occ_60 for lanscape
rds_folder <- scenario_occ_dir

for (k in seq_along(csv_files)){
  
  #read in single scenario type and add total points   
  scenario_group  <- read.csv(csv_files[[k]]) %>% as.data.table() %>% 
    mutate(num_points = num_parcels*bird_CF) %>%  
    filter(harvest_delay %in% delayFilters)  # only filter subset of delay to enhance efficieny 
  
  # Generate a unique rds file name
  rds_file_name <- sub(".csv$", "", unique(scenario_group$scenarioName))
  rds_file_name <- paste(unique(rds_file_name), "occ60.rds", sep = "_")
  
  # Combine folder path and file name to create full file path
  rds_file_path <- file.path(rds_folder, rds_file_name)   
  
  #=====
  # # Choose a smaller number of scenario indices and species for testing
  #.............................................................
  # # For example, let's use the first 5 indices and first 3 species
  # selected_indices <- scenario_group %>% select(index) %>% unique %>%  slice(1:30) %>%  pull()
  # selected_species <- processed_birds %>% select(species) %>% unique %>%  slice(1:5) %>%  pull()
  # # Create subsets of scenario_group and processed_birds
  # subset_scenario_group <- scenario_group %>% filter(index %in% selected_indices)
  # subset_processed_birds <- processed_birds %>% filter(species %in% selected_species)
  # 
  # single_scenario_i <- as.data.table(subset_scenario_group)
  # processed_birds_i <- as.data.table(subset_processed_birds)
  # names(single_scenario2)
  # names(processed_birds2)
  #=====
  # 
  
  #----apply the function using parrellised approach  -----
  
  #calculate full set of combinatations of species and scenario
  #get unique scenarios asn species 
  unique_index <- scenario_group %>% select(index) %>% unique() %>% as.vector()
  unique_spp <- processed_birds %>% select(species) %>%  unique() %>% as.vector()
  combinations <- as.data.table(expand.grid(index = unique_index$index, species = unique_spp$species))
  
  # Pre-allocate a list for results
  result_list <- vector("list", nrow(combinations))
  
  
  # Register a parallel backend
  cl <- makeCluster(detectCores())
  cl <- makeCluster(8)
  registerDoParallel(cl)
  clusterEvalQ(cl, c(library(tidyr),library(data.table),library(dplyr),library(purrr),library(profvis)))
  clusterExport(cl,c("combinations","scenario_group","processed_birds","function_scenario_60yr_uncertainty","harvest_window","total_bird_pts"))
  
  
  # Use lapply to apply the function to each combination
  #function works by taking the first row (defined by zeta) of combinations, filtering that given species and scenario, and then applying the function_scenario_60yr_uncertainty to these  
  
  execute_uncertainty_fun <-function(zeta) {
    single_scenario_name <- combinations$index[zeta]
    processed_birds_name <- combinations$species[zeta]
    
    single_scenario_i <- scenario_group[index == single_scenario_name]
    processed_birds_i <- processed_birds[species == processed_birds_name]
    
    result <- function_scenario_60yr_uncertainty(single_scenario_i, processed_birds_i)
    
    
    # Print information for each iteration
    cat("Iteration:", zeta, "- Single Scenario:", single_scenario_name, "- Processed Birds:", processed_birds_name, "\n")
    return(result)
  }
  
  
  timing <- system.time({
    # result_list <- parLapplyLB(cl, 1:1000, execute_uncertainty_fun)
    result_list <- parLapplyLB(cl, 1:nrow(combinations), execute_uncertainty_fun)
  })
  
  cat("Elapsed time: ", timing[3], " seconds\n")
  # Clean up parallel backend
  stopCluster(cl)
  
  
  #save the output to an rds folder 
  saveRDS(result_list, file = rds_file_path)
  
  #saveRDS(result_list, "scenario60yrUncertainty.rds")
  
  rm(result_list)
  
  # Print the execution time
  cat("Elapsed time: ", timing[3], " seconds\n")
}


result_list


#-----------------calculate rel occ for each  iteration and species category ----
cap <- 1.5 # don't allow scenario occ to be more than 1.5 starting landscape occ [only used if calculating geometric mean]
sppCategories <- readRDS(spp_categories_file)
sppCategories<- as.data.table(sppCategories)

#where data on occ_60yr of scenarios is stored 
rds_folder <- scenario_occ_dir
occ60_files <- list.files(rds_folder, pattern = "*.rds", full.names = TRUE)

#occ60_files <- occ60_files[2:3]

#SL_60yrOcc 
if (!file.exists(sl_occ_file)) {
  stop("Missing required SL file: Outputs/NR2/rds/SLoccOutputs/SL_occ60yr_perIteration.rds")
}
SL_occ60 <- readRDS(sl_occ_file)
SL_occ60_dt <- rbindlist(SL_occ60) %>%
  rename(SL_occ_60yr = occ_60yr)

#EXTRACT ONLY THE BASELINE ALL_PRIMARY SL
SL_all_primary_dt<- SL_occ60_dt %>% filter(scenarioStart == "all_primary") 
rm(SL_occ60_dt)

#allocate folder to hold raw relative occupancy values, for further apraisal 
raw_rel_occ_folder <- rel_occ_dir

for (w in seq_along(occ60_files)){
  occ60 <- readRDS(occ60_files[[w]])
  occ60_dt <- rbindlist(occ60)
  
  # Generate a unique  file name
  rds_file_name <- paste("OGbaseline_", basename(occ60_files[[w]]), sep = "")
  #relOcc_file_name <- paste("relOcc_", basename(occ60_files[[w]]), sep = "")
  
  
  # Combine folder path and file name to create full file path
  occ_file_path <- file.path(raw_rel_occ_folder, rds_file_name)   

  #add starting landscape to scenarios 
  scenarioStart <- occ60_dt %>% select(index, production_target) %>%
    unique() %>% left_join(scenario_composition, by = c("index", "production_target")) %>%  
    select(scenarioStart) %>% unique() %>% drop_na()
  occ60_dt[, scenarioStart := scenarioStart]
  
  #NB1: this conveys each species starting landscape occupancy
  #join SL and scenarios for each iteration 
  #occ_comb <- occ60_dt[SL_occ60_dt, on = .(species, scenarioStart, iteration), nomatch = 0]
  #NB2: this instead denotes each species old-growth baseline ouccupancy 
  occ_comb <- occ60_dt[SL_all_primary_dt, on = .(species, iteration), nomatch = 0]
  
  
  # #calculate rel_occ; if rel_occ is > cap, replace with cap, to ensure scenario landscape cannot be more than 1.5 of starting landscape
    occ_comb[, rel_occ := pmin((occ_60yr / SL_occ_60yr), cap)]

  # -------------------------------------------------------------------------- #
  # NR2 — DELTA_ABUNDANCE (same RDS as rel_occ; additive, not a ratio)         #
  # -------------------------------------------------------------------------- #
  # Per draw: delta_abundance = occ_60yr − SL_occ_60yr (all-primary baseline,
  # same pairing as denominator for rel_occ). Summarised by species × index ×
  # production_target with median/mean and central posterior intervals: 95%
  # (q025–q975) and 80% (q10–q90). Written with relative occupancy columns in
  # one table: OGbaseline_<...>.rds under Rel_Occ_PerIteration.
  # -------------------------------------------------------------------------- #
  occ_comb[, delta_abundance := occ_60yr - SL_occ_60yr]

  #export raw relative occupancy values 
  #saveRDS(occ_comb, file = "relOcc_file_path")
  
  #add in species categories 
  occ_comb <- sppCategories[occ_comb, on = "species"]

  # Species-level summary: relative occupancy (ratio) and delta abundance (additive), same groups
  rel_occ <- occ_comb %>%
    group_by(species, index, production_target) %>%
    summarize(
      medianRelativeOccupancy = median(rel_occ, na.rm = TRUE),
      meanRelativeOccupancy = mean(rel_occ, na.rm = TRUE),
      p1_medianRelativeOccupancy = quantile(rel_occ, 0.1, na.rm = TRUE),
      p9_medianRelativeOccupancy = quantile(rel_occ, 0.9, na.rm = TRUE),
      median_delta_abundance = median(delta_abundance, na.rm = TRUE),
      mean_delta_abundance = mean(delta_abundance, na.rm = TRUE),
      q025_delta_abundance = quantile(delta_abundance, 0.025, na.rm = TRUE),
      q975_delta_abundance = quantile(delta_abundance, 0.975, na.rm = TRUE),
      q10_delta_abundance = quantile(delta_abundance, 0.10, na.rm = TRUE),
      q90_delta_abundance = quantile(delta_abundance, 0.90, na.rm = TRUE),
      .groups = "drop"
    )
  
  # #summarise posterior draws directly over the sp grp (ie over losers, winners etc.)
  # rel_occ_grp <- occ_comb[, .(
  #   medianRelativeOccupancy = median(rel_occ, na.rm = TRUE),
  #   p1_medianRelativeOccupancy = quantile(rel_occ, 0.1, na.rm = TRUE),
  #   p9_medianRelativeOccupancy = quantile(rel_occ, 0.9, na.rm = TRUE)
  # ), by = .(spp_category, index, production_target)]
  # 
  # 

  #save the output to an rds folder 
  saveRDS(rel_occ, file = occ_file_path)
}


##################################################
#FOR UNCERTAINTY -calculate proportion of scenarios where logging is better than plantations
#set folder for storing best scenario (logging or plantation) for each production target
best_scenario_folder <- best_scenario_dir

#how often are plantation scenarios better than logging scenarios 
logging_or_plantation_scenarios <- scenario_composition %>%  
  group_by(index, production_target) %>%  
  # Add information on proportion of plantation
  mutate(propPlant = sum(num_parcels[habitat %in% c("eucalyptus_current", "albizia_current", "albizia_future", "eucalyptus_future")]) / 1000) %>%  
  select(index, production_target, propPlant, scenarioStart) %>% 
  mutate(treatment_strategy = case_when(
    propPlant > 0 ~ "plantation",
    propPlant == 0 ~ "logging"
  )) %>%    select(-propPlant) %>%  unique() %>%  
  as.data.table()

for (w in seq_along(occ60_files)){
  occ60 <- readRDS(occ60_files[[w]])
  occ60_dt <- rbindlist(occ60)
    rds_file_name <- paste("BestScenario_", basename(occ60_files[[w]]), sep = "")
    best_scenario_file_path <- file.path(best_scenario_folder, rds_file_name)   

  #add starting landscape to scenarios 
  scenarioStart <- occ60_dt %>% select(index, production_target) %>%
    unique() %>% left_join(scenario_composition, by = c("index", "production_target")) %>%  
    select(scenarioStart) %>% unique() %>% drop_na()
  occ60_dt[, scenarioStart := scenarioStart]
  
  #NB this conveys each species starting landscape occupancy
  occ_comb <- occ60_dt[SL_all_primary_dt, on = .(species, iteration), nomatch = 0]
  
  #calculate rel_occ
  occ_comb <- occ_comb[, rel_occ := occ_60yr / SL_occ_60yr]
  
  #add in scenario composition to highlight logging vs plantation scenarios
  occ_comb <- occ_comb %>% left_join(logging_or_plantation_scenarios)
  
  #for each production target and species, find the proportion of iterations where the best scenario 
  #(ie with the highest relOcc) is plantation-dominated. 
  #Do this for PAIRED scenario draws (ie iterations of the model)
  best_scenario <- occ_comb %>%  group_by(species, production_target, iteration) %>% 
    filter(rel_occ == max(rel_occ)) %>%  
    select(species, iteration, production_target, treatment_strategy, rel_occ) %>%  
    rename(max_rel_occ = rel_occ) %>% unique()
  
  #save the output to an rds folder 
  saveRDS(best_scenario, file = best_scenario_file_path)
}

#This would be an example of paired scenario draws (ie based on the same model params)
PairedExample <- occ_comb %>% filter(species == "Helmeted Hornbill" & iteration == 'draw_459' & production_target == 1)


#----- summarise relative abundance across groups of species for which have median relative occupancy  -----

#summarised across data that has already been medianed per spp from 500 draws
rel_occ_files <- list.files(raw_rel_occ_folder, pattern = "\\.rds$", full.names = TRUE)
if (length(rel_occ_files) == 0) {
  stop("No rel_occ files found in Outputs/NR2/rds/Rel_Occ_PerIteration; cannot build rel_occ_df")
}

rel_occ_df <- rel_occ_files %>%
  lapply(readRDS) %>%
  bind_rows() %>%
  as.data.table()

required_rel_occ_cols <- c("species", "index", "production_target", "medianRelativeOccupancy")
missing_rel_occ_cols <- setdiff(required_rel_occ_cols, names(rel_occ_df))
if (length(missing_rel_occ_cols) > 0) {
  stop(
    "rel_occ_df is missing required columns: ",
    paste(missing_rel_occ_cols, collapse = ", ")
  )
}


#for species groupings (winner, loser, intermediate)
summarise_across_posterior_fun <- function(x){
  x %>% left_join(sppCategories, by = "species") %>%
    group_by(spp_category, index, production_target) %>%  
    summarise(medianRelativeOccupancy = median(medianRelativeOccupancy),
              p1_medianRelativeOccupancy = quantile(medianRelativeOccupancy, 0.1),
              p9_medianRelativeOccupancy = quantile(medianRelativeOccupancy, 0.9) 
              # IQR_medianRelativeOccupancy = IQR(medianRelativeOccupancy), 
              # geometric_mean = exp(mean(log(medianRelativeOccupancy), na.rm = TRUE)),
              # p1_geometric_mean = exp(quantile(log(medianRelativeOccupancy), 0.1, na.rm = TRUE)),
              # p9_geometric_mean = exp(quantile(log(medianRelativeOccupancy), 0.9, na.rm = TRUE))
    )
   
  
}
#---output of summarised statistics across posterior draws ----
final_relOcc <- summarise_across_posterior_fun(rel_occ_df)

#add back in key information 

final_relOcc <- final_relOcc %>% ungroup() %>%   left_join(scenario_composition, by = c("index", "production_target"))# %>% 

#for IUCN near threatened species 
summarise_IUCN_across_posterior_fun <- function(x){
  x %>% left_join(IUCN_classification, by = "species") %>%
    group_by(threatened, index, production_target) %>%  
    summarise(medianRelativeOccupancy = median(medianRelativeOccupancy),
              p1_medianRelativeOccupancy = quantile(medianRelativeOccupancy, 0.1),
              p9_medianRelativeOccupancy = quantile(medianRelativeOccupancy, 0.9) 
              # IQR_medianRelativeOccupancy = IQR(medianRelativeOccupancy),
              # geometric_mean = exp(mean(log(medianRelativeOccupancy), na.rm = TRUE)),
              # p1_geometric_mean = exp(quantile(log(medianRelativeOccupancy), 0.1, na.rm = TRUE)),
              # p9_geometric_mean = exp(quantile(log(medianRelativeOccupancy), 0.9, na.rm = TRUE))
    )
  
}

final_IUCN <- summarise_IUCN_across_posterior_fun(rel_occ_df)

#add back in key information 
final_IUCN <- final_IUCN %>%  left_join(scenario_composition, by = c("index", "production_target"))# %>% 


#-----EXPORT OUTCOME PERFORMANCE for consolidated figure of all outcomes -----
getwd()
#output of grouping by winner, loser, int
output <- final_relOcc %>%
select(index, production_target, scenarioName,scenarioStart,
                                 medianRelativeOccupancy,p1_medianRelativeOccupancy, p9_medianRelativeOccupancy,
       #IQR_medianRelativeOccupancy,       geometric_mean, p1_geometric_mean,  p9_geometric_mean,
                                 spp_category) %>% cbind(outcome = "birds")

#output of grouping by IUCN threatened or not
outputIUCN <- final_IUCN %>% select(index, production_target, scenarioName,scenarioStart,
                                  #  IQR_medianRelativeOccupancy,  geometric_mean, p1_geometric_mean,  p9_geometric_mean,
                                    medianRelativeOccupancy,p1_medianRelativeOccupancy, p9_medianRelativeOccupancy,
                                    threatened) %>% cbind(outcome = "birds")

#outputs when using fully primary baseline and when calculating median across species groupings for main figures
saveRDS(output, final_output_file)
saveRDS(outputIUCN, final_iucn_output_file)

#output when instead calculating median and CI relative abundance on a per-species basis; this
#reliably shows error on a per-species basis.

saveRDS(rel_occ_df, species_level_output_file)


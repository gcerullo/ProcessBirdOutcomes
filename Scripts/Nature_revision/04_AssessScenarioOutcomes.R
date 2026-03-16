#GC 11/06/24
#Assess the bird outcomes of different scenarios, where each scenario is disaggregated by age


#This code:
#1. Uses model outputs from Bayesian spp occ to summarise spp categories 
#2. To propagate through bird outcomes for each spp.

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
birds <- readRDS("Inputs/occ500drawsSept24.rds")

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

#FUNCTIONS

#function for making sure we can visualise the order of scenarios correctly
scenario_row_order_visualisation_fun <- function(x){
  x %>%  
    group_by(index, production_target, habitat, original_habitat, harvest_delay) %>%  
    arrange(true_year, .by_group = TRUE) %>%
    ungroup()
}

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

#______________________
#ASSUME NO PLATEUpast 40 years (only a plateu pre 19 year) 
#______________________

process_birds_data_linear_19only <- function(x) {
  
  # Define full age range
  ages <- data.frame(functionalhabAge = seq(0, 60))
  missing1L_R <- data.frame(functionalhabAge = seq(0, 18))
  
  # --------------------------------------------------------------
  # Step 1: Expand primary & twice-logged across ages 0–60
  # --------------------------------------------------------------
  P_2L <- x %>%
    filter(habitat %in% c("primary", "twice-logged")) %>%
    select(-functionalhabAge) %>%
    group_by(species, habitat, iteration) %>%
    crossing(ages)
  
  birds <- x %>%
    filter(!(habitat %in% c("primary", "twice-logged"))) %>%
    bind_rows(P_2L)
  
  # --------------------------------------------------------------
  # Step 2: Plateau *only* early years (<19) for 1L and restored
  # --------------------------------------------------------------
  birds_1L_19 <- birds %>%
    filter(habitat == "once-logged", functionalhabAge == 19) %>%
    select(-functionalhabAge) %>%
    group_by(species, habitat, iteration) %>%
    crossing(missing1L_R)
  
  birds_R_19 <- birds %>%
    filter(habitat == "restored", functionalhabAge == 19) %>%
    select(-functionalhabAge) %>%
    group_by(species, habitat, iteration) %>%
    crossing(missing1L_R)
  
  # Remove model values <19 and add plateau
  birds <- birds %>%
    filter(!(habitat %in% c("once-logged", "restored") &
               functionalhabAge < 19)) %>%
    bind_rows(birds_1L_19, birds_R_19)
  
  # --------------------------------------------------------------
  # Step 3: KEEP model-derived restored 41–60
  # --------------------------------------------------------------
  # ❌ No plateau applied beyond 40
  # (so nothing to remove or replace here)
  
  # --------------------------------------------------------------
  # Step 4: Remove eucalyptus >6 years
  # --------------------------------------------------------------
  birds <- birds %>%
    filter(!(habitat %in% c("eucalyptus_current", "eucalyptus_improved") &
               functionalhabAge > 6))
  
  # --------------------------------------------------------------
  # Step 5: Interpolate deforested condition from early plantation years
  # --------------------------------------------------------------
  birds_deforested <- birds %>%
    filter(habitat %in% c("eucalyptus_current", "albizia_current"),
           functionalhabAge %in% c(0, 1, 2)) %>%
    select(-functionalhabAge, -habitat) %>%
    group_by(species, iteration) %>%
    mutate(occ = mean(occ)) %>%
    crossing(ages) %>%
    mutate(habitat = "deforested")
  
  # Bind and finish
  birds <- birds %>%
    bind_rows(birds_deforested) %>%
    filter(functionalhabAge <= 60)
  
  return(birds)
}

# Call the function to process  'birds' data frame with no plateus
processed_birds_no_plateua <- process_birds_data_linear_19only(birds_raw)
x <- processed_birds_no_plateua %>% filter(iteration == "draw_1")
processed_birds_no_plateua <- as.data.table(processed_birds_no_plateua)



#---- save processed birds ----
saveRDS(processed_birds, "Outputs/processedOccBirdsSept24.rds")
saveRDS(processed_birds_no_plateua, "Outputs/processedOccBirdsno40yrplateu.rds")


#----can start HERE ----
processed_birds <- readRDS("Outputs/processedOccBirdsSept24.rds")
processed_birds_no_plateua <- readRDS("Outputs/processedOccBirdsno40yrplateu.rds")


###########
#############
###########
##########

#====================================================================
#==================   MASTER PIPELINE FUNCTION   ====================
#====================================================================

run_full_pipeline <- function(processed_birds_input, label) {
  
  #message("Starting pipeline for: ", label)
  
  # Create output folders ------------------------------------------------------
  dir.create(file.path("Outputs", label), showWarnings = FALSE)
  dir.create(file.path("Outputs", label, "SLoccOutputs"), showWarnings = FALSE)
  dir.create(file.path("Outputs", label, "occ60PerScenarioIteration"), showWarnings = FALSE)
  dir.create(file.path("Outputs", label, "Rel_Occ_PerIteration"), showWarnings = FALSE)
  
  # Assign local paths for this run
  sl_output_folder   <- file.path("Outputs", label, "SLoccOutputs")
  scenario_folder    <- file.path("Outputs", label, "occ60PerScenarioIteration")
  rel_occ_folder     <- file.path("Outputs", label, "Rel_Occ_PerIteration")
  
  #===========================================================================
  #=======================   SECTION 1: PREP BIRDS   ==========================
  #===========================================================================
  
  processed_birds <- as.data.table(processed_birds_input)
  
  #remove improved improved yields
  processed_birds <- processed_birds[!grepl("improved", habitat, ignore.case = TRUE)]
  
  #----summarise across posterior draws (for calculating spp categories only) ----
  birds <- processed_birds[, .(
    occ = mean(occ),
    occ_lwr = quantile(occ, 0.2),
    occ_upr = quantile(occ, 0.8)
  ),
  by = .(species, habitat, functionalhabAge, dependency)]
  
  #-----calculate species categories ----- 
  losers <- birds %>% 
    filter(functionalhabAge < 30) %>%  
    group_by(species)  %>%  
    filter(occ == max(occ)) %>% 
    mutate(spp_category = case_when(habitat == "primary" ~ "loser", TRUE ~ NA_character_)) %>% 
    filter(spp_category == "loser") %>% select(species, spp_category) %>% unique()
  
  intermediates1L <- birds %>%
    filter(functionalhabAge < 30) %>%  
    group_by(species) %>% filter(occ == max(occ)) %>% 
    mutate(spp_category = case_when(
      habitat == "once-logged" ~ "intermediate1L",
      habitat == "restored" ~ "intermediate1L",
      TRUE ~ NA_character_
    )) %>% 
    filter(spp_category == "intermediate1L") %>% select(species, spp_category) %>% unique()
  
  intermediates2L <- birds %>%
    filter(functionalhabAge < 30) %>%  
    group_by(species) %>% filter(occ == max(occ)) %>% 
    mutate(spp_category = case_when(
      habitat == "twice-logged" ~ "intermediate2L",
      TRUE ~ NA_character_
    )) %>% 
    filter(spp_category == "intermediate2L") %>% select(species, spp_category) %>% unique()
  
  winners <- birds %>%
    filter(functionalhabAge < 30) %>%  
    group_by(species) %>%
    filter(occ == max(occ)) %>%
    mutate(spp_category = case_when(
      habitat == "albizia_current" ~ "winner",
      habitat == "eucalyptus_current" ~ "winner",
      TRUE ~ NA_character_
    )) %>% 
    filter(spp_category == "winner") %>% select(species, spp_category) %>% unique()
  
  sppCategories <- rbind(losers, intermediates1L, intermediates2L, winners) %>% ungroup()
  saveRDS(sppCategories, file.path("Outputs", label, "sppCategories.rds"))
  
  #===========================================================================
  #================ SECTION 2: STARTING LANDSCAPE OCC THROUGH TIME ===========
  #===========================================================================
  
  species_list <- processed_birds %>% select(species) %>% unique() %>% pull()
  
  all_start_landscape_scaled <- all_start_landscape %>%
    mutate(num_points = num_parcels * bird_CF) %>% 
    select(-num_parcels) %>% 
    as.data.table()
  
  unique_SL <- all_start_landscape_scaled %>%
    select(scenarioStart) %>% unique() %>% as.vector()
  
  unique_spp <- processed_birds %>% select(species) %>% unique() %>% as.vector()
  
  combinations_SL <- as.data.table(expand.grid(
    scenarioStart = unique_SL$scenarioStart,
    species = unique_spp$species
  ))
  
  # function for SL
  function_SL_60yr_uncertainty <- function(single_scenario_i, processed_birds_i) {
    
    bird_join <- processed_birds_i[single_scenario_i, on = .(habitat), allow.cartesian = TRUE]
    
    result <- bird_join[, hab_occ := occ * num_points]
    
    result <- bird_join[, .(landscape_occ = sum(hab_occ) / total_bird_pts),
                        by = .(species, scenarioStart, iteration, functionalhabAge)]
    
    result <- result[, .(occ_60yr = sum(landscape_occ)),
                     by = .(species, scenarioStart, iteration)]
  }
  
  # wrapper
  execute_SL_fun <- function(zeta) {
    single_scenario_name <- combinations_SL$scenarioStart[zeta]
    processed_birds_name <- combinations_SL$species[zeta]
    
   # message("Processing SL: ", zeta, "/", nrow(combinations_SL))
    
    single_scenario_i <- all_start_landscape_scaled[scenarioStart == single_scenario_name]
    processed_birds_i <- processed_birds[species == processed_birds_name]
    
    function_SL_60yr_uncertainty(single_scenario_i, processed_birds_i)
  }
  
  result_list_SL <- lapply(1:nrow(combinations_SL), execute_SL_fun)
  
  saveRDS(result_list_SL, file.path(sl_output_folder, "SL_occ60yr_perIteration.rds"))
  
  #===========================================================================
  #=======================  SECTION 3: SCENARIO UNCERTAINTY ==================
  #===========================================================================

  
  function_scenario_60yr_uncertainty <- function(single_scenario_i, processed_birds_i, harvest_window, total_bird_pts) {
    
    # join data
    bird_join <- merge(
      single_scenario_i, processed_birds_i,
      by.x = c("functional_habitat", "functionalhabAge"),
      by.y = c("habitat", "functionalhabAge"),
      all.x = TRUE, allow.cartesian = TRUE
    )
    
    # per-parcel yearly occupancy
    bird_join[, hab_occ := occ * num_points]
    bird_join[, parcel_occ_stag := hab_occ / harvest_window]
    
    # step 1: sum within parcel, species, year
    result <- bird_join[, .(occ_hab_year = sum(parcel_occ_stag)),
                        by = .(species, index, iteration, production_target, true_year, original_habitat, habitat)]
    
    # step 2: scale to landscape
    result <- result[, .(landscape_occ = sum(occ_hab_year) / total_bird_pts),
                     by = .(species, index, iteration, production_target, true_year)]
    
    # step 3: sum over 60 years
    result <- result[, .(occ_60yr = sum(landscape_occ)),
                     by = .(species, index, iteration, production_target)]
    
    return(result)
  }
  
  
  #========================== RUN SCENARIOS ==================================
  
  for (k in seq_along(csv_files)) {
    
    #-----------------------
    # load scenario group
    #-----------------------
    scenario_group <- read.csv(csv_files[[k]]) %>%
      as.data.table() %>%
      mutate(num_points = num_parcels * bird_CF) %>%
      filter(harvest_delay %in% c("delay 0", "delay 29"))
    
    harvest_window <- 2
    rds_file_name <- paste0(unique(sub(".csv$", "", scenario_group$scenarioName)), "_occ60.rds")
    rds_file_path <- file.path(scenario_folder, rds_file_name)
    
    # FULL cross combination: each index × each species
    combinations <- as.data.table(expand.grid(
      index   = unique(scenario_group$index),
      species = unique(processed_birds$species)
    ))
    
    # Split into list so each worker gets a single row cleanly
    combo_list <- split(combinations, seq_len(nrow(combinations)))
    
    #----------------------
    # SETUP PARALLEL
    #----------------------
    cl <- makeCluster(12)
    on.exit(stopCluster(cl), add = TRUE)
    registerDoParallel(cl)
    
    clusterEvalQ(cl, {
      library(data.table)
      library(dplyr)
    })
    
    clusterExport(cl, varlist = c("scenario_group", "processed_birds",
                                  "function_scenario_60yr_uncertainty",
                                  "harvest_window", "total_bird_pts"))
    
    #----------------------
    # WORKER FUNCTION
    #----------------------
    execute_uncertainty_fun <- function(combo_row) {
      
      idx   <- combo_row$index
      spp   <- combo_row$species
      
      single_scenario_i <- scenario_group[index == idx]
      processed_birds_i <- processed_birds[species == spp]
      
      function_scenario_60yr_uncertainty(
        single_scenario_i, processed_birds_i,
        harvest_window, total_bird_pts
      )
    }
    
    #----------------------
    # RUN PARALLEL
    #----------------------
    timing <- system.time({
      result_list <- parLapplyLB(cl, combo_list, execute_uncertainty_fun)
    })
    
    stopCluster(cl)
    
    saveRDS(result_list, rds_file_path)
  }
  #===========================================================================
  #====================  SECTION 4: RELATIVE OCCUPANCY =======================
  #===========================================================================
  
  cap <- 1.5
  sppCategories <- as.data.table(sppCategories)
  
  occ60_files <- list.files(scenario_folder, pattern = "*.rds", full.names = TRUE)
  
  SL_occ60 <- result_list_SL
  SL_occ60_dt <- rbindlist(SL_occ60) %>% rename(SL_occ_60yr = occ_60yr)
  
  SL_all_primary_dt <- SL_occ60_dt %>% filter(scenarioStart == "all_primary")
  
  for (w in seq_along(occ60_files)) {
    
    occ60 <- readRDS(occ60_files[[w]])
    occ60_dt <- rbindlist(occ60)
    
    rds_file_name <- paste("OGbaseline_", basename(occ60_files[[w]]), sep = "")
    occ_file_path <- file.path(rel_occ_folder, rds_file_name)
    
    scenarioStart <- occ60_dt %>% 
      select(index, production_target) %>% 
      unique() %>% 
      left_join(scenario_composition, by = c("index", "production_target")) %>%  
      select(scenarioStart) %>% unique() %>% drop_na()
    
    occ60_dt[, scenarioStart := scenarioStart]
    
    occ_comb <- occ60_dt[SL_all_primary_dt, on = .(species, iteration), nomatch = 0]
    
    occ_comb[, rel_occ := pmin((occ_60yr / SL_occ_60yr), cap)]
    
    occ_comb <- sppCategories[occ_comb, on = "species"]
    
    rel_occ <- occ_comb %>%
      group_by(species, index, production_target) %>%
      summarize(
        medianRelativeOccupancy = median(rel_occ, na.rm = TRUE),
        meanRelativeOccupancy = mean(rel_occ, na.rm = TRUE),
        p1_medianRelativeOccupancy = quantile(rel_occ, 0.1, na.rm = TRUE),
        p9_medianRelativeOccupancy = quantile(rel_occ, 0.9, na.rm = TRUE)
      )
    
    saveRDS(rel_occ, file = occ_file_path)
  }
  
 # message("Pipeline complete for: ", label)
}

#====================================================================
#=========================    RUN EXAMPLES   ========================
#====================================================================

processed_birds <- readRDS("Outputs/processedOccBirdsSept24.rds")
processed_birds_no_plateua <- readRDS("Outputs/processedOccBirdsno40yrplateu.rds")

run_full_pipeline(processed_birds_input = processed_birds, 
                  label = "withPlateaus")

run_full_pipeline(processed_birds_no_plateua, 
                  label = "NoPlateau")


###########
#############
###########
##########
























































#========================================================

# Function to run bird occupancy workflow for any processed_birds input

#========================================================
run_bird_workflow <- function(processed_birds_input, label) {
  
  #remove improved improved yields
  processed_birds <- as.data.table(processed_birds_input)
  
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
  saveRDS(sppCategories,paste0("Outputs/sppCategories_", label, ".rds"))
  
  #================ CALCULATE starting LANDSCAPE OCCUPANCY THRU TIME UNCERTAINTY ===========
  
  species <- processed_birds %>% select(species) %>%  unique() %>% pull()
  all_start_landscape_scaled <- all_start_landscape %>%
    mutate(num_points = num_parcels*bird_CF) %>% select(-num_parcels) %>%
    as.data.table()
  
  unique_SL <- all_start_landscape_scaled %>% select(scenarioStart) %>% unique() %>% as.vector()
  unique_spp <- processed_birds %>% select(species) %>%  unique() %>% as.vector()
  combinations_SL <- as.data.table(expand.grid(scenarioStart = unique_SL$scenarioStart, species = unique_spp$species))
  
  #----function for 0-60yr occupancy----
  function_SL_60yr_uncertainty <- function(single_scenario_i, processed_birds_i) {
    bird_join <- processed_birds_i[single_scenario_i, on = .(habitat), allow.cartesian = TRUE]
    result <- bird_join[, hab_occ := occ * num_points]
    result <- bird_join[, .(landscape_occ = sum(hab_occ) / total_bird_pts),
                        by = .(species, scenarioStart, iteration, functionalhabAge)]
    result <- result[, .(occ_60yr = sum(landscape_occ)),
                     by = .(species, scenarioStart, iteration)]
  }
  
  execute_SL_fun <-function(zeta) {
    single_scenario_name <- combinations_SL$scenarioStart[zeta]
    processed_birds_name <- combinations_SL$species[zeta]
   # print(paste("Processing scenario:", single_scenario_name, "with species:", processed_birds_name, "(", zeta, "of", nrow(combinations_SL), ")"))
    single_scenario_i <- all_start_landscape_scaled[scenarioStart == single_scenario_name]
    processed_birds_i <- processed_birds[species == processed_birds_name]
    result <- function_SL_60yr_uncertainty(single_scenario_i, processed_birds_i)
    return(result)
  }
  
  result_list_SL <- lapply(1:nrow(combinations_SL), execute_SL_fun)
  saveRDS(result_list_SL,paste0("Outputs/SLoccOutputs/SL_occ60yr_perIteration_", label, ".rds"))
  
  #----scenario occupancy thru time uncertainty----
  #same as in your code...
  
  # all remaining code from scenario 0-60yr calculation can remain identical
  
  # save results using paste0("Outputs/occ60PerScenarioIterationJan25/...", label, ".rds")
  
  #----calculate relative occupancy----
 # cap <- 1.5 #don't allow scenario occ to be more than 1.5 starting landscape occ
  sppCategories <- readRDS(paste0("Outputs/sppCategories_", label, ".rds"))
  sppCategories<- as.data.table(sppCategories)
  
  rds_folder <- "Outputs/occ60PerScenarioIterationJan25"
  occ60_files <- list.files(rds_folder, pattern = "*.rds", full.names = TRUE)
  SL_occ60 <- readRDS(paste0("Outputs/SLoccOutputs/SL_occ60yr_perIteration_", label, ".rds"))
  SL_occ60_dt <- rbindlist(SL_occ60) %>%
    rename(SL_occ_60yr = occ_60yr)
  SL_all_primary_dt<- SL_occ60_dt %>% filter(scenarioStart == "all_primary")
  rm(SL_occ60_dt)
  
  raw_rel_occ_folder <- "Outputs/Rel_Occ_PerIterationSept24"
  
  for (w in seq_along(occ60_files)){
    occ60 <- readRDS(occ60_files[[w]])
    occ60_dt <- rbindlist(occ60)
    rds_file_name <- paste("OGbaseline_", basename(occ60_files[[w]]), sep = "")
    occ_file_path <- file.path(raw_rel_occ_folder, rds_file_name)
    occ_comb <- occ60_dt[SL_all_primary_dt, on = .(species, iteration), nomatch = 0]
    occ_comb[, rel_occ := pmin((occ_60yr / SL_occ_60yr), cap)]
    occ_comb <- sppCategories[occ_comb, on = "species"]
    rel_occ <- occ_comb %>%  group_by(species, index, production_target) %>%
      summarize(
        medianRelativeOccupancy = median(rel_occ, na.rm = TRUE),
        meanRelativeOccupancy = mean(rel_occ, na.rm = TRUE),
        p1_medianRelativeOccupancy = quantile(rel_occ, 0.1, na.rm = TRUE),
        p9_medianRelativeOccupancy = quantile(rel_occ, 0.9, na.rm = TRUE)
      )
    saveRDS(rel_occ, file = occ_file_path)
  }
  
}

#========================================================

# Run workflow for both plateau and linear inputs

#========================================================
run_bird_workflow(processed_birds, "plateau")
run_bird_workflow(processed_birds_no_plateua, "no_plateau_yr40")

processed_birds <- readRDS()
processed_birds_no_plateua <- readRDS("Outputs/processedOccBirdsno40yrplateu.rds")
#-----read in scenario group and define harvest delay ------

#DEFINE DELAY FILTER ####
#(we  subset only a few delay schedules to improve computational efficiency)
#delay filter (availalbe 0-29 in 1 year increments basically allow each scenario to be delayed in its first conversion by the delay filter)
delayFilters <- c("delay 0", "delay 29")
harvest_window <-  length(delayFilters)##how many harvest delays?

#set a folder for saving outputs, showing for each species and scenario and iteration, occ_60 for lanscape
rds_folder <- "Outputs/occ60PerScenarioIterationJan25"

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
    #cat("Iteration:", zeta, "- Single Scenario:", single_scenario_name, "- Processed Birds:", processed_birds_name, "\n")
    return(result)
  }
  
  
  timing <- system.time({
    # result_list <- parLapplyLB(cl, 1:1000, execute_uncertainty_fun)
    result_list <- parLapplyLB(cl, 1:nrow(combinations), execute_uncertainty_fun)
  })
  
  #cat("Elapsed time: ", timing[3], " seconds\n")
  # Clean up parallel backend
  stopCluster(cl)
  
  
  #save the output to an rds folder 
  saveRDS(result_list, file = rds_file_path)
  
  #saveRDS(result_list, "scenario60yrUncertainty.rds")
  
  rm(result_list)
  
  # Print the execution time
  #cat("Elapsed time: ", timing[3], " seconds\n")
}


result_list


#-----------------calculate rel occ for each  iteration and species category ----
cap <- 1.5 # don't allow scenario occ to be more than 1.5 starting landscape occ [only used if calculating geometric mean]
sppCategories <- readRDS("Outputs/sppCategoriesSept24.rds")
sppCategories<- as.data.table(sppCategories)

#where data on occ_60yr of scenarios is stored 
rds_folder <- "Outputs/occ60PerScenarioIterationJan25"
occ60_files <- list.files(rds_folder, pattern = "*.rds", full.names = TRUE)

#occ60_files <- occ60_files[2:3]

#SL_60yrOcc 
SL_occ60 <- readRDS("Outputs/SLoccOutputs/SL_occ60yr_perIterationSept24.rds") 
SL_occ60_dt <- rbindlist(SL_occ60) %>%
  rename(SL_occ_60yr = occ_60yr)

#EXTRACT ONLY THE BASELINE ALL_PRIMARY SL
SL_all_primary_dt<- SL_occ60_dt %>% filter(scenarioStart == "all_primary") 
rm(SL_occ60_dt)

#allocate folder to hold raw relative occupancy values, for further apraisal 
raw_rel_occ_folder <- "Outputs/Rel_Occ_PerIterationSept24"

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

  #export raw relative occupancy values 
  #saveRDS(occ_comb, file = "relOcc_file_path")
  
  #add in species categories 
  occ_comb <- sppCategories[occ_comb, on = "species"]

  #summarise species-level median rel occ across 500 iterations 
 
  rel_occ <- occ_comb %>%  group_by(species, index, production_target) %>%  
    summarize(
      medianRelativeOccupancy = median(rel_occ, na.rm = TRUE),
      meanRelativeOccupancy = mean(rel_occ, na.rm = TRUE),
      p1_medianRelativeOccupancy = quantile(rel_occ, 0.1, na.rm = TRUE),
      p9_medianRelativeOccupancy = quantile(rel_occ, 0.9, na.rm = TRUE)
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
best_scenario_folder <- "Outputs/BestScenarioUncertainty"

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
saveRDS(output, "FinalPerformanceOutput/OG_baseline_birdsSept24.rds")
saveRDS(outputIUCN, "FinalPerformanceOutput/OG_baseline_birdsIUCNSept24.rds")

#output when instead calculating median and CI relative abundance on a per-species basis; this
#reliably shows error on a per-species basis.

saveRDS(rel_occ_df, "FinalPerformanceOutput/species_level_relative_occ.rds")


#GC 11/06/24
#Assess the bird outcomes of different scenarios, where each scenario is disaggregated by age
#Nb 11.06.24 - still need to add in yield-corrected scenarios and outputs of full bird model


#This code:
#1. Uses model outputs from Bayesian spp occ to summarise spp categories 
#2. To propate through bird outcomes for each spp.

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


#read in the scenario parametres containing conversion factors for converting from point to parcel/entire landscape  
source("Inputs/ScenarioParams.R")

#this allows table joins to be 100 million rows (instead of 50,000,000 )
options(datatable.cautious = 100e6)

#read in Inputs ####

#-----read in scenarios without delays to get scenario composition -------
scenarios <- readRDS("Inputs/MasterAllScenarios.rds")
scenario_composition <- rbindlist(scenarios, use.names=TRUE) # get scenario composition
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
  select(spp, redlistCategory) %>% unique()%>% rename(species = spp) %>% as.data.table() %>%  
  mutate(threatened = case_when(
    redlistCategory != "Least Concern" & redlistCategory != "" ~ "Y",
    TRUE ~ "N"
  ))


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
saveRDS(processed_birds, "Outputs/processedOccBirdsSept24.rds")

#----can start HERE ----
processed_birds <- readRDS("Outputs/processedOccBirdsSept24.rds")

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
saveRDS(sppCategories,"Outputs/sppCategoriesSept24.rds")


#-----plot data ------
species_filt <- birds %>%  
  filter(dependency =="high") %>% 
  select(species) %>%
  unique %>% slice(31:60)

#NB there is considerable uncertainty in occupancy through time; make sure to get this across in manuscript
#plot occupancy by species and habitat (with Confidence intervals)
birds %>% 
  filter(habitat %in% c("primary", "once-logged", "twice-logged", "restored")) %>% 
  #filter(species == "Helmeted Hornbill") %>% 
  filter(species %in% species_filt$species) %>% 
  
  ggplot(aes(functionalhabAge, occ)) +
  geom_line()+
  #geom_ribbon(aes(ymin = occ - se_occ, ymax = occ + se_occ), fill = "blue", alpha = 0.2) +
  geom_line(aes(y = occ_lwr), linetype = "dashed", color = "red") +
  geom_line(aes(y = occ_upr), linetype = "dashed", color = "red") +   #geom_ribbon(aes(ymin = mean_occ - se_occ, ymax = mean_occ + se_occ), alpha = 0.2) + # SE envelope
  #facet_wrap(~habitat)+
  facet_grid(habitat~species) +
  ylim(0,1)+
  theme(strip.text = element_text(hjust=0, face="bold"), 
        strip.background = element_blank(), 
        axis.text = element_text(colour="black")) +
  labs(y = "P(occupancy)", x = "Plantation age")+
  theme_bw()

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
  
  # Step 4: Calculate occ_60yr for each iteration and species
  #[calculate occ60 for each iteration and species]
  result <- result[, .(occ_60yr = sum(landscape_occ)), 
                   by = .(species, scenarioStart, iteration)]
}

execute_SL_fun <-function(zeta) {
  single_scenario_name <- combinations_SL$scenarioStart[zeta]
  processed_birds_name <- combinations_SL$species[zeta]
  
  single_scenario_i <- all_start_landscape_scaled[scenarioStart == single_scenario_name]
  processed_birds_i <- processed_birds[species == processed_birds_name]
  
  result <- function_SL_60yr_uncertainty(single_scenario_i, processed_birds_i)
  
  return(result)
}

#apply the starting landscape function for each starting landscape and species in combinations SL
#do this to calculate occ_60yr for each species, scenarioStart, and posterior draw iteration 
result_list_SL <- lapply(1:nrow(combinations_SL), execute_SL_fun) 

saveRDS(result_list_SL,"Outputs/SLoccOutputs/SL_occ60yr_perIterationSept24.rds")

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
  
  # # Step 5: Summarise across posterior draws
  # result <- result [, .(mean_60yr = mean(occ_60yr),
  #                       sd_60yr_error = sd(occ_60yr),
  #                       se_60yr_error = sd(occ_60yr) / sqrt(.N)),
  #                       by = .(index, species, production_target)]
  
  return(result)
}


#-----read in scenario group and define harvest delay ------

#DEFINE DELAY FILTER ####
#(we  subset only a few delay schedules to improve computational efficiency)
#delay filter (availalbe 0-29 in 1 year increments basically allow each scenario to be delayed in its first conversion by the delay filter)
delayFilters <- c("delay 0", "delay 29")
harvest_window <-  length(delayFilters)##how many harvest delays?

#set a folder for saving outputs, showing for each species and scenario and iteration, occ_60 for lanscape
rds_folder <- "Outputs/occ60PerScenarioIterationSept24"

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
  # # Choose a smaller number of scenairos indices and species for testing
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
  
  #saveRDS(result_list, "scenario60yrUncertainty1.rds")
  
  rm(result_list)
  
  # Print the execution time
  cat("Elapsed time: ", timing[3], " seconds\n")
}


#-----------------calculate rel occ for each  iteration and species category ----
cap <- 1.5 # don't allow scenario occ to be more than 1.5 starting landscape occ [only used if calculating geometric mean]
sppCategories <- readRDS("R_code/AssessBiodiversityOutcomes/Outputs/sppCategoriesSept24.rds")
sppCategories<- as.data.table(sppCategories)

rds_folder <- "Outputs/occ60PerScenarioIterationSept24"
occ60_files <- list.files(rds_folder, pattern = "*.rds", full.names = TRUE)

#occ60_files <- occ60_files[2:3]

#SL_60yrOcc 
SL_occ60 <- readRDS("Outputs/SL_occ60yr_perIterationSept24.rds") 
SL_occ60_dt <- rbindlist(SL_occ60) %>%
  rename(SL_occ_60yr = occ_60yr)

#EXTRACT ONLY THE BASELINE ALL_PRIMARY SL
SL_all_primary_dt<- SL_occ60_dt %>% filter(scenarioStart == "all_primary") 

# Allocate folder for geomresults
#geom_result_folder <- "R_code/AssessBiodiversityOutcomes/Outputs/GeometricMeansPerIteration"
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
  #relOcc_file_path <- file.path(raw_rel_occ_folder, relOcc_file_name) 
  
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
  
  
  #calculate rel_occ; if rel_occ is > cap, replace with cap, to ensure scenario landscape cannot be more than 1.5 of starting landscape
  #occ_comb[, rel_occ_capped := pmin((occ_60yr / SL_occ_60yr), cap)]
  
  #calculate rel_occ -uncapped, for calculating median 
  occ_comb <- occ_comb[, rel_occ := occ_60yr / SL_occ_60yr]
  
  #export raw relative occupancy values 
  #saveRDS(occ_comb, file = "relOcc_file_path")
  
  #add in species categories 
  occ_comb <- sppCategories[occ_comb, on = "species"]
  
  #calculate geometric mean of each iteration of posterior draw. Thus we will end up with 500 geometric means 
  #per spp category,and scenario 
  
  # geom_means <- occ_comb %>% group_by(iteration, spp_category, index, production_target) %>%  
  #   summarise(geometric_mean = exp(mean(log(rel_occ_capped),na.rm = TRUE)), 
  #             medRelOcc = median(rel_occ,na.rm = TRUE)) %>% as.data.table()
  
  #summarise species-level median rel occ across 500 iterations 
  
  # Summarize species-level median rel occ across iterations
  rel_occ <- occ_comb[, .(SppMedRelOcc = median(rel_occ, na.rm = TRUE)), 
                         by = .(species, index, production_target)]
  
  #save the output to an rds folder 
  saveRDS(rel_occ, file = occ_file_path)
}

#--------  read in summarized outputs ----------------------
relOcc_result_folder <- "Outputs/Rel_Occ_PerIterationSept24"
relOcc_files <- list.files(relOcc_result_folder, pattern = "^OGbaseline.*\\.rds$", full.names = TRUE)

relOcc <- lapply(geomMean_files, relOcc_files)

rel_occ_df <-rbindlist(relOcc_files)
helmeted_hornbill <- rel_occ_df  %>% filter(species == "Helmeted Hornbill")
greatArgus <-  rel_occ_df  %>% filter(species == "Great Argus")
#----- summarise geom means and across posterior draws  -----


#summarised across data that has already been medianed per spp from 500 draws


#for species groupings (winner, loser, intermediate)
summarise_across_posterior_fun <- function(x){
  x %>% left_join(sppCategories, by = "species") %>%
    group_by(spp_category, index, production_target) %>%  
    summarise(medianRelativeOccupancy = median(SppMedRelOcc),
              p5_medianRelativeOccupancy = quantile(SppMedRelOcc, 0.05),
              p95_medianRelativeOccupancy = quantile(SppMedRelOcc, 0.95), 
              IQR = IQR(SppMedRelOcc))
  
}
#---output of summarised statistics across posterior draws ----
final_relOcc <- lapply(relOcc, summarise_across_posterior_fun)
final_relOcc <- rbindlist(final_relOcc)

#add back in key information 
final_relOcc <- final_relOcc %>%  left_join(scenario_composition, by = c("index", "production_target"))# %>% 


#for IUCN near threatened species 
summarise_IUCN_across_posterior_fun <- function(x){
  x %>% left_join(IUCN_classification, by = "species") %>%
    group_by(threatened, index, production_target) %>%  
    summarise(medianRelativeOccupancy = median(SppMedRelOcc),
              p5_medianRelativeOccupancy = quantile(SppMedRelOcc, 0.05),
              p95_medianRelativeOccupancy = quantile(SppMedRelOcc, 0.95), 
              IQR = IQR(SppMedRelOcc))
  
}

final_IUCN <- lapply(final_relOcc, summarise_IUCN_across_posterior_fun)
final_IUCN <- rbindlist(final_IUCN)

#add back in key information 
final_IUCN <- final_IUCN %>%  left_join(scenario_composition, by = c("index", "production_target"))# %>% 



#-----EXPORT OUTCOME PERFORMANCE for consolidated figure of all outcomes -----
getwd()
names(final_geoms)
#output of grouping by winner, loser, int
output <- final_relOcc %>% select(index, production_target, scenarioName,scenarioStart,
                                 medianRelativeOccupancy,p5_medianRelativeOccupancy, p95_medianRelativeOccupancy,
                                 spp_category) %>% cbind(outcome = "birds")

#output of grouping by IUCN threatened or not
outputIUCN <- final_IUCN %>% select(index, production_target, scenarioName,scenarioStart,
                                    medianRelativeOccupancy,p5_medianRelativeOccupancy, p95_medianRelativeOccupancy,
                                    threatened) %>% cbind(outcome = "birds")

#outuput when using starting landscape as baseline 
# saveRDS(output, "R_code/AllOutcomesFigure/Data/birds.rds")
# saveRDS(outputIUCN, "R_code/AllOutcomesFigure/Data/birdsIUCN.rds")

#outputs when using fully primary baseline
saveRDS(output, "FinalPerformanceOutput/OG_baseline_birdsSept24.rds")
saveRDS(outputIUCN, "FinalPerformanceOutput/OG_baseline_birdsIUCNSept24.rds")

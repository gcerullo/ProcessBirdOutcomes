#GC 11/06/24
#Assess the bird outcomes of different scenarios, where each scenario is disaggregated by age

#Nb 11.06.24 - still need to add in yield-corrected scenarios and outputs of full bird model

library(tidyr)
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

#------- MAKE KEY DECISIONS FOR SCRIPT HERE ---------------
#this allows table joins to be 100 million rows (instead of 50,000,000 )
options(datatable.cautious = 100e6)

#-----read in scenarios -------
#either yield 
#NEED TO CORRECT WITH PROPER YIELDS !!!!!! ####
scenarios <- readRDS("Inputs/allScenariosStaggered.rds")

#------------SELECT CORRECT FOLDER TO SAVE OUTPUT (temporal scenarios) -------------- 

#stores yield-matched scenarios, with temporal split
csv_folder <- "Outputs/scenariosForBirdsToBatch"

#---------- define final output folder for storing processed bird outputs from scenarios -----------
#define folder for saving, per scenario, final bird outputs (time-averaged-occupancy per scenario and species) 
final_output_folder <- "Outputs/TimeAvOccFinalBirds"

#---------- get scenario compositions -------------------------------
#get scenario composition by reading in scenarios as an rds
#(not processed with code above to be temporal, #but allows us to get scenario composition efficiently 

#yield matched
scenarios <- readRDS("Inputs/allScenariosStaggered.rds")

scenario_composition <- rbindlist(scenarios, use.names=TRUE) # get scenario composition
rm(scenarios) 

#define total points of a landscape 
total_landscape_pts <- bird_CF*1000

#------- END KEY DECISIONS FOR SCRIPT HERE ---------------

#make scenarios temporally organized
#read in Scenarios 
#scenarios where all plantation conversion happens in year 0 
#scenarios <- readRDS("R_code/BuildScenarios/BuildingHarvestingScenarios/allScenarios.rds") 


hab_by_year <- read.csv("Tables/HabByYears.csv", strip.white = TRUE) %>%  
  rename(true_year = year, 
         functionalhabAge = functional_habAge, 
         habitat = transition_habitat) %>% select(-X)
hab_by_year$habitat %>% unique

#INCORPORATE TEMPORAL INFORMATION INTO SCENARIOS####

# ---------1. add time-delay to temporal information, so that we allow a 30 year harvest window  ------
#hab_by year currently assumes all harvesting starts in year 0. 
#what if delay harvests/first plantation plants? 

hab_by_year0 <- hab_by_year %>% cbind(harvest_delay = "delay 0")
#for a time window of 30 years apply harvesting annually
delay <- seq(1:29)

output_list <- list()


for (i in delay) {
  #make a datafrmae of 0s to add to beginning, to cause delay and leave habitat as original cover for the time of delay
  yearZ <- hab_by_year %>%
    filter(true_year == 0) %>%
    uncount(i) %>%
    group_by(original_habitat,habitat) %>%
    mutate(true_year = seq_along(true_year)-1 ) %>%
    ungroup() %>% 
    #give age of original habitat during harvest delay, except for primary and deforested
    mutate(functionalhabAge = case_when(
      functional_habitat != "primary" & functional_habitat != "deforested" ~ true_year,
      TRUE ~ functionalhabAge)) 
  
  #push true years by the length of the delay (e.g. add in 0s) and remove true year >60 
  delayed_df <-hab_by_year %>%   
    mutate(true_year = true_year + i ) %>% 
    filter(true_year <61)
  
  #combine then remove beyond 60th years 
  output <- yearZ %>%  rbind(delayed_df) %>% cbind(harvest_delay = paste("delay",i))
  
  output_list[[as.character(i)]] <- output
}


#make sure we have all the years with same amount of rows
x <-  rbindlist(output_list)
test <- x %>% group_by(true_year) %>% count()
plot(test) # ;looks good 

#this hab_by_year now includes the temporal dimension assuming different delays until first harvest
hab_by_year<- rbindlist(output_list) %>% rbind(hab_by_year0)

#adjust to make sure that if original habitat = transition habitat for scenario duration
#then functionalhabAge age doesn't reset after the delay
hab_by_year <- hab_by_year %>% mutate(functionalhabAge = case_when(
  original_habitat == habitat ~ true_year,
  TRUE ~ functionalhabAge))



#get just plantation data, so that we can calculate average plantation yields, assuming staggered harvests
# plant_data <- hab_by_year %>% 
#   filter(grepl("euc|alb", habitat, ignore.case = TRUE))
# write.csv(plant_data, "Tables/PlantationYearsForCalculatingPlantationYields.csv")


#---------- ADD TEMPORAL INFORMATION TO SCENARIOS --------  ####

#nb: only original habitat -> transition_habitat is possible
scenarios[[16]] %>% select(scenarioName, original_habitat) %>% unique
scenarios[[13]] %>% select(scenarioName, original_habitat) %>% unique
scenarios[[11]] %>% select(scenarioName, original_habitat) %>% unique
#so for instance, you CAN'T go from primary to once-logged 
#AND THEN once-logged to restored; this is not possible in my scenarios
#This means we can join age values by original and transition habitat
#then assess ACD based on the functional habitat
add_temporal_fun <- function(x){
  x %>% left_join(hab_by_year, by = c("original_habitat" = "original_habitat", 
                                      "habitat" = "habitat"), relationship = "many-to-many") 
}

scenarios <- lapply(scenarios, add_temporal_fun)

# save temporal scenarios, 1csv per scenario --------------------------
#TAKES 30 MINS TO RUN CAN SKIP #####


# save each scenario as a csv in its own folder - we will process each scenario seperately 
#to save on memory
for (i in seq_along(scenarios)) {
  # Generate a unique CSV file name
  csv_file_name <- paste("scenario", i, ".csv", sep = "")
  
  # Combine folder path and file name to create full file path
  csv_file_path <- file.path(csv_folder, csv_file_name)
  # -----UNCOMMENT!!!! ------
  # Save the current element as a CSV file
  #  write.csv(scenarios[[i]], file = csv_file_path, row.names = FALSE)
  
  
  # Print a progress message
  cat("CSV file", csv_file_name, "for scenario", "saved. Progress:", i, "out of", length(scenarios), "\n")
}

# saveRDS(scenarios, "R_code/AssessBiodiversityOutcomes/Outputs/scenarios0_15_29_yeardelay.rds")


# ------ PROCESS BIRD OCCUPANCY DATA ACROSS POSTERIOR DRAWS  ------------
#  build the habitat by age table from summarising 500 draws -------------
#NB;The situations in which you want to propagate error (and so can't simply use the summarised estimates) are when you want to derive a quantity other than one that is spat out by the model directly. E.g. if you wanted to calculate average occupancy over 0-3 years of plantation age. In this situation you calculate the derived quantity per draw and then summarise across these. So in this example it would be: calculate the average occupancy over 0-3 years for each posterior draw, and then the distribution of this derived quantity (across your posterior draws) gives you your uncertainty-propagated derived quantity. You can summarise this down to a mean + CI by averaging across draws and taking X quantiles. Does that make sense?

#read in summarised birds
birds_summarised <- {readRDS("R_code/AssessBiodiversityOutcomes/Inputs/spRand_bird_occupancy_by_habAge_500_draws_summarised.rds") %>% 
    mutate(habitat = case_when(
      habitat == "Albizia_falcataria" ~ "albizia_current",
      habitat == "Eucalyptus_pellita" ~ "eucalyptus_current",
      habitat == "Twice_logged" ~ "twice-logged",
      habitat == "Once_logged" ~ "once-logged",
      habitat == "Restored" ~ "restored",
      habitat == "Primary" ~ "primary",
      TRUE ~ habitat)) %>%
    
    rename(occ = mid, 
           occ_lwr = lwr, 
           occ_upr = upr) %>% 
    
    select(species,habitat, dependency, time_since_logging, plantation_age, occ, occ_lwr, occ_upr) %>% 
    
    mutate(functionalhabAge = coalesce(time_since_logging, plantation_age)) %>% 
    select(-c(time_since_logging, plantation_age)) %>% 
    
    
    mutate(functionalhabAge = case_when(
      habitat == "primary" ~ 0,
      habitat == "twice-logged" ~ 0,
      TRUE ~ functionalhabAge)) %>%  
    mutate(functionalhabAge  = round(functionalhabAge,0)) }


#shows birds separately for each posterior draw iteration;
birds_raw <- readRDS("R_code/AssessBiodiversityOutcomes/Inputs/spRand_bird_occupancy_by_habAge_500_draws.rds") %>% 
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
  
  
  # Step 2: Add "improved" habitat
  improved <- birds %>%
    filter(habitat %in% c("eucalyptus_current", "albizia_current")) %>%
    mutate(
      habitat = case_when(
        habitat == "eucalyptus_current" ~ "eucalyptus_improved",
        habitat == "albizia_current" ~ "albizia_improved",
        TRUE ~ habitat
      )
    )
  
  birds <- bind_rows(birds, improved)
  
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
#----can start HERE ----
#saveRDS(processed_birds, "R_code/AssessBiodiversityOutcomes/Outputs/processed_birds.rds")
processed_birds <- readRDS("R_code/AssessBiodiversityOutcomes/Outputs/processed_birds.rds")

#remove improved improved yields
processed_birds <- as.data.table(processed_birds)
# Filter out rows where 'habitat' contains the string "improved" (case-insensitive)
processed_birds <- processed_birds[!grepl("improved", habitat, ignore.case = TRUE)]
# getwd()

#----summarise across posterior draws (for visualisation only) ----
# #calculate standard error
# birds <- processed_birds[, .(occ = mean(occ), se_occ = sqrt(var(occ)/.N)), 
#    by = .(species, habitat, functionalhabAge, dependency)] # group by these variable and calculate the mean and standard error 

#calculate 90% confidence intervals 
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
saveRDS(sppCategories,"R_code/AssessBiodiversityOutcomes/Outputs/sppCategories.rds")


#-----plot data ------
species_filt <- birds %>%  filter(dependency =="high") %>% select(species) %>% unique %>% slice(31:60)
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


# scenario_filters <- c("all_primary_CY_D.csv")
# spp_filter <- combined_df %>% select(species) %>% unique %>%  slice(1:100) %>% as.vector()
# #, "mostly_1L_CY_D.csv", "mostly_2L_CY_D.csv")
# 
# birds %>% filter(habitat %in% c("primary", "once-logged", "twice-logged", "restored")) %>% 
#   filter(species %in% species_filt$species) %>% 
#   filter(production_target == 0.5) %>% 
#   group_by(production_target, species) %>% slice(1) %>% ungroup() %>% 
#   ggplot(aes(x = species, y = occ_60yr)) +
#   geom_point(position = position_dodge(width = 0.7), size = 3) +
#   geom_errorbar(aes(ymin = occ_60yr_lwr, ymax = occ_60yr_upr), width = 0.25, position = position_dodge(0.7)) +
#   facet_wrap(~scenarioStart, scales = "free_x") +
#   labs(x = "Species", y = "Occ_60yr") +
#   theme_minimal()+
#   # Flip x-axis labels 90 degrees
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# 
# birds %>% select(habitat) %>% unique


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

saveRDS(result_list_SL,"R_code/AssessBiodiversityOutcomes/Outputs/SL_occ60yr_perIteration.rds")


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
#get the csv file name for each scenario 
csv_files <- list.files(csv_folder, pattern = "*.csv", full.names = TRUE)


#DEFINE DELAY FILTER ####
#(we have to subset only a few delay schedules to improve computational efficiency)
#delay filter (availalbe 0-29 in 1 year increments basically allow each scenario to be delayed in its first conversion by the delay filter)
delayFilters <- c("delay 0", "delay 29")
#delayFilters <- c("delay 0")
harvest_window <-  length(delayFilters)##how many harvest delays?

#set a folder for saving outputs, showing for each species and scenario and iteration, occ_60 for lanscape
rds_folder <- "R_code/AssessBiodiversityOutcomes/Outputs/occ60PerScenarioIteration"

for (k in seq_along(csv_files)){
  
  #read in single scenario type and add total points   
  scenario_group  <- read.csv(csv_files[[k]]) %>% as.data.table() %>% 
    mutate(num_points = num_parcels*bird_CF) %>%  
    filter(harvest_delay == delayFilters)  # only filter subset of delay to enhance efficieny 
  
  # Generate a unique rds file name
  rds_file_name <- sub(".csv$", "", unique(scenario_group$scenarioName))
  rds_file_name <- paste(unique(rds_file_name), "occ60.rds", sep = "_")
  
  # Combine folder path and file name to create full file path
  rds_file_path <- file.path(rds_folder, rds_file_name)   
  
  #=====
  # # Choose a smaller number of indices and species for testing
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


#-----------------calculate geometric for each  iteration and species category ----
cap <- 1.5 # don't allow scenario occ to be more than 1.5 starting landscape occ [only used if calculating geometric mean]
sppCategories <- readRDS("R_code/AssessBiodiversityOutcomes/Outputs/sppCategories.rds")
sppCategories<- as.data.table(sppCategories)
IUCN_classification <- read.csv("R_code/AssessBiodiversityOutcomes/Inputs/AllBorneoSpeciesTraits.csv") %>% 
  select(spp, redlistCategory) %>% unique()%>% rename(species = spp) %>% as.data.table() %>%  
  mutate(threatened = case_when(
    redlistCategory != "Least Concern" & redlistCategory != "" ~ "Y",
    TRUE ~ "N"
  ))

rds_folder <- "R_code/AssessBiodiversityOutcomes/Outputs/occ60PerScenarioIteration"
occ60_files <- list.files(rds_folder, pattern = "*.rds", full.names = TRUE)

#occ60_files <- occ60_files[2:3]

#SL_60yrOcc 
SL_occ60 <- readRDS("R_code/AssessBiodiversityOutcomes/Outputs/SL_occ60yr_perIteration.rds") 
SL_occ60_dt <- rbindlist(SL_occ60) %>%
  rename(SL_occ_60yr = occ_60yr)

#EXTRACT ONLY THE BASELINE ALL_PRIMARY SL
SL_all_primary_dt<- SL_occ60_dt %>% filter(scenarioStart == "all_primary") 

# Allocate folder for geomresults
geom_result_folder <- "R_code/AssessBiodiversityOutcomes/Outputs/GeometricMeansPerIteration"
#allocate folder to hold raw relative occupancy values, for further apraisal 
raw_rel_occ_folder <- "R_code/AssessBiodiversityOutcomes/Outputs/Rel_Occ_PerIteration"

for (w in seq_along(occ60_files)){
  occ60 <- readRDS(occ60_files[[w]])
  occ60_dt <- rbindlist(occ60)
  
  # Generate a unique  file name
  rds_file_name <- paste("OGbaseline_", basename(occ60_files[[w]]), sep = "")
  #relOcc_file_name <- paste("relOcc_", basename(occ60_files[[w]]), sep = "")
  
  
  # Combine folder path and file name to create full file path
  geom_file_path <- file.path(geom_result_folder, rds_file_name)   
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
  geom_means <- occ_comb[, .(SppMedRelOcc = median(rel_occ, na.rm = TRUE)), 
                         by = .(species, index, production_target)]
  
  #save the output to an rds folder 
  saveRDS(geom_means, file = geom_file_path)
}

#--------  read in geom means ----------------------
geom_result_folder <- "R_code/AssessBiodiversityOutcomes/Outputs/GeometricMeansPerIteration"
geomMean_files <- list.files(geom_result_folder, pattern = "*.rds", full.names = TRUE)
#read in data that has been baselined the fully old-growth starting landscape  
geomMean_files <- list.files(geom_result_folder, pattern = "^OGbaseline.*\\.rds$", full.names = TRUE)

geomMeans <- lapply(geomMean_files, readRDS)

rel_occ <-rbindlist(geomMeans)
helmeted_hornbill <- rel_occ  %>% filter(species == "Helmeted Hornbill")
greatArgus <-  rel_occ  %>% filter(species == "Great Argus")
#----- summarise geom means and across posterior draws  -----

#check; are values normally distributed across posterior draws - if not then take median of median rel occ
# #or median of geometric means 
# subGeom <- geomMeans[[1]] %>% filter(spp_category == "loser") %>% select(index) %>% 
#   unique() %>% slice(1:80) %>% pull()
# checkDist <- geomMeans[[1]] %>% filter(index %in% subGeom)
# 
# #looks like a fairly normal distribution; can take means across bootstraps
# checkDist %>% ggplot(aes(x = SppMedRelOcc)) +
#   geom_histogram(binwidth = 0.01, color = "black", fill = "blue", alpha = 0.6) +
#   facet_wrap(~index, scales = "free") +
#   labs(
#     title = "Histogram of medianRelOcc by Index",
#     x = "medianRelOcc",
#     y = "Frequency"
#   )+
#   xlim(0, 1)
# 
# checkDist %>% filter(index == "all_primary_CY_D.csv 6") %>% 
#   ggplot(aes(x = SppMedRelOcc)) +
#   geom_histogram(binwidth = 0.01, color = "black", fill = "blue", alpha = 0.6) +
#   facet_wrap(~index, scales = "free") +
#   labs(
#     title = "Histogram of medianRelOcc by Index",
#     x = "medianRelOcc",
#     y = "Frequency"
#   )+
#   xlim(0, 1)



# summarise_across_posterior_fun <- function(x){
#   x %>% group_by(spp_category, index, production_target) %>%  
#     summarise(geom_mean =median(geometric_mean), 
#               medianRelativeOccupancy = median(medRelOcc),
#               p5_medianRelativeOccupancy = quantile(medianRelativeOccupancy, 0.05),
#               p95_medianRelativeOccupancy = quantile(medianRelativeOccupancy, 0.95), 
#               IQR)
#               
# }

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
final_geoms <- lapply(geomMeans, summarise_across_posterior_fun)
final_geoms <- rbindlist(final_geoms)

#add back in key information 
final_geoms <- final_geoms %>%  left_join(scenario_composition, by = c("index", "production_target"))# %>% 


#for IUCN near threatened species 
summarise_IUCN_across_posterior_fun <- function(x){
  x %>% left_join(IUCN_classification, by = "species") %>%
    group_by(threatened, index, production_target) %>%  
    summarise(medianRelativeOccupancy = median(SppMedRelOcc),
              p5_medianRelativeOccupancy = quantile(SppMedRelOcc, 0.05),
              p95_medianRelativeOccupancy = quantile(SppMedRelOcc, 0.95), 
              IQR = IQR(SppMedRelOcc))
  
}

final_IUCN <- lapply(geomMeans, summarise_IUCN_across_posterior_fun)
final_IUCN <- rbindlist(final_IUCN)

#add back in key information 
final_IUCN <- final_IUCN %>%  left_join(scenario_composition, by = c("index", "production_target"))# %>% 



#-----EXPORT OUTCOME PERFORMANCE for consolidated figure of all outcomes -----
getwd()
names(final_geoms)
#output of grouping by winner, loser, int
output <- final_geoms %>% select(index, production_target, scenarioName,scenarioStart,
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
saveRDS(output, "R_code/AllOutcomesFigure/Data/OG_baseline_birds.rds")
saveRDS(outputIUCN, "R_code/AllOutcomesFigure/Data/OG_baseline_birdsIUCN.rds")
#---- Calculate Prop OG in scenario ----

# #get the amount of hab in each starting landscape 
# primaryInStart <- all_start_landscape %>% filter(habitat == "primary") %>%  
#   rename(SL_primary_parcels = num_parcels) %>% dplyr::select(-habitat)
# habInStart <- all_start_landscape %>% select(scenarioStart) %>% unique() %>% 
#   mutate(originalOG = c(1,0.2,0.2,0.8,0.2,0.2), 
#          original1L = c(0,0.8,0,0,0.6,0), 
#          original2L = c(0,0,0.8,0,0,0.6))
# 
# 
# #build a function that calculates proportion of remaining habitat 
# #in each scenario 
# 
# prop_OG_fun <- function(x){
#   
#   #proportion of TOTAL landscape [1000 parcels] in different habitat type 
#   x %>% group_by(index, production_target) %>% 
#     #total OG
#     mutate(propOG = sum(num_parcels[habitat == "primary"])/1000,
#            propPlant = sum(num_parcels[habitat %in% c("eucalyptus_current", "albizia_current", "albizia_future","eucalyptus_future")])/1000,   
#            #prop-1L in the scenario landscape
#            prop1L = sum(num_parcels[habitat == "once-logged"])/1000,
#            #proportion of 2-L in the scenario landscape
#            prop2L = sum(num_parcels[habitat == "twice-logged"])/1000) %>%  
#     
#     #get starting landscape
#     mutate(scenarioStart = scenarioName) %>% 
#     mutate(scenarioStart = str_remove(scenarioStart, "_IY_ND.csv")) %>%
#     mutate(scenarioStart = str_remove(scenarioStart, "_CY_ND.csv")) %>%
#     mutate(scenarioStart = str_remove(scenarioStart, "_IY_D.csv")) %>%
#     mutate(scenarioStart = str_remove(scenarioStart, "_CY_D.csv")) %>% 
#     ungroup %>% 
#     
#     #get total amount of each habitat in STARTING landscape for a scenario
#     left_join(habInStart, by = "scenarioStart") %>% 
#     
#     #calculate PROPORTION of REMAINING original habitat type 
#     #(nb there can actually be more once-logged or twice-logged forest in scenario than scenarioStart, if primary forest is logged)
#     mutate(remainingOG = propOG/originalOG, 
#            remaining1L = prop1L/original1L, 
#            remaining2L = prop2L/original2L) %>%  
#     #correct for INF values for if dividing by 0
#     mutate_at(vars(remainingOG, remaining1L, remaining2L), ~ ifelse(is.infinite(.) | is.nan(.), 0, .)) %>%
#     
#     select(index, production_target, scenarioName,scenarioStart,
#            propOG, propPlant,prop1L,prop2L,
#            remainingOG,remaining1L,remaining2L) %>% unique()
#   
# }
# 
# propOGcomp <- prop_OG_fun(scenario_composition) %>% ungroup
# 
# #for each scenario, add the proportion starting landscapes
# propOGcomp_dt <- as.data.table(propOGcomp)
# geom_results <- as.data.table(final_geoms)
# 
# #if index is numeric make character
# geom_results <- geom_results[, index := as.character(index)]
# geom_results <- geom_results[, production_target := as.numeric(production_target)]
# propOGcomp_dt <- propOGcomp_dt[, index := as.character(index)]
# propOGcomp_dt <- propOGcomp_dt[, production_target := as.numeric(production_target)]
# 
# geom_results_df <- propOGcomp_dt[geom_results, on = .(index, production_target)] 
# 
# 
# #---------- #BIVARIATE PLOTTING PARAMETRES --------------------
# 
# library(biscale)
# COL <- "DkBlue2" # define colour pallete
# COL <- "BlueOr"
# #get colours for bivariate plotting
# biv_pallete <- bi_pal(COL, dim =4 ) # for plotting
# cols <- data.frame(bi_pal(COL, dim = 4, preview = FALSE))
# colnames(cols) <- c("hex")
# cols <- cols %>% mutate(bi_class = rownames(.))
# 
# textSize  <- 15
# 
# #make bivar legend
# primary_legend <- bi_legend(pal = "BlueOr", dim = 4, 
#                             xlab = "Proportion old-growth", 
#                             ylab = "Proportion once-logged", size = textSize)
# 
# onceL_legend <- bi_legend(pal = "BlueOr", dim = 4, 
#                           xlab = "Proportion remaining old-growth", 
#                           ylab = "Proportion remainng once-logged",size = textSize)
# 
# twiceL_legend <- bi_legend(pal = "BlueOr", dim = 4, 
#                            xlab = "Proportion remaining old-growth", 
#                            ylab = "Proportion remainng twice-logged",size = textSize)
# 
# all_legend <- plot_grid(primary_legend,onceL_legend,twiceL_legend, ncol =3)
# 
# #assign scenarios the colours from the bivariate plot for primary start
# bivariate_colours_PRIM <- function(X){
#   X %>%  bi_class(x = propOG, y = prop1L, dim = 4, style = "equal") %>%  
#     left_join(cols, by = "bi_class") # add hex colours
# }
# geom_results_df<- bivariate_colours_PRIM(geom_results_df) %>% rename(hexP = hex)
# 
# #assign scenarios the colours from the bivariate plot for mostly 1L start
# bivariate_colours_1L <- function(X){
#   X %>%  bi_class(x = remainingOG, y = remaining1L, dim = 4, style = "equal") %>%  
#     left_join(cols, by = "bi_class") # add hex colours
# }
# geom_results_df<- bivariate_colours_1L(geom_results_df) %>% rename(hex1L = hex)
# 
# #assign scenarios the colours from the bivariate plot for mostly 2L start
# bivariate_colours_2L <- function(X){
#   X %>%  bi_class(x = remainingOG, y = remaining2L, dim = 4, style = "equal") %>%  
#     left_join(cols, by = "bi_class") # add hex colours
# }
# geom_results_df<- bivariate_colours_2L(geom_results_df) %>% rename(hex2L = hex)
# 
# #hex shows colours for 1L vs primary.
# #hex_2L shows colours for 2L vs primary
# 
# 
# # final_carbon_df_4DR <- bi_class(final_carbon_df_4DR, x = propOriginalOG, y = prop1L, dim = 4, style = "equal") %>%  
# #   left_join(cols, by = "bi_class") # add hex colours
# 
# #================= build some summary plts ==========================================================
# 
# #filter by category
# legend_plot <-  geom_results_df %>% filter(spp_category == "loser" & scenarioName == "all_primary_CY_D.csv") 
# losers <- geom_results_df %>% filter(spp_category == "loser") 
# intermediate1L <- geom_results_df %>% filter(spp_category == "intermediate1L") 
# intermediate2L <- geom_results_df %>% filter(spp_category == "intermediate2L") 
# winners <-  geom_results_df %>% filter(spp_category == "winner") 
# 
# 
# 
# scenario_filters <- c("all_primary_CY_D.csv", "mostly_1L_CY_D.csv", "mostly_2L_CY_D.csv")
# 
# 
# 
# #build PLOTTING FUNCTION #### 
# plot_fun <- function(x){
#   
#   x <- x %>%
#     filter(scenarioName %in% scenario_filters)
#   
#   #if scenario contains plantation add cross 
#   x <- x %>%
#     mutate(is_cross = ifelse(propPlant > 0, "Cross", "Point"))
#   
#   
#   max_propPlant <- max(x$propPlant, na.rm = TRUE)
#   
#   
#   #reorder facet order 
#   
#   x$scenarioName <- factor(x$scenarioName, levels = c(
#     "all_primary_CY_D.csv",
#     #"all_primary_CY_ND.csv","all_primary_IY_D.csv", "all_primary_IY_ND.csv",
#     "mostly_1L_CY_D.csv",
#     #"mostly_1L_CY_ND.csv", "mostly_1L_IY_D.csv", "mostly_1L_IY_ND.csv",
#     #"mostly_1L_deforested_CY_D.csv", "mostly_1L_deforested_CY_ND.csv", "mostly_1L_deforested_IY_D.csv", "mostly_1L_deforested_IY_ND.csv", 
#     "mostly_2L_CY_D.csv"))
#   #"mostly_2L_CY_ND.csv", "mostly_2L_IY_D.csv", "mostly_2L_IY_ND.csv",
#   #"mostly_2L_deforested_CY_D.csv", "mostly_2L_deforested_CY_ND.csv", "mostly_2L_deforested_IY_D.csv","mostly_2L_deforested_IY_ND.csv",
#   #"primary_deforested_CY_D.csv", "primary_deforested_CY_ND.csv", "primary_deforested_IY_D.csv","primary_deforested_IY_ND.csv"))
#   
#   
#   x %>%  ggplot(aes(x = production_target, y = medianRelativeOccupancy))+
#     
#     # conditionally colour so that if we plot bivariate between proportion of primary and proportion of least logged (either 1L or 2L depending on starting landscape) in the scenario    
#     geom_point(aes(
#       x = production_target,
#       # y = geom_mean,
#       y = medianRelativeOccupancy,
#       colour = case_when(
#         scenarioStart %in% c("all_primary", "primary_deforested") ~ hexP,
#         scenarioStart %in% c("mostly_1L", "mostly_1L_deforested") ~ hex1L,
#         scenarioStart %in% c("mostly_2L", "mostly_2L_deforested") ~ hex2L
#       ),
#       shape = is_cross, 
#     ), position = position_jitter(width = 0.05, height = -0.03)) +
#     scale_colour_identity()+
#     #GIVE CORSSES TO PLANTATION CONTAINING SCENAIOS ####
#   scale_shape_manual(values = c("Point" = 19, "Cross" = 3)) + # Define shape mapping
#     #scale_size_continuous(range = c(3, 8), breaks = seq(0, max_propPlant, by = 0.05)) + # Adjust size range and breaks
#  
#     xlim(0, 1)+
#     xlab("Production target")+
#     ylab(   "Median relative occupancy 
#   (averaged over posterior draws)")+
#     
#     #labs(colour = "Proportion of remaining old-growth forest spared")+
#     # labs(colour = "Proportion of plantation in remaining landscape")+
#     
#     facet_wrap(~scenarioName, ncol = 4)+
#     #   geom_hline(aes(yintercept = SL_geom_mean))+
#     theme_bw(base_size = textSize)+
#     theme(legend.position = "none")
#   
# }
# 
# 
# #get legend
# legend_plot <- plot_fun(legend_plot)
# legend <- get_legend(legend_plot + theme(legend.position = "bottom",         # c(0.5, 0.15),
#                                          legend.spacing.x = unit(1.0, 'cm'),
#                                          legend.title  = element_text(size  = 30, face = "bold"))) 
# #plot figures (without legends)
# losers <- plot_fun(losers)
# intermediate1L <- plot_fun(intermediate1L)
# intermediate2L <- plot_fun(intermediate2L)
# winners <- plot_fun(winners)
# 
# #add legend function
# add_legend <-  function(x){
#   plot_grid(x, legend, 
#             nrow =2 , ncol = 1,
#             rel_heights = c(1, 0.1))
# } 
# 
# #plot final figs for each above-defined category 
# add_legend(losers)
# plot_grid(losers, all_legend, nrow =2)
# add_legend(intermediate1L)
# plot_grid(intermediate1L, all_legend, nrow =2)
# add_legend(intermediate2L)
# plot_grid(intermediate2L, all_legend, nrow =2)
# add_legend(winners)
# plot_grid(winners, all_legend, nrow =2)
# 
# 
# #---UNUSED CODE----
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # ##----- non parrellised version -----------------
# #  
# # 
# # 
# # 
# # 
# #  
# #  
# #  #----slower function approach ---------------------------------------
# #  # function_scenario_60yr_uncertainty <- function(single_scenario2, processed_birds) {
# #  #   #join birds and scenarios and get num point for a given habitat type and stagger
# #  #   bird_join <- single_scenario2 %>% 
# #  #     left_join(processed_birds2, 
# #  #               by = c("functional_habitat" = "habitat", "functionalhabAge" = "functionalhabAge"),
# #  #               relationship = "many-to-many")
# #  #   
# #  #   bird_join <- as.data.table(bird_join)
# #  #   
# #  #   # Calculate hab_occ
# #  #   bird_join[, hab_occ := occ * num_points]
# #  #   
# #  #   bird_join[,parcel_occ_stag := hab_occ/harvest_window]
# #  #   #for each SPECIFIC true year and habitat transition, calculate occupancy combined across the staggered
# #  #   #harvesting schedule (i.e. the occupancy in a given habitat transition for a given year) 
# #  #   
# #  #   bird_join %>% 
# #  #     group_by(species, index, iteration,production_target, true_year, original_habitat, habitat) %>%
# #  #     summarise(occ_hab_year = sum(parcel_occ_stag)) %>% ungroup() %>% 
# #  #     #4. Across habitat type transitions (e.g for ALL hab_parcel transitions) in a scenario, calculate occupancy for a given year
# #  #     group_by(species, index, iteration, production_target, true_year) %>%
# #  #     summarise(landscape_occ = sum(occ_hab_year)/total_bird_pts) %>%
# #  #     ungroup() %>%
# #  #     #5 calculate occ60 for each iteration and species 
# #  #     group_by(species, index, iteration,production_target) %>%
# #  #     summarise(occ_60yr = sum(landscape_occ)) %>%
# #  #     ungroup() #%>%
# #  #     # group_by(index, species) %>%
# #  #     # summarise(mean_60yr = mean(occ_60yr),
# #  #     #           sd_60yr_error = sd(occ_60yr),
# #  #     #           se_60yr_error = sd(occ_60yr) / sqrt(n()))
# #  #   
# #  #   return(birds)
# #  # }
# #  # 
# #  
# # # 
# # # # Create an expanded grid of index-species combinations
# # # #small number for testing 
# # # combinations <- expand.grid(index = selected_indices, species = selected_species)
# # # #full set of combinatations
# # # #combinations <- as.data.table(expand.grid(index = unique_index$index, species = unique_spp$species))
# # # 
# # # # Pre-allocate a list for results
# # # result_list <- vector("list", nrow(combinations))
# # # 
# # # # Use lapply to apply the function to each combination
# # # timing <- system.time({
# # #   result_list <- lapply(1:nrow(combinations), function(zeta) {
# # #     single_scenario_name <- combinations$index[zeta]
# # #     processed_birds_name <- combinations$species[zeta]
# # #     
# # #     single_scenario_i <- scenario_group[index == single_scenario_name]
# # #     processed_birds_i <- processed_birds[species == processed_birds_name]
# # #     
# # #     result <- function_scenario_60yr_uncertainty(single_scenario_i, processed_birds_i)
# # #     
# # #     return(result)
# # #   })
# # # })
# # 
# # 
# # 
# # 
# # # Print the execution time
# # cat("Elapsed time: ", timing[3], " seconds\n")
# # 
# # 
# # 
# # 
# # # Use foreach for parallel processing
# # result_list <- foreach(zeta = 1:nrow(combinations), .combine = rbindlist) %dopar% {
# #   single_scenario_name <- combinations$index[zeta]
# #   processed_birds_name <- combinations$species[zeta]
# #   
# #   single_scenario_i <- scenario_group[index == single_scenario_name]
# #   processed_birds_i <- processed_birds[species == processed_birds_name]
# #   
# #   result <- function_scenario_60yr_uncertainty(single_scenario_i, processed_birds_i)
# #   
# #   return(result)
# # }
# # 
# # # Clean up the parallel backend
# # stopCluster(cl)
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # # Example usage
# # #combinations <- data.frame(index = c(1, 2, 3), species = c("A", "B", "C"))  # Replace with your data
# # #result_list <- lapply(1:nrow(combinations), function(zeta) {
# # 
# # # Create a list of data frames by iterating over combinations
# # result_list <- pmap(list(single_scenario = split(subset_scenario_group, unique(subset_scenario_group$index)), 
# #                          processed_birds = split(subset_processed_birds, subset_processed_birds$species)),
# #                     function(single_scenario, processed_birds) {
# #                       function_scenario_60yr_uncertainty(single_scenario, processed_birds)
# #                     })
# # 
# # 
# # 
# # 
# # ##.................................
# # 
# # # Convert data frames to data tables
# # as.data.table(scenario_group)
# # as.data.table(processed_birds)
# # 
# # 
# # # Create a list of subsets for each index and species
# # subset_scenario_list <- lapply(selected_indices, function(index) {
# #   scenario_group[scenario_group$index == index, ]
# # })
# # 
# # subset_processed_birds_list <- lapply(selected_species, function(species) {
# #   processed_birds[processed_birds$species == species,]
# # })
# # # Register parallel backend
# # cl <- makeCluster(detectCores())
# # registerDoParallel(cl)
# # 
# # # Profile your code for optimization
# # profvis({
# #   # Create an expanded grid of index-species combinations
# #   combinations <- expand.grid(index = selected_indices, species = selected_species)
# #   
# #   # Parallelize the computation
# #   result_list <- foreach(
# #     zeta = 1:nrow(combinations), 
# #     .combine = rbindlist
# #   ) %dopar% {
# #     single_scenario_name <- combinations$index[zeta]
# #     processed_birds_name <- combinations$species[zeta]
# #     
# #     single_scenario <- scenario_group[index == single_scenario_name]
# #     processed_birds_i <- processed_birds[species == processed_birds_name]
# #     
# #     function_scenario_60yr_uncertainty(single_scenario, processed_birds_i)
# #   }
# # })
# # 
# # # Clean up parallel backend
# # stopCluster(cl)
# # 
# # #The modifications above address the key areas for optimization. Make sure to adapt and further optimize your function_scenario_60yr_uncertainty function as needed. Also, remember to monitor memory usage and garbage collection to ensure efficient resource management.
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # #..............................
# # #..............................
# # 
# # 
# # 
# # 
# # #=======================
# # #========================
# # results_list <- list()
# # 
# # combinations <- as.data.frame(expand.grid(index = unique_index$index, species = unique_spp$species))
# # 
# # sc <- "all_primary_CY_D.csv 472"
# # sp <- "Zebra Dove" 
# # 
# # results <- list()
# #   #for(i in 1:2){
# #     #for(j in 1:2){
# #   
# # # Start profiling
# # Rprof(filename = "profile.out")
# # #zeta goes thru each row in the combinations, and apply the function to this combination of species 
# # #and g
# # function_scenario_60yr_uncertainty <-     function(zeta)  {
# #       
# #   single_scenario_name <- combinations$index[zeta]
# #   processed_birds_name <- combinations$species[zeta]
# # 
# #   
# #   
# #   #filter a single scenario
# #   single_scenario <- scenario_group %>% filter(index == single_scenario_name)
# #   #filter a single bird species 
# #   processed_birds_i <-  processed_birds %>% filter(species == processed_birds_name)
# #   
# #   #join the single scenario and bird 
# #   
# #   # # Join that scenarios (based on above set join keys) to bird occupancy data
# #   bird_join <- single_scenario %>% 
# #     left_join(processed_birds_i, 
# #               by = c("functional_habitat" = "habitat", "functionalhabAge" = "functionalhabAge"),
# #               relationship = "many-to-many")
# #   
# #   #get num point for a given habitat type and stagger
# #   birds <- bird_join %>% mutate(
# #     hab_occ = occ*num_points, 
# #     
# #     # calculate the occupancy of each habitat type across the landscape 
# #     ##assuming 1/NUM-of-different-harvest-delays of of each habitat type is applied to each harvesting delay schedule  
# #     parcel_occ_stag = (hab_occ) / harvest_window) %>% 
# #     
# #     #2. for each SPECIFIC true year and habitat transition, calculate occupancy combined across the staggered
# #     #harvesting schedule (i.e. the occupancy in a given habitat transition for a given year) 
# #     #(in data.table group_by equivalnet is written after the occ_hab year calculation, as below )      
# #     group_by(species, index, production_target,iteration, original_habitat, true_year,habitat) %>%  
# #     
# #     mutate(occ_hab_year = sum(parcel_occ_stag)) %>% 
# #     
# #     #3. #select a single harvest delay worth of data, as we now have calculated occupancy across harvesting schedules
# #     filter(harvest_delay == "delay 0") %>% ungroup() %>% 
# #     
# #     
# #     #4. Across habitat type transitions (e.g for ALL hab_parcel transitions) in a scenario, calculate occupancy for a given year
# #     group_by(species,index,iteration, production_target, true_year) %>% 
# #     mutate(landscape_occ = sum(occ_hab_year)) %>%  ungroup %>% 
# #     
# #     #5.  #now we make sure we only have one row for each scenario and year and species, showing scen_occ_year
# #     group_by(true_year, index,species, iteration) %>% slice(1) %>% 
# #     
# #     #rescale occupancy per species between 0-1 by dividing by the total number of points in the landscape(~319,000) 
# #     mutate(landscape_occ = landscape_occ/ total_bird_pts)  %>%  
# #     
# #     #6. sum landscape occ across years for each species and iteration 
# #     group_by(index, species,dependency, iteration) %>% 
# #     summarise(occ_60yr = sum(landscape_occ)) %>% ungroup()# %>% 
# # 
# #     # #calculate the error about sum of occ_60yr across bootstraps
# #     # group_by(index, species, dependency) %>%
# #     # summarise(mean_60yr = mean(occ_60yr),
# #     #           sd_60yr_error = sd(occ_60yr),
# #     #           se_60yr_error =  sd(occ_60yr) / sqrt(n()))
# # 
# #   return(birds)
# #     }
# #   
# #   
# # birds <- lapply(1:3,function_scenario_60yr_uncertainty)
# # head(birds)
# # 
# # #=======================
# # #========================
# # i <- "all_primary_CY_D.csv 472"
# # j <- "Zebra Dove" 
# # function_scenario_60yr_uncertainty <- function(i,j){
# # 
# #   #filter a single scenario
# #   single_scenario <- scenario_group %>% filter(index == i)
# #   #filter a single bird species 
# #   processed_birds_i <-  processed_birds %>% filter(species == j)
# #   
# #   #join the single scenario and bird 
# #   
# #   # # Join that scenarios (based on above set join keys) to bird occupancy data
# #   bird_join <- single_scenario %>% 
# #     left_join(processed_birds_i, 
# #               by = c("functional_habitat" = "habitat", "functionalhabAge" = "functionalhabAge"),
# #               relationship = "many-to-many")
# #   
# #   #get num point for a given habitat type and stagger
# #   birds <- bird_join %>% mutate(
# #     hab_occ = occ*num_points, 
# #     
# #  # calculate the occupancy of each habitat type across the landscape 
# #     ##assuming 1/NUM-of-different-harvest-delays of of each habitat type is applied to each harvesting delay schedule  
# #     parcel_occ_stag = (hab_occ) / harvest_window) %>% 
# #   
# #   #2. for each SPECIFIC true year and habitat transition, calculate occupancy combined across the staggered
# #     #harvesting schedule (i.e. the occupancy in a given habitat transition for a given year) 
# #     #(in data.table group_by equivalnet is written after the occ_hab year calculation, as below )      
# #     group_by(species, index, production_target,iteration, original_habitat, true_year,habitat) %>%  
# #     
# #     mutate(occ_hab_year = sum(parcel_occ_stag)) %>% 
# #   
# #   #3. #select a single harvest delay worth of data, as we now have calculated occupancy across harvesting schedules
# #   filter(harvest_delay == "delay 0") %>% ungroup() %>% 
# #     
# #     
# #  #4. Across habitat type transitions (e.g for ALL hab_parcel transitions) in a scenario, calculate occupancy for a given year
# #   group_by(species,index,iteration, production_target, true_year) %>% 
# #   mutate(landscape_occ = sum(occ_hab_year)) %>%  ungroup %>% 
# #   
# #   #5.  #now we make sure we only have one row for each scenario and year and species, showing scen_occ_year
# #   group_by(true_year, index,species, iteration) %>% slice(1) %>% 
# #     
# #   #rescale occupancy per species between 0-1 by dividing by the total number of points in the landscape(~319,000) 
# #    mutate(landscape_occ = landscape_occ/ total_bird_pts)  %>%  
# #     
# #   #6. sum landscape occ across years for each species and iteration 
# #     group_by(index, species,dependency, iteration) %>% 
# #     summarise(occ_60yr = sum(landscape_occ)) %>% ungroup() %>% 
# #     
# #     # #calculate the error about sum of occ_60yr across bootstraps 
# #     group_by(index, species, dependency) %>%
# #     summarise(mean_60yr = mean(occ_60yr),
# #               sd_60yr_error = sd(occ_60yr),
# #               se_60yr_error =  sd(occ_60yr) / sqrt(n()))
# #   
# #   cat("Result for Index", i, "and Species", j, ":\n")
# #   
# # }
# # 
# # 
# # 
# #  landscape_occ <- function(x) {
# #    x[, `:=`(
# #      #1. calculate the occupancy of each habitat type across the landscape 
# #      ##assuming 1/NUM-of-different-harvest-delays of of each habitat type is applied to each harvesting delay schedule
# #      parcel_occ_stag = (parcel_occ * num_parcels) / harvest_window,
# #      parcel_occ_min_stag = (parcel_occ_min * num_parcels) / harvest_window,
# #      parcel_occ_max_stag = (parcel_occ_max * num_parcels) / harvest_window
# #    )]
# #    
# #    x[, `:=`(
# #      #2. for each SPECIFIC true year and habitat transition, calculate occupancy combined across the staggered
# #      #harvesting schedule (i.e. the occupancy in a given habitat transition for a given year) 
# #      #(in data.table group_by equivalnet is written after the occ_hab year calculation, as below )
# #      occ_hab_year = sum(parcel_occ_stag),
# #      occ_hab_year_lwr = sum(parcel_occ_min_stag),
# #      occ_hab_year_upr = sum(parcel_occ_max_stag)                             #!!!!!!
# #    ), by = .(species, index, production_target, original_habitat, true_year,habitat)]
# #    
# #    #3. #select a single harvest delay worth of data, as we now have calculated occupancy across harvesting schedules
# #    x <- x[harvest_delay == "delay 0"]
# #    
# #    #4. Across habitat type transitions (e.g for ALL hab_parcel transitions) in a scenario, calculate occupancy for a given year
# #    x[, `:=`(
# #      scen_occ_year = sum(occ_hab_year),
# #      scen_occ_year_lwr = sum(occ_hab_year_lwr),
# #      scen_occ_year_upr = sum(occ_hab_year_upr)
# #    ), by = .(species,index, production_target, true_year)]
# #    
# #    #5.  #now we make sure we only have one row for each scenario and year and species, showing scen_occ_year
# #    #Select columns we want 
# #    x <- x[, .(species, index, production_target, scenarioName, scenarioStart, true_year,
# #               scen_occ_year, scen_occ_year_lwr, scen_occ_year_upr), by = .(true_year, index,species)]
# #    #this slice one row for each true_year and index and species
# #    x <- x[, .SD[1], by = .(species,true_year, index)]
# #    
# #    #rescale occupancy per species between 0-1 by dividing by the total number of points in the landscape(~319,000) 
# #    
# #    x[, `:=`(
# #      occ1_0 = scen_occ_year / total_landscape_pts,
# #      occ1_0_min = scen_occ_year_lwr / total_landscape_pts,
# #      occ1_0_max = scen_occ_year_upr / total_landscape_pts
# #    )]
# #    
# #  
# #  
# #  
# #  
# #  
# #  
# #  
# #  
# #  
# #  
# # # 
# # 
# # # function_SL_60yr_uncertainty <- function(x) {
# # #   bird_join <- data.table(x)[processed_birds, on = .(species, habitat), allow.cartesian = TRUE]
# # #   
# # #   # Calculate landscape_occ
# # #   birds <- bird_join[, .(hab_occ = sum(occ * num_points)),
# # #                      by = .(species, scenarioStart, iteration, functionalhabAge, dependency)]
# # #   
# # #   birds <- birds[, .(landscape_occ = sum(hab_occ) / (bird_CF * 1000)),
# # #                  by = .(scenarioStart, species, dependency, iteration)]
# # #   
# # #   # Calculate occ_60yr
# # #   birds <- birds[, .(occ_60yr = sum(landscape_occ)),
# # #                  by = .(scenarioStart, species, dependency, iteration)]
# # #   
# # #   # Calculate the error about the sum of occ_60yr
# # #   birds <- birds[, .(mean_60yr_error = mean(occ_60yr),
# # #                      sd_60yr_error = sd(occ_60yr)),
# # #                  by = .(scenarioStart, species, dependency)]
# # #   
# # #   return(birds)
# # # 
# # # }
# # # 
# # # # Create a list of unique species and scenarioStart combinations
# # # combinations <- unique(all_start_landscape_scaled[, .(species, scenarioStart)])
# # # 
# # # # Initialize an empty list to store the results
# # # results_list <- list()
# # # 
# # # # Iterate through each combination and calculate the mean_60yr_error and sd_60yr_error
# # # for (i in 1:nrow(combinations)) {
# # #   species_val <- combinations$species[i]
# # #   scenarioStart_val <- combinations$scenarioStart[i]
# # #   
# # #   #print progress
# # #   cat("Processing species:", species_val, "Scenario Start:", scenarioStart_val, "\n")
# # #   
# # #   
# # #   # Subset the data for the current combination
# # #   subset_data <- all_start_landscape_scaled[species == species_val & scenarioStart == scenarioStart_val]
# # #   
# # #   # Call the function and store the results in a list
# # #   result <- function_SL_60yr_uncertainty(subset_data)
# # #   
# # #   # Add species and scenarioStart to the result
# # #   result$species <- species_val
# # #   result$scenarioStart <- scenarioStart_val
# # #   
# # #   # Append the result to the list
# # #   results_list[[i]] <- result
# # # }
# # # 
# # # # Combine the results into a single dataframe
# # # result_df <- do.call(rbind, results_list)
# # # 
# # # # Reset row names
# # # rownames(result_df) <- NULL
# # # 
# # # 
# # # test <-  function_SL_60yr_uncertainty(all_start_landscape_scaled)  
# # #     
# # # 
# # #   
# # #   
# # 
# # 
# # # Scale occupancy to 10km parcel scale  ------------------
# # 
# # #scale birds from 100m points to 10km2 parcel scale (total landscape is) - apply conversion factor for going from 1 point to 10km2
# # #max occupancy is now bird_CF, if birds are present on all points in landscape.
# # # Treat restored forest seperately, as we assume particular impacts in restored forest  
# # 
# # #---!!!!!----COME BACK TO
# # birds_1 <- birds %>% filter(!habitat == "restored") %>%  
# #   mutate(parcel_occ = occ*bird_CF, 
# #          parcel_occ_min = occ_lwr*bird_CF, 
# #          parcel_occ_max = occ_upr*bird_CF)
# # 
# # #for birds in restored forest we assume:
# # #1. That 28% of the forest is left as restored
# # #2  That 72% of the forest is twice-logged 
# # L2 <- 0.72
# # R <- 0.28
# # 
# # portion_2L <- birds %>% filter(habitat == "twice-logged") %>%  
# #   mutate(parcel_occ = occ*L2, 
# #          parcel_occ_min = occ_lwr*L2, 
# #          parcel_occ_max = occ_upr*L2)
# # 
# # portion_R <- birds %>% filter(habitat == "restored") %>%  
# #   mutate(parcel_occ = occ*R, 
# #          parcel_occ_min = occ_lwr*R, 
# #          parcel_occ_max = occ_upr*R)
# # 
# # #combine 2L and R portions 
# # birds_restored <- rbind(portion_2L, portion_R) %>%  group_by(species,functionalhabAge) %>% 
# #   mutate(parcel_occ = sum(parcel_occ), 
# #          parcel_occ_min = sum(parcel_occ_min), 
# #          parcel_occ_max = sum(parcel_occ_max)) %>% filter(habitat == "restored")
# # 
# # #scale up to 10km2   
# # birds_restored <- birds_restored %>%
# #   mutate(parcel_occ = parcel_occ*bird_CF,  
# #          parcel_occ_min = parcel_occ_min*bird_CF, 
# #          parcel_occ_max = parcel_occ_max*bird_CF)
# # 
# # 
# # #put restored forest back in 
# # 
# # birds_10km2 <- rbind(birds_1, birds_restored)
# # 
# # # --------------  save parcel scale bird occupancy outputs ---------- 
# # #save bird_10km2_parcels data and temporal scenarios
# # write.csv(birds_10km2, "R_code/AssessBiodiversityOutcomes/Outputs/birds10km2_Occupancy_perYear_all.csv")
# # 
# # 
# # 
# # #CAN START CODE FROM HERE ####
# # #----- run occupancy calculations for #birds scenarios -----
# # 
# # rm(list = ls())
# # setwd("C:/Users/Gianluca Cerullo/OneDrive - University of Cambridge/PhD/Chapter_4_Borneo/CompleteFolder")
# # 
# # #set a cap for relative occupancy so that the scenario occupancy of a species cannot be more than CAP
# # #times the occupancy of the starting landscape
# # cap <- 1.5
# # 
# # hab_by_year <- read.csv("Tables/HabByYears.csv", strip.white = TRUE) %>%  
# #   rename(true_year = year, 
# #          functionalhabAge = functional_habAge, 
# #          habitat = transition_habitat) %>% select(-X)
# # 
# # #read in bird occupancy per habitat type per 10km2 
# # birds_10km2 <- read.csv("R_code/AssessBiodiversityOutcomes/Outputs/birds10km2_Occupancy_perYear_all.csv") %>% select(-X)
# # #remove eucalpyus and albizia improved if we don't need it 
# # birds_10km2 <- birds_10km2 %>% filter(!(habitat == "eucalyptus_improved"|habitat == "eucalyptus_improved"))
# # 
# # #read in the scenario parametres containing conversion factors for converting from point to parcel/entire landscape  
# # source('R_code/BuildScenarios/BiolerCodeScenarioConstruction.R')
# # 
# # 
# # #get the csv file name for each scenario 
# # csv_files <- list.files(csv_folder, pattern = "*.csv", full.names = TRUE)
# # 
# # 
# # #---check how many scenarios we have ------
# # # scenarioSums <- lapply(csv_files, function(file) {
# # #      df <- read.csv(file)  # You can use read.csv or any other appropriate function
# # #      return(df)
# # #    })
# # # scenarioSumdf <- rbindlist(scenarioSums)
# # # #how many scenarios do I have, and how are they arranged?
# # # scenarioSumdf %>% select(production_target,index,scenarioName) %>% unique() %>% count()
# # # X <- scenario_composition %>% select(production_target,index,scenarioName) %>%
# # #   group_by(production_target,scenarioName) %>% count()
# # # 
# # # 
# # 
# # #---
# # # csv_files_unprc <- read.csv(csv_files[1])
# # # csv_files_proc <- list.files(final_output_folder, pattern = "*.csv", full.names = TRUE)
# # # csv_files_proc <- read.csv(csv_files_proc[1])
# # # 
# # # unproc_ind <- csv_files_unprc %>% select(index) %>% unique %>% cbind(X = 2)
# # # proc_ind <- csv_files_proc %>% select(index) %>% unique  
# # # matched <-  proc_ind %>% left_join(unproc_ind, by = "index")
# # # matched %>% filter(X == 2) %>% count
# # #---
# # #define all starting landscapes: 
# # all_start_landscape
# # 
# # print(final_output_folder)
# # 
# # 
# # # --------calculate time-averaged landscape occupancy per scenario -----
# # 
# # #build a vector of bird species 
# # #bird_list <- birds_10km2 %>% select(species) %>% unique
# # #bird_vector <- bird_list$species
# # 
# # total_landscape_pts <- bird_CF*1000 ##total number of points in the landscape? 
# # 
# # #DEFINE DELAY FILTER ####
# # #(we have to subset only a few delay schedules to improve computational efficiency)
# # #delay filter (availalbe 0-29 in 1 year increments basically allow each scenario to be delayed in its first conversion by the delay filter)
# # #delayFilters <- c("delay 0", "delay 15", "delay 29")
# # #delayFilters <- c("delay 0", "delay 29")
# # delayFilters <- c("delay 0")
# # 
# # harvest_window <-  length(delayFilters)##how many harvest delays?
# # 
# # #this allows table joins to be 100 million rows (instead of 50,000,000 )
# # options(datatable.cautious = 100e6)
# # 
# # # Convert the data frames to data.tables as this enables us to process faster 
# # birds_10km2 <- as.data.table(birds_10km2)
# # total_landscape_pts <- bird_CF*1000
# # #scenarios <- lapply(scenarios, as.data.table)
# # 
# # # ---- define functions used to calculate landscape occupancy ------ 
# # 
# # #1. This function filter a subset of delay years (0,15,29) so that we spend less computer time 
# # filtDelay <- function(x){
# #   x <- x[
# #     harvest_delay %in% c(delayFilters) # filter harvest delatys 
# #   ]
# #   return(x)
# # }
# # 
# # # Define a function for calculating landscape occupancy using data.table syntax
# # #2. This functions calculate for each species and year the occupancy in a given scenarios landscape 
# # landscape_occ <- function(x) {
# #   x[, `:=`(
# #     #1. calculate the occupancy of each habitat type across the landscape 
# #     ##assuming 1/NUM-of-different-harvest-delays of of each habitat type is applied to each harvesting delay schedule
# #     parcel_occ_stag = (parcel_occ * num_parcels) / harvest_window,
# #     parcel_occ_min_stag = (parcel_occ_min * num_parcels) / harvest_window,
# #     parcel_occ_max_stag = (parcel_occ_max * num_parcels) / harvest_window
# #   )]
# #   
# #   x[, `:=`(
# #     #2. for each SPECIFIC true year and habitat transition, calculate occupancy combined across the staggered
# #     #harvesting schedule (i.e. the occupancy in a given habitat transition for a given year) 
# #     #(in data.table group_by equivalnet is written after the occ_hab year calculation, as below )
# #     occ_hab_year = sum(parcel_occ_stag),
# #     occ_hab_year_lwr = sum(parcel_occ_min_stag),
# #     occ_hab_year_upr = sum(parcel_occ_max_stag)                             #!!!!!!
# #   ), by = .(species, index, production_target, original_habitat, true_year,habitat)]
# #   
# #   #3. #select a single harvest delay worth of data, as we now have calculated occupancy across harvesting schedules
# #   x <- x[harvest_delay == "delay 0"]
# #   
# #   #4. Across habitat type transitions (e.g for ALL hab_parcel transitions) in a scenario, calculate occupancy for a given year
# #   x[, `:=`(
# #     scen_occ_year = sum(occ_hab_year),
# #     scen_occ_year_lwr = sum(occ_hab_year_lwr),
# #     scen_occ_year_upr = sum(occ_hab_year_upr)
# #   ), by = .(species,index, production_target, true_year)]
# #   
# #   #5.  #now we make sure we only have one row for each scenario and year and species, showing scen_occ_year
# #   #Select columns we want 
# #   x <- x[, .(species, index, production_target, scenarioName, scenarioStart, true_year,
# #              scen_occ_year, scen_occ_year_lwr, scen_occ_year_upr), by = .(true_year, index,species)]
# #   #this slice one row for each true_year and index and species
# #   x <- x[, .SD[1], by = .(species,true_year, index)]
# #   
# #   #rescale occupancy per species between 0-1 by dividing by the total number of points in the landscape(~319,000) 
# #  
# #   x[, `:=`(
# #     occ1_0 = scen_occ_year / total_landscape_pts,
# #     occ1_0_min = scen_occ_year_lwr / total_landscape_pts,
# #     occ1_0_max = scen_occ_year_upr / total_landscape_pts
# #   )]
# #   
# #   return(x)
# # }
# # #see landcape_occ_tidyverse format at bottom of code if needed
# # 
# # #3. This function calculate occupancy over sixty years, by summing occupancy for a given scenario across the 60 years
# # timed_occ <- function(x) {
# #   # Convert x to a data.table
# #   x <- as.data.table(x)
# #   
# #   # Group by specified columns using data.table syntax
# #   x <- x[, .(occ_60yr = sum(occ1_0),
# #              #-----------COME BACK TO !!!!THIS IS PROBABLY AN INCORRECT WAY OF SUMMING ERROR -----------------          
# #              
# #                   occ_60yr_lwr = sum(occ1_0_min),
# #                   occ_60yr_upr = sum(occ1_0_max)),
# #               by = .(species, index, production_target, scenarioStart, scenarioName)]
# #   
# #   #Select columns we want 
# #   x <- x[, .(species, index, production_target, scenarioName, scenarioStart,
# #              occ_60yr, occ_60yr_lwr, occ_60yr_upr), by = .(index,production_target,species)]
# #   #this slice one row for time-averaged occupancy  and index and species
# #   x <- x[, .SD[1], by = .(species,production_target, index)]
# #   
# #   return(x)
# # }
# # 
# # 
# # #4. this function filters so that there are not production targets with more than X scenarios, to speed up computational efficiency
# # # Assuming you have a data.table 'test' with columns 'production_target' and 'index'
# # max_prod_filt <- 10 # define the max number of scenarios to filter per production target 
# # 
# # filter_scenarios <- function(x){
# #   ## ONLY FILTER A SUBSET OF SCENARIOS PER PRODUCTION TARGRT
# #   # Step 1: Group by production_target and select up to 100 unique indexes per group
# # selected_indexes <- x[, .(selected_index = unique(index)[1:min(max_prod_filt, .N)]), by = production_target]
# # # Step 2: Filter the original data using the selected indexes
# # x <- x[index %in% selected_indexes$selected_index] 
# #  ## ONLY FILTER A SUBSET OF PRODUCTION TARGETS
# # filtered_targets <- selected_indexes[production_target %in% seq(0, 1, by = 0.05), production_target]
# # x <- x[index %in% selected_indexes$selected_index & production_target %in% filtered_targets]
# # x
# # }
# # 
# # #RUN ONCE####
# # # --CALCULATE TIME-AVERAGED OCCUPANCY FOR EACH SCENARIO -------
# # ##takes 30 mins for 1-delay period
# # #code takes a csv file for a scenario, joins bird data and processes time-averaged biodiversity,
# # #which it then stores as a csv file - RUN ONCE 
# # #nb - need to rerun for different delay periods
# # 
# # 
# # for (csv_file_path  in csv_files) {
# #   
# #     # Get a single  scenario 
# #    # scenario <- read.csv("R_code/AssessBiodiversityOutcomes/Outputs/scenariosForBirdsToBatch/scenario15.csv")  # Corrected syntax
# #    scenario <- read.csv(csv_file_path)
# #    #make scenario data table format
# #    scenario <- as.data.table(scenario)
# #    
# #    #filter so that there are no more than 100 scenarios per production target 
# #    scenario <- filter_scenarios(scenario)
# #   #scenario <- scenarios[[i]]
# #  
# #     
# #     #filter subset of harvest delays
# #     scenario <- filtDelay(scenario)
# #     
# #     # #set the join keys to match on for merge 
# #     # setkeyv(birds_10km2, c("habitat", "functionalhabAge"))
# #     # setkeyv(scenario, c("functional_habitat", "functionalhabAge"))
# #     
# #     
# #    
# #     # # Join that scenarios (based on above set join keys) to bird occupancy data
# #      scen_bio <- scenario[birds_10km2,
# #                           on = .(functional_habitat == habitat,
# #                          functionalhabAge ==  functionalhabAge),
# #                             nomatch = NA,
# #                          allow.cartesian=TRUE]
# #  
# #                                 
# #     # on = c("functional_habitat" = "habitat", "functionalhabAge" = "functionalhabAge"), 
# #                                #  nomatch = NA]#, #if not exact match, don't join
# #                         #  allow.cartesian=TRUE] #allows "many-to-many" join
# #     
# #     # Reset keys (to remove grouping)
# #      setkey(scen_bio, NULL)
# #     
# #     # Apply the landscape_occ function to calculate the landscape-wide occupancy of that scenario 
# #      #this shows occupancy between 0-1 over the entire scenario landscape, for each species and year (0-60)
# #      scen_bio_processed <- landscape_occ(scen_bio)
# #      
# #      #remove chunky scen_bio
# #      rm(scen_bio)
# #      
# #      #-----apply the timed_occ function----------
# #      # apply the timed_occ function to get occupancy for each scenario and species summed across 60years 
# #     time_scen_bio_processed <- timed_occ(scen_bio_processed)   #this bit sums occupancy across 60 years and returns one row per scenario for time-averaged occupancy
# #     # Store the processed data in the outcomes list            #REMVOVE if you want annual occupancy per species per year per scenario 
# # 
# #     #store the time-averaged occupancy for each species and scenario in the outcomes empty list
# #      
# #     #  outcomes[[i]] <- time_scen_bio_processed
# #      
# #      #remove chunky time_scen_bio_processed
# #      #rm(time_scen_bio_processed)
# #     
# #     # Print a message indicating the completion of processing for the current scenario
# #     #cat("Scenario", i, "processed.\n")
# #     #cat("Species:", sp_bird, "Scenario", i, "processed.\n")
# #     
# #     #---  save the processed scenario as a csv ---
# #     
# #     # Define a new file name for the processed data
# #     processed_csv_file_name <- paste("processed_", basename(csv_file_path), sep = "")
# #     #make file path by combinning folder name and file name
# #     processed_csv_file_path <- file.path(final_output_folder, processed_csv_file_name)
# #     
# #     # Save the processed data as a new CSV file
# #     # ---- UNCOMMENT!!!! to run ---------
# #     #write.csv(time_scen_bio_processed, file = processed_csv_file_path, row.names = FALSE)
# #     
# #     # Remove the scenario data from memory
# #     rm(scenario_data)
# #     rm(processed_scenario_data)
# #     
# #   
# #     # Print a progress message
# #     cat("Processed CSV file", processed_csv_file_name, "saved.\n")
# #     
# #   }
# # 
# # #--------read back in occupancy calculated through time for each scenario (currently) -------------
# # 
# # #CAN START AGAIN HERE####
# # # Get a list of all CSV files in the folder
# # csv_files <- list.files(final_output_folder, pattern = "\\.csv$", full.names = TRUE)
# # 
# # #csv_files <- csv_files[1]
# # #make one master dataframe of all birf results
# # combined_df <- data.table()
# # 
# # # Loop through each CSV file and rbind into the combined dataframe
# # for (csv_file_path in csv_files) {
# #   # Read the CSV file
# #   csv_data <- fread(csv_file_path)
# #   
# #   # rbind the data to the combined dataframe
# #   combined_df <- rbind(combined_df, csv_data)
# # }
# # 
# # #calculate number of scenarios 
# # combined_df %>% select(production_target,index,scenarioName) %>% unique() %>% count()
# # X <- scenario_composition %>% select(production_target,index,scenarioName) %>%
# #   group_by(production_target,scenarioName) %>% count()
# # 
# # 
# # #----plot estimate and error --------
# # scenario_filters <- c("all_primary_CY_D.csv")
# # spp_filter <- combined_df %>% select(species) %>% unique %>%  slice(1:100) %>% as.vector()
# # #, "mostly_1L_CY_D.csv", "mostly_2L_CY_D.csv")
# # 
# # combined_df %>% filter(scenarioName %in% scenario_filters) %>% 
# #   filter(species %in% spp_filter$species) %>% 
# #   filter(production_target == 0.5) %>% 
# #   group_by(production_target, species) %>% slice(1) %>% ungroup() %>% 
# #   ggplot(aes(x = species, y = occ_60yr)) +
# #   geom_point(position = position_dodge(width = 0.7), size = 3) +
# #   geom_errorbar(aes(ymin = occ_60yr_lwr, ymax = occ_60yr_upr), width = 0.25, position = position_dodge(0.7)) +
# #   facet_wrap(~scenarioStart, scales = "free_x") +
# #   labs(x = "Species", y = "Occ_60yr") +
# #   theme_minimal()+
# #   # Flip x-axis labels 90 degrees
# #   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
# # 
# # 
# # 
# # 
# # # ------ calculate the time-averaged occupancy in the starting landscape ------
# # 
# # #add time
# # time_forSL <- hab_by_year %>% 
# #   select(original_habitat, true_year) %>% unique() %>%  
# #   rename(habitat = original_habitat, 
# #          functionalhabAge = true_year)
# # all_start_landscape <- all_start_landscape %>%
# #   left_join(time_forSL, by = "habitat", relationship = "many-to-many")
# # 
# # 
# # #add biodiversity
# # all_start_landscape_birds <- all_start_landscape %>% 
# #   left_join(birds_10km2, by = c("habitat","functionalhabAge"), relationship = "many-to-many")
# # 
# # #calculate time averaged biodiversity 
# # all_start_landscape_birds  <- all_start_landscape_birds %>%   #add columns to match function format
# #                                      mutate(index = scenarioStart, 
# #                                      production_target = 0,
# #                                      original_habitat = habitat, 
# #                                      true_year = functionalhabAge, 
# #                                      harvest_delay = "delay 0", 
# #                                      scenarioName = scenarioStart ) %>% 
# #   as.data.table() %>% 
# #   #calculate starting landscape occupancy per year and species
# #   landscape_occ() %>%  
# #   #calculate time-averaged occupancy 
# #   timed_occ() %>%  
# #   #rename to make clear this is occupancy for starting landscape 
# #   mutate(SLocc_60yr = occ_60yr, 
# #          SLocc_60yr_lwr = occ_60yr_lwr, 
# #          SLocc_60yr_upr = occ_60yr_upr) %>% 
# #   #remove unwanted columns 
# #   select(-c(index, scenarioName,occ_60yr,occ_60yr_lwr,occ_60yr_upr, production_target))  
# # 
# # #Join each scenario to the relevant starting landscape occupancy ####
# # combined_df <- combined_df[all_start_landscape_birds, on = .(species, scenarioStart)] %>% 
# #   #remove scenarios with producion target of zero 
# #   filter(!production_target == 0)
# # 
# # 
# # #-------plot 60yr occupancies ------
# # 
# # # Summarise WINNER, LOSER, INTERMEDIATE  -------------
# # 
# # #ADD IN IUCN CATEGORY! ####
# # 
# # #DO I NEED TO INCORPORTATE OCCUPANCY UNCERTAINTY INTO THIS CATEGORISATION?
# # #at the moment we are selecting losers so that if occupancy is higher at ANY age of logging, then 
# # #the species is not a loser. 
# # losers <- birds_10km2 %>%  group_by(species)  %>%  
# #   filter(parcel_occ == max(parcel_occ)) %>% 
# #   mutate(spp_category = case_when(habitat =="primary" ~"loser", TRUE ~ NA_character_)) %>% 
# #   filter(spp_category == "loser") %>% select(species,spp_category) %>% unique()
# # 
# # 
# # intermediates <- birds_10km2 %>%  group_by(species) %>% filter(parcel_occ == max(parcel_occ)) %>% 
# #   mutate(spp_category = case_when(habitat =="once-logged" ~"intermediate",
# #                                   habitat == "twice-logged" ~ "intermediate", 
# #                                   habitat == "restored" ~ "intermediate",
# #                                   TRUE ~ NA_character_)) %>% 
# #   filter(spp_category == "intermediate") %>% select(species,spp_category) %>% unique
# # 
# # 
# # winners <- birds_10km2 %>%
# #   group_by(species) %>%
# #   filter(parcel_occ == max(parcel_occ)) %>%
# #   mutate(spp_category = case_when(
# #     habitat == "albizia_current" ~ "winner",
# #     habitat == "eucalyptus_current" ~ "winner",
# #     TRUE ~ NA_character_
# #   )) %>% 
# #   filter(spp_category == "winner") %>% select(species,spp_category) %>% unique()
# # 
# # #spp categories 
# # winner_loser <- rbind(losers,intermediates,winners) %>% ungroup
# # 
# # #join species category information for each scenario
# # add_spp_inf <- function(x){
# #   x %>%  left_join(winner_loser, by = "species")
# # }
# # 
# # combined_df <- add_spp_inf(combined_df)
# # getwd()
# # tom <- combined_df %>% filter(spp_category == "loser")
# # #write.csv(tom, "CsvForTom.csv")
# # 
# # # ---------------  Calculate the  geometric mean with errors ----------------------- 
# # 
# # #1. calculate relative occupancy (occupancy in scenario landscape versus in starting landscape
# # #NB calculating delta_occupancy(the difference in time-averaged occupancy between starting landscape and scenario landscape) results in many negative values (as higher occ in scenrio landscape) which means you can't take geometric mean (as you can't log a negative value)
# # rel_occ <- function(x){
# #   x %>% 
# #     #calculate 
# #     mutate(rel_occ  = occ_60yr/ SLocc_60yr
# #            ) %>%  
# #     #APPLY CAP ON RELATIVE ABUNDANCE SO THAT IT CAN'T BE > THAN CAP TIMES STARTING LANDSCAPE OCCUPANCY
# #     mutate(rel_occ = ifelse(rel_occ > cap, cap, rel_occ)) %>%  
# #     
# #     #CALCULATE ERROR
# #     
# #     #THIS is the individual error for a specific species comparing starting landscape versus scenario landscape
# #     mutate(errorSL = sqrt(
# #       ( 
# #        #calcaulate total error for starting landscape for a given species 
# #         abs(SLocc_60yr - SLocc_60yr_upr)^2 + abs(SLocc_60yr - SLocc_60yr_lwr)^2) 
# #       )/2 
# #       ) %>%  
# #        #calcaulate total error for scenario landscape for a given species 
# #         mutate(errorScen = sqrt(
# #           (
# #           abs(occ_60yr - occ_60yr_upr)^2 + abs(occ_60yr - occ_60yr_lwr)^2)
# #        )/2
# #         )
# #   
# # }
# # 
# # combined_df <- rel_occ(combined_df)
# # 
# # #The geometric mean function incorporates two types of error:
# # #1. Species-level error, which is the error associated with calculating occupancy for the starting and scenario landscape for a given species
# # #2. Geometric mean error, which is the error from taking a mean over diferent species with different errors
# # 
# # #note; we use weighted geometric means as this means that we weight species where we are less sure of species-level occupancy less strongly
# # geom_means <- function(x){
# #   x %>% group_by(index, spp_category)  %>%  
# #     mutate(
# #       
# #       #calculate the geometric mean of relative occupancy (the difference in time-averaged occupancy between starting landscape and scenario landscape)
# #       #scenario geom mean and error 
# #       geom_mean = exp(mean(log(rel_occ))),
# #       
# #       #calculate nrows in geom_mean calculation (equivalent to num species in the subset)
# #       nrows_geom_mean = n(),
# #       
# #       #calculate the error of calculatinng spp error between starting landscpape and scenario landscape 
# #       sppError = sqrt(
# #         (errorScen^2+ errorSL^2)/2
# #         ),
# #    
# #      #calcluate arithmetic mean of relative occupancy 
# #       arith_mean = mean(rel_occ)
# #     
# #     #calculate the erorr from calculating geometric mean across species with different errors   
# #     ) %>% 
# #     mutate(
# #     # multSppsd = sd(rel_occ),
# #               geomError = geom_mean *   (
# #               (prod(sppError / geom_mean))
# #                ^(1/nrows_geom_mean)     )
# #           ) %>% 
# #     
# #     mutate(
# #     # Calculate the weighted geometric mean using the reciprocal of the square of the species-specific errors as weights
# #     weighted_geom_mean = exp(sum(log(rel_occ) / sppError^2) / sum(1 / sppError^2)),
# #     
# #     # Calculate the error from calculating weighted geometric mean across species
# #     weighted_geomError = weighted_geom_mean * (
# #       prod(sppError / weighted_geom_mean) ^ (1 / nrows_geom_mean)
# #     )
# #     ) %>% 
# #     
# #       #NEED TO INPUT HOW TO
# #      slice(1) %>%  
# #     dplyr::select(index, spp_category, geom_mean,arith_mean,scenarioName,production_target,
# #                   sppError,geomError, nrows_geom_mean,weighted_geom_mean,weighted_geomError)
# # }
# # geom_results <- geom_means(combined_df)
# # 
# # # --------- Summarise the composition of scenarios (proportion of forest) --------
# # 
# # 
# # #Calculate Prop OG in scenario 
# # 
# # #get the amount of hab in each starting landscape 
# # primaryInStart <- all_start_landscape %>% filter(habitat == "primary") %>%  
# #   rename(SL_primary_parcels = num_parcels) %>% dplyr::select(-habitat)
# # habInStart <- all_start_landscape %>% select(scenarioStart) %>% unique() %>% 
# #   mutate(originalOG = c(1,0.2,0.2,0.8,0.2,0.2), 
# #          original1L = c(0,0.8,0,0,0.6,0), 
# #          original2L = c(0,0,0.8,0,0,0.6))
# #   
# #                                                                           
# # #build a function that calculate proportion of remaining habitat 
# # #in each scenario 
# # 
# # prop_OG_fun <- function(x){
# #   
# #   #proportion of TOTAL landscape [1000 parcels] in different habitat type 
# #     x %>% group_by(index, production_target) %>% 
# #     #total OG
# #     mutate(propOG = sum(num_parcels[habitat == "primary"])/1000,
# #     propPlant = sum(num_parcels[habitat %in% c("eucalyptus_current", "albizia_current", "albizia_future","eucalyptus_future")])/1000,   
# #     #prop-1L in the scenario landscape
# #     prop1L = sum(num_parcels[habitat == "once-logged"])/1000,
# #     #proportion of 2-L in the scenario landscape
# #     prop2L = sum(num_parcels[habitat == "twice-logged"])/1000) %>%  
# #     
# #     #get starting landscape
# #     mutate(scenarioStart = scenarioName) %>% 
# #     mutate(scenarioStart = str_remove(scenarioStart, "_IY_ND.csv")) %>%
# #     mutate(scenarioStart = str_remove(scenarioStart, "_CY_ND.csv")) %>%
# #     mutate(scenarioStart = str_remove(scenarioStart, "_IY_D.csv")) %>%
# #     mutate(scenarioStart = str_remove(scenarioStart, "_CY_D.csv")) %>% 
# #     ungroup %>% 
# #   
# #     #get total amount of each habitat in STARTING landscape for a scenario
# #     left_join(habInStart, by = "scenarioStart") %>% 
# #      
# #     #calculate PROPORTION of REMAINING original habitat type 
# #    #(nb there can actually be more once-logged or twice-logged forest in scenario than scenarioStart, if primary forest is logged)
# #     mutate(remainingOG = propOG/originalOG, 
# #            remaining1L = prop1L/original1L, 
# #            remaining2L = prop2L/original2L) %>%  
# #     #correct for INF values for if dividing by 0
# #     mutate_at(vars(remainingOG, remaining1L, remaining2L), ~ ifelse(is.infinite(.) | is.nan(.), 0, .)) %>%
# #     
# #     select(index, production_target, scenarioName,scenarioStart,
# #            propOG, propPlant,prop1L,prop2L,
# #            remainingOG,remaining1L,remaining2L) %>% unique()
# #   
# # }
# # 
# # propOGcomp <- prop_OG_fun(scenario_composition) %>% ungroup
# # 
# # #for each scenario, add the proportion starting landscapes
# # propOGcomp_dt <- as.data.table(propOGcomp)
# # geom_results <- as.data.table(geom_results)
# # 
# # #if index is numeric make character
# # geom_results <- geom_results[, index := as.character(index)]
# # geom_results <- geom_results[, production_target := as.numeric(production_target)]
# # propOGcomp_dt <- propOGcomp_dt[, index := as.character(index)]
# # propOGcomp_dt <- propOGcomp_dt[, production_target := as.numeric(production_target)]
# # 
# # 
# # geom_results_df <- propOGcomp_dt[geom_results, on = .(index, production_target,scenarioName)] 
# # 
# # 
# # #---------- #BIVARIATE PLOTTING PARAMETRES --------------------
# # 
# # library(biscale)
# # COL <- "DkBlue2" # define colour pallete
# # COL <- "BlueOr"
# # #get colours for bivariate plotting
# # biv_pallete <- bi_pal(COL, dim =4 ) # for plotting
# # cols <- data.frame(bi_pal(COL, dim = 4, preview = FALSE))
# # colnames(cols) <- c("hex")
# # cols <- cols %>% mutate(bi_class = rownames(.))
# # 
# # textSize  <- 15
# # 
# # #make bivar legend
# # primary_legend <- bi_legend(pal = "BlueOr", dim = 4, 
# #           xlab = "Proportion old-growth", 
# #           ylab = "Proportion once-logged", size = textSize)
# # 
# # onceL_legend <- bi_legend(pal = "BlueOr", dim = 4, 
# #                             xlab = "Proportion remaining old-growth", 
# #                             ylab = "Proportion remainng once-logged",size = textSize)
# # 
# # twiceL_legend <- bi_legend(pal = "BlueOr", dim = 4, 
# #                            xlab = "Proportion remaining old-growth", 
# #                            ylab = "Proportion remainng twice-logged",size = textSize)
# # 
# # all_legend <- plot_grid(primary_legend,onceL_legend,twiceL_legend, ncol =3)
# # 
# # #assign scenarios the colours from the bivariate plot for primary start
# # bivariate_colours_PRIM <- function(X){
# #   X %>%  bi_class(x = propOG, y = prop1L, dim = 4, style = "equal") %>%  
# #     left_join(cols, by = "bi_class") # add hex colours
# # }
# # geom_results_df<- bivariate_colours_PRIM(geom_results_df) %>% rename(hexP = hex)
# # 
# # #assign scenarios the colours from the bivariate plot for mostly 1L start
# # bivariate_colours_1L <- function(X){
# #   X %>%  bi_class(x = remainingOG, y = remaining1L, dim = 4, style = "equal") %>%  
# #     left_join(cols, by = "bi_class") # add hex colours
# # }
# # geom_results_df<- bivariate_colours_1L(geom_results_df) %>% rename(hex1L = hex)
# # 
# # #assign scenarios the colours from the bivariate plot for mostly 2L start
# # bivariate_colours_2L <- function(X){
# #   X %>%  bi_class(x = remainingOG, y = remaining2L, dim = 4, style = "equal") %>%  
# #     left_join(cols, by = "bi_class") # add hex colours
# # }
# # geom_results_df<- bivariate_colours_2L(geom_results_df) %>% rename(hex2L = hex)
# # #hex shows colours for 1L vs primary.
# # #hex_2L shows colours for 2L vs primary
# # 
# # 
# # # final_carbon_df_4DR <- bi_class(final_carbon_df_4DR, x = propOriginalOG, y = prop1L, dim = 4, style = "equal") %>%  
# # #   left_join(cols, by = "bi_class") # add hex colours
# # 
# # #================= build some summary plts ==========================================================
# # 
# # #filter by category
# # legend_plot <-  geom_results_df %>% filter(spp_category == "loser" & scenarioName == "all_primary_CY_D.csv") 
# # losers <- geom_results_df %>% filter(spp_category == "loser") 
# # intermediate <- geom_results_df %>% filter(spp_category == "intermediate") 
# # winners <-  geom_results_df %>% filter(spp_category == "winner") 
# # 
# # scenario_filters <- c("all_primary_CY_D.csv", "mostly_1L_CY_D.csv", "mostly_2L_CY_D.csv")
# # 
# # 
# # 
# # #build PLOTTING FUNCTION #### 
# # plot_fun <- function(x){
# #   
# #   x <- x %>%
# #     filter(scenarioName %in% scenario_filters)
# #   
# #   #if scenario contains plantation add cross 
# #   x <- x %>%
# #     mutate(is_cross = ifelse(propPlant > 0, "Cross", "Point"))
# #   
# #   
# #   max_propPlant <- max(x$propPlant, na.rm = TRUE)
# #   
# #   
# #   #reorder facet order 
# #   
# #   x$scenarioName <- factor(x$scenarioName, levels = c(
# #     "all_primary_CY_D.csv",
# #     #"all_primary_CY_ND.csv","all_primary_IY_D.csv", "all_primary_IY_ND.csv",
# #     "mostly_1L_CY_D.csv",
# #     #"mostly_1L_CY_ND.csv", "mostly_1L_IY_D.csv", "mostly_1L_IY_ND.csv",
# #     #"mostly_1L_deforested_CY_D.csv", "mostly_1L_deforested_CY_ND.csv", "mostly_1L_deforested_IY_D.csv", "mostly_1L_deforested_IY_ND.csv", 
# #     "mostly_2L_CY_D.csv"))
# #   #"mostly_2L_CY_ND.csv", "mostly_2L_IY_D.csv", "mostly_2L_IY_ND.csv",
# #   #"mostly_2L_deforested_CY_D.csv", "mostly_2L_deforested_CY_ND.csv", "mostly_2L_deforested_IY_D.csv","mostly_2L_deforested_IY_ND.csv",
# #   #"primary_deforested_CY_D.csv", "primary_deforested_CY_ND.csv", "primary_deforested_IY_D.csv","primary_deforested_IY_ND.csv"))
# #   
# # 
# #   x %>%  ggplot(aes(x = production_target, y = weighted_geom_mean))+
# #     # conditionally colour so that if we plot bivariate between proportion of primary and proportion of least logged (either 1L or 2L depending on starting landscape) in the scenario    
# #     geom_point(aes(
# #       x = production_target,
# #       y = weighted_geom_mean,
# #       colour = case_when(
# #         scenarioStart %in% c("all_primary", "primary_deforested") ~ hexP,
# #         scenarioStart %in% c("mostly_1L", "mostly_1L_deforested") ~ hex1L,
# #         scenarioStart %in% c("mostly_2L", "mostly_2L_deforested") ~ hex2L
# #       ),
# #       shape = is_cross, 
# #     ), position = position_jitter(width = 0.05, height = -0.03)) +
# #     scale_colour_identity()+
# #     #GIVE CORSSES TO PLANTATION CONTAINING SCENAIOS ####
# #     scale_shape_manual(values = c("Point" = 19, "Cross" = 3)) + # Define shape mapping
# #     #scale_size_continuous(range = c(3, 8), breaks = seq(0, max_propPlant, by = 0.05)) + # Adjust size range and breaks
# #     
# #     
# #     xlim(0, 1)+
# #     xlab("Production target")+
# #     ylab("Weighted geometric mean change")+
# #     
# #     #labs(colour = "Proportion of remaining old-growth forest spared")+
# #     # labs(colour = "Proportion of plantation in remaining landscape")+
# #     
# #     facet_wrap(~scenarioName, ncol = 4)+
# #     #   geom_hline(aes(yintercept = SL_geom_mean))+
# #     theme_bw(base_size = textSize)+
# #     theme(legend.position = "none")
# #   
# # }
# # 
# # 
# # #get legend
# # legend_plot <- plot_fun(legend_plot)
# # legend <- get_legend(legend_plot + theme(legend.position = "bottom",         # c(0.5, 0.15),
# #                                          legend.spacing.x = unit(1.0, 'cm'),
# #                                          legend.title  = element_text(size  = 30, face = "bold"))) 
# # #plot figures (without legends)
# # losers <- plot_fun(losers)
# # intermediate <- plot_fun(intermediate)
# # winners <- plot_fun(winners)
# # 
# # #add legend function
# # add_legend <-  function(x){
# #   plot_grid(x, legend, 
# #             nrow =2 , ncol = 1,
# #             rel_heights = c(1, 0.1))
# # } 
# # 
# # #plot final figs for each above-defined category 
# # add_legend(losers)
# # plot_grid(losers, all_legend, nrow =2)
# # add_legend(intermediate)
# # plot_grid(intermediate, all_legend, nrow =2)
# # add_legend(winners)
# # plot_grid(winners, all_legend, nrow =2)
# # 
# # 
# # 
# # 
# # #========== unused code ================
# # 
# # 
# # 
# # # 
# # # 
# # # 
# # # #build plotting function
# # # plot_fun <- function(x){
# # #   
# # #   #reorder facet order 
# # #   
# # #   x$scenarioName <- factor(x$scenarioName, levels = c(
# # #     "all_primary_CY_D.csv","all_primary_CY_ND.csv","all_primary_IY_D.csv", "all_primary_IY_ND.csv",
# # #     "mostly_1L_CY_D.csv", "mostly_1L_CY_ND.csv", "mostly_1L_IY_D.csv", "mostly_1L_IY_ND.csv",
# # #     "mostly_1L_deforested_CY_D.csv", "mostly_1L_deforested_CY_ND.csv", "mostly_1L_deforested_IY_D.csv", "mostly_1L_deforested_IY_ND.csv", 
# # #     "mostly_2L_CY_D.csv", "mostly_2L_CY_ND.csv", "mostly_2L_IY_D.csv", "mostly_2L_IY_ND.csv",
# # #     "mostly_2L_deforested_CY_D.csv", "mostly_2L_deforested_CY_ND.csv", "mostly_2L_deforested_IY_D.csv","mostly_2L_deforested_IY_ND.csv",
# # #     "primary_deforested_CY_D.csv", "primary_deforested_CY_ND.csv", "primary_deforested_IY_D.csv","primary_deforested_IY_ND.csv"))
# # #   
# # #   
# # #   x %>%  ggplot(aes(x = production_target, y = geom_mean))+
# # #     #colour by proportion of remaining old-growth forest 
# # # 
# # #    #geom_point(aes(x= production_target, y = geom_mean, colour = propOriginalOG), position = position_jitter(width = 0.05, height = 0.01)) + 
# # #     geom_point(aes(x= production_target, y = geom_mean, colour = hex), position = position_jitter(width = 0.05, height = 0.01)) + 
# # #     scale_colour_identity()+
# # #     #colour by proportion of plantation in end scenarios
# # #     #geom_point(aes(x= production_target, y = geom_mean, colour = propPlant)) + 
# # #     
# # #     # scale_colour_gradient(low = '#fee090', high = '#d73027',
# # #     #                       breaks =c(0,1), 
# # #     #                       limits=c(0,1),#only show 0 and 4000 (parcels of legend
# # #     #                       labels = c("0", "1"))+   #display as a percentage o concession 
# # #     #xlim(0, 0.5)+
# # #     xlim(0, 1)+
# # #     labs(colour = "Proportion of remaining old-growth forest spared")+
# # #     # labs(colour = "Proportion of plantation in remaining landscape")+
# # #     
# # #     facet_wrap(~scenarioName, ncol = 4, scales = "free")+
# # #  #   geom_hline(aes(yintercept = SL_geom_mean))+
# # #     theme_bw()+
# # #     theme(legend.position = "none")
# # # }
# # # 
# # # #get legend
# # # legend_plot <- plot_fun(legend_plot)
# # # legend <- get_legend(legend_plot + theme(legend.position = "bottom",         # c(0.5, 0.15),
# # #                                          legend.spacing.x = unit(1.0, 'cm'),
# # #                                          legend.title  = element_text(size  = 30, face = "bold"))) 
# # # #plot figures (without legends)
# # # losers <- plot_fun(losers)
# # # intermediate <- plot_fun(intermediate)
# # # winners <- plot_fun(winners)
# # # 
# # # #add legend function
# # # add_legend <-  function(x){
# # #   plot_grid(x, legend, 
# # #             nrow =2 , ncol = 1,
# # #             rel_heights = c(1, 0.1))
# # # } 
# # # 
# # # 
# # # #plot final figs for each above-defined category 
# # # add_legend(losers)
# # # add_legend(intermediate)
# # # add_legend(winners)
# # # 
# # # 
# # # #plot prop plantation against prop 1L 
# # # geom_results_df %>% plotI
# # # 
# # # #quick checks
# # # x <- geom_results_df %>%  filter(scenarioName == "mostly_1L_deforested_IY_D.csv") %>% filter(spp_category == "loser") %>% 
# # #   filter(production_target == 0.48) %>% 
# # #   #slice_max(geom_mean) %>% 
# # #   select(index, production_target,geom_mean) %>% 
# # #   #get compositon  of scenarios 
# # #   left_join(scenario_composition, by = c("index", "production_target"))
# # # 
# # # #NOTES: 
# # # #1. There is a problem with mostly 1L- more than 800 parcels of once-logged converted in some scenarios 
# # # #2. There is also a problem with mostly 1L and deforested 
# # # 
# # # #===============================
# # # #UNUSED CODE
# # # 
# # # # landscape_occ_tidyverFORMAT <- function(x){
# # # #     x %>% 
# # # #       #1. calculate the occupancy of each habitat type across the landscape 
# # # #       ##assuming 1/NUM-of-different-harvest-delays of of each habitat type is applied to each harvesting delay schedule
# # # #       mutate(parcel_occ_stag = (parcel_occ*num_parcels)/harvest_window, 
# # # #              parcel_occ_min_stag =  (parcel_occ_min*num_parcels)/harvest_window, 
# # # #              parcel_occ_max_stag = (parcel_occ_max*num_parcels)/harvest_window) %>%  
# # # #       
# # # #       #2. for each true year and habitat transition, calculate occupancy combined across the staggered
# # # #       #harvesting schedule (i.e. the occupancy in a given habitat transition for a given year) 
# # # #       #sum across habitat types for each scenario to give landscape-level occupancy
# # # #       group_by(species, index,production_target, original_habitat,true_year) %>% 
# # # #       mutate(occ_hab_year = sum(parcel_occ_stag), 
# # # #              occ_hab_year_lwr= sum(parcel_occ_min_stag),
# # # #              occ_hab_year_upr = sum(parcel_occ_max_stag)) %>%  
# # # #       ungroup() %>% 
# # # #       #3. #select a single harvest delay worth of data, as we now have calculated occupancy across harvesting schedules
# # # #       filter(harvest_delay == "delay 0") %>% select(-harvest_delay) %>% 
# # # #       
# # # #       #4. Across habitat type transitions (e.g for all hab_parcel transitions) in a scenario, calculate occupancy for a given year
# # # #       group_by(species, index, production_target, true_year) %>%  
# # # #       mutate(scen_occ_year = sum(occ_hab_year), 
# # # #              scen_occ_year_lwr= sum(occ_hab_year_lwr), 
# # # #              scen_occ_year_upr = sum(occ_hab_year_upr)) %>%  ungroup() %>% 
# # # #       
# # # #       #5.  #now we make sure we only have one row for each scenario and year and species, showing scen_occ_year
# # # #       select(species, index, production_target,scenarioName,scenarioStart, true_year, 
# # # #              scen_occ_year,scen_occ_year_lwr, scen_occ_year_upr) %>%  
# # # #       group_by(true_year,species,index) %>% 
# # # #       slice(1) %>% 
# # # #       ungroup() %>% 
# # # #       #rescale occupancy per species between 0-1 by dividing by the total number of points in the landscape(~319,000) 
# # # #       mutate(occ1_0 = scen_occ_year/total_landscape_pts, 
# # # #              occ1_0_min= scen_occ_year_lwr/total_landscape_pts, 
# # # #              occ1_0_max= scen_occ_year_upr/total_landscape_pts)
# # # #     
# # # # }   

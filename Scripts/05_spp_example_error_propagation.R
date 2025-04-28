#This script takes the key steps out of functions for the AsssessScenarioOutcomes.R script, so 
#that we can see easily step-by-step the calculations we are doing and how these propagate error
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
library(bayestestR)


#read in the scenario parameters containing conversion factors for converting from point to parcel/entire landscape  
source("Inputs/ScenarioParams.R")

#this allows table joins to be 100 million rows (instead of 50,000,000 )
options(datatable.cautious = 100e6)
#INPUTS -----------------------------
processed_birds <- readRDS("Outputs/processedOccBirdsSept24.rds")

#remove improved improved yields
processed_birds <- as.data.table(processed_birds)
# Filter out rows where 'habitat' contains the string "improved" (case-insensitive)
processed_birds <- processed_birds[!grepl("improved", habitat, ignore.case = TRUE)]
# -----------------------------

# Step 1: Extract unique species and scenarios
species <- processed_birds %>% 
  select(species) %>% 
  unique() %>% 
  pull()

all_start_landscape_scaled <- all_start_landscape %>%
  mutate(num_points = num_parcels * bird_CF) %>% 
  select(-num_parcels) %>% 
  as.data.table()

unique_SL <- all_start_landscape_scaled %>% 
  select(scenarioStart) %>% 
  unique() %>% 
  as.vector()

unique_spp <- processed_birds %>% 
  select(species) %>% 
  unique() %>% 
  as.vector()

combinations_SL <- as.data.table(expand.grid(scenarioStart = unique_SL$scenarioStart, 
                                           species = unique_spp$species))

#--------------------------------------------
#TEST FOR STARTING LANDSCAPES 
#--------------------------------------------
# Step 2: Select a specific scenario and species
zeta <- 625 # Example index for testing
single_scenario_name <- combinations_SL$scenarioStart[zeta]
processed_birds_name <- as.character(combinations_SL$species[zeta])


print(paste("Processing scenario:", single_scenario_name, "with species:", processed_birds_name))

# Filter data for the specific scenario and species
single_scenario_i <- all_start_landscape_scaled[scenarioStart == single_scenario_name]
processed_birds_i <- processed_birds[species == processed_birds_name]

# Step 3: Join scenario and birds data 
bird_join <- processed_birds_i[single_scenario_i, on = .(habitat)] 
# bird_join_oneindex <- bird_join %>% filter(iteration == "draw_237")

# Step 4: Calculate habitat occupancy
# bird_join_oneindex[, hab_occ := occ * num_points]
bird_join[, hab_occ := occ * num_points]

# Step 5: Calculate landscape occupancy by year
# result_oneindex <- bird_join_oneindex[, .(landscape_occ = sum(hab_occ) / total_bird_pts), 
#                     by = .(species, scenarioStart, iteration, functionalhabAge)]
resultSL <- bird_join[, .(landscape_occ = sum(hab_occ) / total_bird_pts), 
                    by = .(species, scenarioStart, iteration, functionalhabAge)]

# Step 6: Calculate 60-year occupancy for each iteration and species
# result_oneindex <- result_oneindex[, .(occ_60yr = sum(landscape_occ)), 
#                  by = .(species, scenarioStart, iteration)]

resultSL_median <- resultSL[, .(occ_60yr = median(landscape_occ)), 
                 by = .(species, scenarioStart, iteration)]

resultSL_sum <- resultSL[, .(occ_60yr = sum(landscape_occ)), 
                     by = .(species, scenarioStart, iteration)]

#NOTE; look at the very high uncertainty in the starting landscape-level occupancy; it's huge...
# Calculate quantiles for median occupancy data
lower_quantile_median <- quantile(resultSL_median$occ_60yr, 0.25) # 25th percentile
upper_quantile_median <- quantile(resultSL_median$occ_60yr, 0.75) # 75th percentile

# Calculate quantiles for sum occupancy data
lower_quantile_sum <- quantile(resultSL_sum$occ_60yr, 0.25)
upper_quantile_sum <- quantile(resultSL_sum$occ_60yr, 0.75)

# Plot the histogram for the median occupancy with quantile lines
ggplot(resultSL_median, aes(x = occ_60yr)) +
  geom_histogram(bins = 60, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = lower_quantile_median, color = "red", linetype = "dashed", size = 1) + 
  geom_vline(xintercept = upper_quantile_median, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Histogram of 60-Year Occupancy (Median)", 
       x = "60-Year Occupancy", y = "Frequency") +
  theme_minimal()

# Plot the histogram for the sum occupancy with quantile lines
ggplot(resultSL_sum, aes(x = occ_60yr)) +
  geom_histogram(bins = 60, fill = "lightgreen", color = "black", alpha = 0.7) +
  geom_vline(xintercept = lower_quantile_sum, color = "blue", linetype = "dashed", size = 1) + 
  geom_vline(xintercept = upper_quantile_sum, color = "blue", linetype = "dashed", size = 1) +
  labs(title = "Histogram of 60-Year Occupancy (Sum)", 
       x = "60-Year Occupancy", y = "Frequency") +
  theme_minimal()


#--------------------------------------------
#TEST FOR SCENARIO LANDSCAPES 
#--------------------------------------------
#get the csv file name for each scenario 
csv_folder <- "Inputs/ScenariosWithDelaysCSVs"
csv_files <- list.files(csv_folder, pattern = "*.csv", full.names = TRUE)
delayFilters <- c("delay 0", "delay 29")
harvest_window <-  length(delayFilters)##how many harvest delays?

#select one single group of scenarios
k=1
scenario_group  <- read.csv(csv_files[[k]]) %>% as.data.table() %>% 
  mutate(num_points = num_parcels*bird_CF) %>%  
  filter(harvest_delay %in% delayFilters)

#calculate full set of combinatations of species and scenario
#get unique scenarios asn species 
unique_index <- scenario_group %>% select(index) %>% unique() %>% as.vector()
unique_spp <- processed_birds %>% select(species) %>%  unique() %>% as.vector()
combinations <- as.data.table(expand.grid(index = unique_index$index, species = unique_spp$species))

# Step 2: Select a specific scenario and species
combinations %>% filter(species == "Helmeted Hornbill")
zeta_index <- 1:507 # Example index for testing
zeta_sp <- 52729
single_scenario_name <- combinations$index[zeta_index]
processed_birds_name <- as.character(combinations$species[zeta_sp])

print(paste("Processing scenario:", single_scenario_name, "with species:", processed_birds_name))

# Filter data for the specific scenario and species
single_scenario_i <- scenario_group[index %in% single_scenario_name]
processed_birds_i <- processed_birds[species == processed_birds_name]


  # Step 1: Perform a left join and calculate hab_occ 
  # [join birds and scenarios and get num point for a given habitat type and stagger]
  
  bird_join <- merge(single_scenario_i, processed_birds_i, by.x = c("functional_habitat", "functionalhabAge"), 
                     by.y = c("habitat", "functionalhabAge"), all.x = TRUE, allow.cartesian = TRUE)
  bird_join[, hab_occ := occ * num_points]
  bird_join[, parcel_occ_stag := hab_occ / harvest_window]
  
  # Step 2: Group and summarize occupancy for specific year and habitat transition
  #[for each true year and habitat transition, calculate occupancy combined across the staggered
  # harvesting schedule (i.e. the occupancy in a given habitat transition for a given year)]
  
  result <- bird_join[, .(occ_hab_year = mean(parcel_occ_stag)), 
                      by = .(species, index, iteration, production_target, true_year, original_habitat, habitat)]
  
  # Step 3: Calculate landscape occupancy
  #[Across habitat type transitions (e.g for ALL hab_parcel transitions) in a scenario, calculate occupancy for a given year]
  result <- result[, .(landscape_occ = sum(occ_hab_year) / total_bird_pts), 
                   by = .(species, index, iteration, production_target, true_year)]
  
  # Step 4: Calculate occ_60yr for each iteration and species
  #[calculate occ60 for each iteration and species]
  result_median <- result[, .(occ_60yr = median(landscape_occ)), 
                   by = .(species, index, iteration, production_target)]

  result_sum <- result[, .(occ_60yr = sum(landscape_occ)), 
                          by = .(species, index, iteration, production_target)]
  
  #NOTE; look at the very high uncertainty in the scenario landscape-level occupancy; it's huge...
  hist(result_median$occ_60yr, breaks = 60) 
  hist(result_sum$occ_60yr,  breaks = 60)
  # Step 5: Summarise across posterior draws
  occyears_error_median <- result_median [, .(mean_60yr = mean(occ_60yr),
                        p1_medianRelativeOccupancy = quantile(occ_60yr, 0.25, na.rm = TRUE),
                        p9_medianRelativeOccupancy = quantile(occ_60yr, 0.75, na.rm = TRUE),
                        sd_60yr_error = sd(occ_60yr),
                        se_60yr_error = sd(occ_60yr) / sqrt(.N)),
                        by = .(index, species, production_target)]
  
  occyears_error_sum <- result_sum [, .(mean_60yr = mean(occ_60yr),
                                       p1_medianRelativeOccupancy = quantile(occ_60yr, 0.25, na.rm = TRUE),
                                       p9_medianRelativeOccupancy = quantile(occ_60yr, 0.75, na.rm = TRUE),
                                       sd_60yr_error = sd(occ_60yr),
                                       se_60yr_error = sd(occ_60yr) / sqrt(.N)),
                                   by = .(index, species, production_target)]
#plots
  
  # Plot the histogram for the median occupancy with quantile lines
  ggplot(resultSL_median, aes(x = occ_60yr)) +
    geom_histogram(bins = 60, fill = "skyblue", color = "black", alpha = 0.7) +
    geom_vline(xintercept = lower_quantile_median, color = "red", linetype = "dashed", size = 1) + 
    geom_vline(xintercept = upper_quantile_median, color = "red", linetype = "dashed", size = 1) +
    labs(title = "Histogram of 60-Year Occupancy (Median)", 
         x = "60-Year Occupancy", y = "Frequency") +
    theme_minimal()
  
  # Plot the histogram for the sum occupancy with quantile lines
  ggplot(resultSL_sum, aes(x = occ_60yr)) +
    geom_histogram(bins = 60, fill = "lightgreen", color = "black", alpha = 0.7) +
    geom_vline(xintercept = lower_quantile_sum, color = "blue", linetype = "dashed", size = 1) + 
    geom_vline(xintercept = upper_quantile_sum, color = "blue", linetype = "dashed", size = 1) +
    labs(title = "Histogram of 60-Year Occupancy (Sum)", 
         x = "60-Year Occupancy", y = "Frequency") +
    theme_minimal()
  
occyears_error_sum %>% 
  group_by(index) %>% 
    ggplot(aes(x = production_target, y = mean_60yr, colour = index)) +
    geom_point()+
    
    # Error bars with matching jitter and reduced prominence
    geom_errorbar(aes(
      ymin = p1_medianRelativeOccupancy,
      ymax = p9_medianRelativeOccupancy
    ),
    width = 0.02, # Horizontal cap width
    alpha = 0.8, # Fainter error bars
    size = 0.5 # Thinner error bars
    ) + 
  theme_bw()+ 
  theme(legend.position = 'none')#TEST THIS FOR MULTIPLE SCENARIOS+



occyears_error_median %>% 
  group_by(index) %>% 
  ggplot(aes(x = production_target, y = mean_60yr, colour = index)) +
  geom_point()+
  
  # Error bars with matching jitter and reduced prominence
  geom_errorbar(aes(
    ymin = p1_medianRelativeOccupancy,
    ymax = p9_medianRelativeOccupancy
  ),
  width = 0.02, # Horizontal cap width
  alpha = 0.8, # Fainter error bars
  size = 0.5 # Thinner error bars
  ) +
  theme_bw()+ 
  theme(legend.position = 'none')#TEST THIS FOR MULTIPLE SCENARIOS+

#---------------------------------------------------------------------
#Add scenario composition 
#----------------------------------------------------------------------

#yield-matched scenarios
scenarios <- readRDS("Inputs/MasterAllScenarios.rds")
scenario_composition <- rbindlist(scenarios, use.names=TRUE) %>%  
  select(index, production_target, num_parcels, habitat, original_habitat)# get scenario composition
rm(scenarios)



#---------------------------------------------------------------------------------
#Now summarise scenario/SL rel occ #####
#---------------------------------------------------------------------------------
 
#use time-averaged 
cap <- 1.5 # don't allow scenario occ to be more than 1.5 starting landscape occ [only used if calculating geometric mean]

  #add starting landscape to scenario
  scenarioStart <- resultSL_median %>% rename(SL_occ_60yr = occ_60yr) 

  #add in the SL occupancy for a single scenario 
  occ_comb <- result_median[scenarioStart, on = .(species, iteration), nomatch = 0]
  
  # #calculate rel_occ; if rel_occ is > cap, replace with cap, to ensure scenario landscape cannot be more than 1.5 of starting landscape
  occ_comb[, rel_occ := pmin((occ_60yr / SL_occ_60yr), cap)]
  
  rel_occ_median <- occ_comb %>%  group_by(species, index, production_target) %>%  
    summarize(
      medianRelativeOccupancy = median(rel_occ, na.rm = TRUE),
      meanRelativeOccupancy = mean(rel_occ, na.rm = TRUE),
      p1_medianRelativeOccupancy = quantile(rel_occ, 0.25, na.rm = TRUE),
      p9_medianRelativeOccupancy = quantile(rel_occ, 0.75, na.rm = TRUE), 
      sd_60yr_error = sd(occ_60yr))

#use time-SUMMED 
  
  cap <- 1.5 # don't allow scenario occ to be more than 1.5 starting landscape occ [only used if calculating geometric mean]
  
  #add starting landscape to scenario
  scenarioStart <- resultSL_sum %>% rename(SL_occ_60yr = occ_60yr)
  
  #add in the SL occupancy for a single scenario 
  occ_comb <- result_sum[scenarioStart, on = .(species, iteration), nomatch = 0]
  
  # #calculate rel_occ; if rel_occ is > cap, replace with cap, to ensure scenario landscape cannot be more than 1.5 of starting landscape
  occ_comb[, rel_occ := pmin((occ_60yr / SL_occ_60yr), cap)]
  
  rel_occ_sum <- occ_comb %>%  group_by(species, index, production_target) %>%  
    summarize(
      medianRelativeOccupancy = median(rel_occ, na.rm = TRUE),
      meanRelativeOccupancy = mean(rel_occ, na.rm = TRUE),
      p1_medianRelativeOccupancy = quantile(rel_occ, 0.25, na.rm = TRUE),
      p9_medianRelativeOccupancy = quantile(rel_occ, 0.75, na.rm = TRUE), 
      # Calculate HPD intervals (95% by default)
      hpd_95_lower = hdi(rel_occ, ci = 0.7)$CI_low,
      hpd_95_upr = hdi(rel_occ, ci = 0.7)$CI_high,
      hpd_50_lower = hdi(rel_occ, ci = 0.5)$CI_low,
      hpd_50_upr = hdi(rel_occ, ci = 0.5)$CI_high,
      sd_60yr_error = sd(occ_60yr))

#Which scenarios have the most uncertainty for helmeted hornbill? 
  
  #BIG ONCE-LOG ERROR FOR HH
biggest_error <-   rel_occ_sum %>% 
    # Add information about scenario composition
    left_join(scenario_composition) %>%
    #filter only scenarios that can once-log
    filter(production_target <0.75) %>% 
    # Add information on proportion of plantation
    mutate(propPlant = sum(num_parcels[habitat %in% c("eucalyptus_current", "albizia_current", "albizia_future", "eucalyptus_future")]) / 1000) %>%
    group_by(production_target) %>% 
    # Create a new column for the difference between p1 and p9
    mutate(diff_p1_p9 = abs(p9_medianRelativeOccupancy - p1_medianRelativeOccupancy)) %>%
    # Filter for the scenario with the largest difference within each index group
    filter(diff_p1_p9 == max(diff_p1_p9))
  
  #plot
rel_occ_sum %>% 
    # Add information about scenario composition
    left_join(scenario_composition) %>%
    # Add information on proportion of plantation
    mutate(propPlant = sum(num_parcels[habitat %in% c("eucalyptus_current", "albizia_current", "albizia_future", "eucalyptus_future")]) / 1000) %>%
    # Remove unnecessary information
    select(!c(num_parcels, habitat, original_habitat)) %>%
    unique() %>%
    # Filter production target range
    #filter(production_target %in% seq(0, 1, by = 0.05)) %>%
    group_by(index) %>%
    ggplot(aes(x = production_target, y = medianRelativeOccupancy)) +  # 
  # Set shape based on propPlant > 0 (triangle for propPlant > 0, circle for propPlant == 0)
  geom_point(aes(shape = propPlant > 0 ,color = propPlant > 0), size = 3) +  
  geom_errorbar(aes(
      # ymin = p1_medianRelativeOccupancy,
      # ymax = p9_medianRelativeOccupancy,
    
    ymin = hpd_95_lower,
    ymax = hpd_95_upr,
    ),
    width = 0.02,  # Horizontal cap width
    alpha = 0.8,   # Fainter error bars
    size = 0.5     # Thinner error bars
    ) + 
    theme_bw() +
    theme(legend.position = 'none')  # Remove the legend
  

  
#   
#   
# x <-   rel_occ_sum %>% 
#     
#     # add information about sceanrio composition 
#     left_join(scenario_composition) %>%  
#     #add infomration on proportion of plantation 
#     mutate( propPlant = sum(num_parcels[habitat %in% c("eucalyptus_current", "albizia_current", "albizia_future","eucalyptus_future")])/1000)  %>%  
#     # remove uncecessary information 
#     select(!c(num_parcels, habitat, original_habitat))%>% unique() %>% 
#     
#     # filter(production_target > 0.45 & production_target < 0.51) %>% 
#      filter(production_target %in% seq(0, 1, by =0.7525)) %>% 
#     
#     group_by(index) %>% 
#     ggplot(aes(x = production_target, y = medianRelativeOccupancy, colour = index)) +
#     geom_point()+
#     
#     # Error bars with matching jitter and reduced prominence
#     geom_errorbar(aes(
#       ymin = p1_medianRelativeOccupancy,
#       ymax = p9_medianRelativeOccupancy
#     ),
#     width = 0.02, # Horizontal cap width
#     alpha = 0.8, # Fainter error bars
#     size = 0.5 # Thinner error bars
#     ) + theme_bw()+
#     theme(legend.position = 'none')
#   
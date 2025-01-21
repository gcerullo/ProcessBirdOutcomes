#This script takes the key steps out of functions for the AsssessScenarioOutcomes.R script, so 
#that we can see easily step-by-step the calculations we are doing and how these propagate error

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
zeta <- 1 # Example index for testing
single_scenario_name <- combinations_SL$scenarioStart[zeta]
processed_birds_name <- as.character(combinations_SL$species[zeta])


print(paste("Processing scenario:", single_scenario_name, "with species:", processed_birds_name))

# Filter data for the specific scenario and species
single_scenario_i <- all_start_landscape_scaled[scenarioStart == single_scenario_name]
processed_birds_i <- processed_birds[species == processed_birds_name]

# Step 3: Join scenario and birds data 
bird_join <- processed_birds_i[single_scenario_i, on = .(habitat)] 
bird_join_oneindex <- bird_join %>% filter(iteration == "draw_237")

# Step 4: Calculate habitat occupancy
bird_join_oneindex[, hab_occ := occ * num_points]
bird_join[, hab_occ := occ * num_points]

# Step 5: Calculate landscape occupancy by year
# result_oneindex <- bird_join_oneindex[, .(landscape_occ = sum(hab_occ) / total_bird_pts), 
#                     by = .(species, scenarioStart, iteration, functionalhabAge)]
resultSL <- bird_join[, .(landscape_occ = sum(hab_occ) / total_bird_pts), 
                    by = .(species, scenarioStart, iteration, functionalhabAge)]

# Step 6: Calculate 60-year occupancy for each iteration and species
# result_oneindex <- result_oneindex[, .(occ_60yr = sum(landscape_occ)), 
#                  by = .(species, scenarioStart, iteration)]

resultSL <- resultSL[, .(occ_60yr = sum(landscape_occ)), 
                 by = .(species, scenarioStart, iteration)]

#NOTE; look at the very high uncertainty in the starting landscape-level occupancy; it's huge...
hist(resultSL$occ_60yr, breaks = 60) 
 
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
zeta_index <- 450 # Example index for testing
zeta_sp <- 7
single_scenario_name <- combinations$index[zeta_index]
processed_birds_name <- as.character(combinations$species[zeta_sp])

# Filter data for the specific scenario and species
single_scenario_i <- scenario_group[index == single_scenario_name]
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

  
  #NOTE; look at the very high uncertainty in the scenario landscape-level occupancy; it's huge...
  hist(result$occ_60yr, breaks = 60) 
  
  # # Step 5: Summarise across posterior draws
  # result <- result [, .(mean_60yr = mean(occ_60yr),
  #                       sd_60yr_error = sd(occ_60yr),
  #                       se_60yr_error = sd(occ_60yr) / sqrt(.N)),
  #                       by = .(index, species, production_target)]
  

#---------------------------------------------------------------------------------
#Now summarise scenario/SL rel occ #####
  #---------------------------------------------------------------------------------
  
cap <- 1.5 # don't allow scenario occ to be more than 1.5 starting landscape occ [only used if calculating geometric mean]

  #add starting landscape to scenario
  scenarioStart <- resultSL %>% rename(SL_occ_60yr = occ_60yr)

  #add in the SL occupancy for a single scenario 
  occ_comb <- result[scenarioStart, on = .(species, iteration), nomatch = 0]
  
  # #calculate rel_occ; if rel_occ is > cap, replace with cap, to ensure scenario landscape cannot be more than 1.5 of starting landscape
  occ_comb[, rel_occ := pmin((occ_60yr / SL_occ_60yr), cap)]
  
  rel_occ <- occ_comb %>%  group_by(species, index, production_target) %>%  
    summarize(
      medianRelativeOccupancy = median(rel_occ, na.rm = TRUE),
      meanRelativeOccupancy = mean(rel_occ, na.rm = TRUE),
      p1_medianRelativeOccupancy = quantile(rel_occ, 0.1, na.rm = TRUE),
      p9_medianRelativeOccupancy = quantile(rel_occ, 0.9, na.rm = TRUE)
    )
  
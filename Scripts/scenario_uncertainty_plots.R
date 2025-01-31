#Build figure of uncertainty for whether plantations or logging are better for each each species 

#NOTES: 
#This script reads in BestScenario Inputs from AssessScenarioOutcomes.R script. 
#This, for each sp and production target, compared paired draws (ie iterations) and extracted whether 
#the scenario with the highest relative occupancy was treatment_strategy or logging or plantation. 
library(tidyverse)
library(cowplot)
library(data.table)


#read in inputs 
#.......................................
#read in scenario performancy  
rds_folder <- "Outputs/BestScenarioUncertainty"
rds_files <- list.files(rds_folder, pattern = "*.rds", full.names = TRUE)

sppCategories <- readRDS("Outputs/sppCategoriesSept24.rds") %>%  as.data.frame()


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


df <- readRDS(rds_files[[1]])

#how many total iteration *production strategy combos
#unique(df$production_target) = 102
#unique(df$iterations = 500)
#102 *500 = 51000
# df <- df %>% filter(production_target >0.25)
unique_combinations <- df %>%  group_by(species) %>% count()  %>% ungroup %>%   select(n) %>% unique %>%  pull()
print(unique_combinations)

#calculate number of posterior draws across all production targets where the best performing scenario 
#was either logging or plantations
df_sum <- df %>%  unique() %>% 
  group_by(species, treatment_strategy) %>% count() %>%  
  mutate(percentage = n / unique_combinations)

#add in species threat and specialsm info 
df_sum <- df_sum %>% left_join(sppCategories) %>% left_join(IUCN_classification)

threatened_sp <- df_sum %>% ungroup %>% filter(threatened == "Y") %>%  
  select(species) %>%  unique() %>%  
  pull()
loser_spp <- df_sum %>% ungroup %>% filter(spp_category == "loser") %>%  
  select(species) %>%  unique() %>%  
  pull()

#quick summaries - which species are generally better with plantations 
sp_prefer_plantation_dominated_production <- df_sum %>% filter(treatment_strategy =="plantation" & percentage>0.5) 


alldataplot <-  df_sum %>%  
   filter(species %in% loser_spp) %>%  # Filter by species of interest
   
   # Group by species to calculate the maximum plantation percentage
   group_by(species) %>%
   mutate(max_plantation = max(percentage[treatment_strategy == "plantation"])) %>%
   ungroup() %>%
   
   # Arrange species by the maximum plantation percentage in descending order
   arrange(max_plantation) %>%
   mutate(species = factor(species, levels = unique(species))) %>% 
   
   # Ensure treatment_strategy is a factor (ordering)
   mutate(treatment_strategy = factor(treatment_strategy, levels = c("logging", "plantation"))) %>% 
   
   # Plot
   ggplot(aes(x = species, y = percentage, fill = treatment_strategy)) +
   geom_bar(stat = "identity", width = 0.7, color = "black", size = 0.3) +  # Stacked bar
  geom_hline(yintercept = c(0.25, 0.5, 0.75), linetype = "dashed", color = "black", size = 0.7) +
  
   coord_flip() +  # Flip coordinates for horizontal bars
   scale_fill_manual(
     values = c("logging" = "#E69F00",   # Orange for logging
                "plantation" = "#56B4E9"),  # Blue for plantation
     labels = c("Selective-logging Best", "Plantations Best")  # Legend labels
   ) +
   labs(
     x = NULL,  # Remove x-axis label
     y = "Proportion",
     fill = element_blank()
   ) +
   theme_minimal(base_size = 14) +  # Clean theme for publication
   theme(
     panel.grid.major.y = element_blank(),  # Remove horizontal grid lines
     panel.grid.minor = element_blank(),
     panel.grid.major.x = element_line(color = "gray80", size = 0.5),
     axis.text.y = element_text(face = "italic", size = 12),  # Italic species names
     axis.title.x = element_text(size = 14),
     legend.position = "top",  # Move legend to the top
     legend.title = element_text(size = 12),
     legend.text = element_text(size = 12)
   )
 
 
#######################################
##########################################################
#Plot uncertaubty when using multiple production target filters 

# Define a function to preprocess the data
process_data <- function(df, production_threshold, sppCategories, IUCN_classification) {
  df_filtered <- df %>% filter(production_target > production_threshold)
  
  unique_combinations <- df_filtered %>%
    group_by(species) %>%
    count() %>%
    ungroup() %>%
    select(n) %>%
    unique() %>%
    pull()
  
  df_sum <- df_filtered %>%
    unique() %>%
    group_by(species, treatment_strategy) %>%
    count() %>%
    mutate(percentage = n / unique_combinations) %>%
    left_join(sppCategories, by = "species") %>%
    left_join(IUCN_classification, by = "species")
  
  threatened_sp <- df_sum %>%
    ungroup() %>%
    filter(threatened == "Y") %>%
    select(species) %>%
    unique() %>%
    pull()
  
  loser_spp <- df_sum %>%
    ungroup() %>%
    filter(spp_category == "loser") %>%
    select(species) %>%
    unique() %>%
    pull()
  
  sp_prefer_plantation_dominated_production <- df_sum %>%
    filter(treatment_strategy == "plantation" & percentage > 0.5)
  
  list(df_sum = df_sum, loser_spp = loser_spp)
}

# Define a function to generate plots
generate_plot <- function(df_sum, loser_spp, show_species_labels = FALSE) {
  df_sum %>%
    filter(species %in% loser_spp) %>%
    group_by(species) %>%
    mutate(max_plantation = max(percentage[treatment_strategy == "plantation"])) %>%
    ungroup() %>%
    arrange(max_plantation) %>%
    mutate(species = factor(species, levels = unique(species))) %>%
    mutate(treatment_strategy = factor(treatment_strategy, levels = c("logging", "plantation"))) %>%
    ggplot(aes(x = species, y = percentage, fill = treatment_strategy)) +
    geom_bar(stat = "identity", width = 0.7, color = "black", size = 0.3) +
    geom_hline(yintercept = c(0.25, 0.5, 0.75), linetype = "dashed", color = "black", size = 0.7) +
    coord_flip() +
    scale_fill_manual(
      values = c("logging" = "#E69F00", "plantation" = "#56B4E9"),
    ) +
    labs(
      x = NULL,
      y = "Proportion",
      fill = element_blank()
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major.y = element_blank(),  # Remove horizontal grid lines
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "gray80", size = 0.5),
      axis.text.y = if (show_species_labels) element_text(face = "italic", size = 12) else element_blank(),  # Conditional y-axis labels
      axis.title.x = element_text(size = 14),
      legend.position = "top",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12)
    )
}

# Process data and generate plots for 0.25, 0.5, and 0.75 thresholds
result025 <- process_data(df, 0.25, sppCategories, IUCN_classification)
plot025 <- generate_plot(result025$df_sum, result025$loser_spp, show_species_labels = FALSE)

result05 <- process_data(df, 0.5, sppCategories, IUCN_classification)
plot05 <- generate_plot(result05$df_sum, result05$loser_spp, show_species_labels = FALSE)

result075 <- process_data(df, 0.75, sppCategories, IUCN_classification)
plot75 <- generate_plot(result075$df_sum, result075$loser_spp, show_species_labels = FALSE)

#COMBINED FIGURE
# Combine the three plots into a single figure
combined_plot <- plot_grid(
  alldataplot + labs(title = "All data"),  # Add a title to each plot
  plot05 + labs(title = "Production Threshold > 0.5"),
  plot75 + labs(title = "Production Threshold > 0.75"),
  ncol = 4,  # Arrange the plots in a single column
  labels = c("A", "B", "C"),  # Add labels (optional)
  label_size = 14,  # Size of the labels
  rel_widths = c(1.8, 1, 1)  # Adjust the relative width of the first plot
)

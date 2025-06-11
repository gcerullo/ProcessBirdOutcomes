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
 
 

##########################################
#For threatened and loser species, plot uncertainty for multiple production targets 

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
  
  list(df_sum = df_sum, loser_spp = loser_spp, threatened_sp = threatened_sp)
}


generate_plot <- function(df_sum, species_filter, show_species_labels = FALSE) {
  df_sum %>%
    filter(species %in% species_filter) %>%
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
      labels = c("Selective-logging Best", "Plantations Best")  # Legend labels
      
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


# Process data for 0.25, 0.5, and 0.75 thresholds
resultalldata <- process_data(df, 0, sppCategories, IUCN_classification)
result025 <- process_data(df, 0.25, sppCategories, IUCN_classification)
result05 <- process_data(df, 0.5, sppCategories, IUCN_classification)
result075 <- process_data(df, 0.75, sppCategories, IUCN_classification)

# Generate plots for loser species #####
plotalldata_loser <- generate_plot(resultalldata$df_sum, resultalldata$loser_spp, show_species_labels = TRUE)
plot025_loser <- generate_plot(result025$df_sum, result025$loser_spp, show_species_labels = FALSE)
plot05_loser <- generate_plot(result05$df_sum, result05$loser_spp, show_species_labels = FALSE)
plot75_loser <- generate_plot(result075$df_sum, result075$loser_spp, show_species_labels = FALSE)

# Generate plots for threatened species ####
plotalldata_threatened <- generate_plot(resultalldata$df_sum, resultalldata$threatened_sp, show_species_labels = TRUE)
plot025_threatened <- generate_plot(result025$df_sum, result025$threatened_sp, show_species_labels = FALSE)
plot05_threatened <- generate_plot(result05$df_sum, result05$threatened_sp, show_species_labels = FALSE)
plot75_threatened <- generate_plot(result075$df_sum, result075$threatened_sp, show_species_labels = FALSE)


#combined figure losers ####

#combined figure losers ####
# Combine the three plots into a single figure
combined_plot_losers_multiple_production_targets <- plot_grid(
  plotalldata_loser + labs(title = "All Scenarios"),  # Add a title to each plot
  plot05_loser + labs(title = "   Production > 0.5"),
  plot75_loser + labs(title = "   Production > 0.75"),
  ncol = 4,  # Arrange the plots in a single column
  labels = c("A", "B", "C"),  # Add labels (optional)
  label_size = 14,  # Size of the labels
  rel_widths = c(1.8, 1, 1)  # Adjust the relative width of the first plot
)

# Combine the three plots into a single figure
combined_plot_threatened_multiple_production_targets <- plot_grid(
  plotalldata_threatened + labs(title = "All Scenarios"),  # Add a title to each plot
  plot05_threatened + labs(title = "   Production > 0.5"),
  plot75_threatened + labs(title = "   Production > 0.75"),
  ncol = 4,  # Arrange the plots in a single column
  labels = c("A", "B", "C"),  # Add labels (optional)
  label_size = 14,  # Size of the labels
  rel_widths = c(1.8, 1, 1)  # Adjust the relative width of the first plot
)

#------------------------------------------
#repeat fig for ALL species 
#------------------------------------------

process_data_all <- function(df, production_threshold, sppCategories, IUCN_classification) {
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
  
  list(df_sum = df_sum)
}

generate_plot_all <- function(df_sum, show_species_labels = FALSE) {
  df_sum %>%
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
      labels = c("Selective-logging Best", "Plantations Best")
    ) +
    labs(
      x = NULL,
      y = "Proportion",
      fill = element_blank()
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "gray80", size = 0.5),
      axis.text.y = if (show_species_labels) element_text(face = "italic", size = 12) else element_blank(),
      axis.title.x = element_text(size = 14),
      legend.position = "top",
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12)
    )
}

result_all <- process_data_all(df, 0.5, sppCategories, IUCN_classification)
plot_all <- generate_plot_all(result_all$df_sum, show_species_labels = TRUE)

# Define the number of chunks
num_chunks <- 3 

all_spp <- as.vector(unique(df$species))
# Split the all_spp vector into multiple chunks
chunked_spp <- split(all_spp, ceiling(seq_along(all_spp) / (length(all_spp) / num_chunks)))

# Generate plots for each chunk
plot_list <- lapply(chunked_spp, function(spp_chunk) {
  # Filter the data for the current species chunk
  df_chunk <- df[df$species %in% spp_chunk, ]
  
  # Process data for the current chunk
  result_chunk <- process_data_all(df_chunk, 0.5, sppCategories, IUCN_classification)
  
  # Generate plot for the current chunk
  generate_plot_all(result_chunk$df_sum, show_species_labels = TRUE)
})

# Access individual plots if needed
plot_chunk1 <- plot_list[[1]]
plot_chunk2 <- plot_list[[2]]
plot_chunk3 <- plot_list[[3]]


#EXPORT FIGURES 

#PDF ####

# Save the loser species combined plots as A4-sized output
ggsave("Figures/loser_species_uncertainty_by_production_target_plots_A4.pdf",
       combined_plot_losers_multiple_production_targets, 
       width = 20, height = 11.69, units = "in", 
       bg = "white")

# Save the threatened species uncertainty by production target plots as A4-sized output with a white background
ggsave("Figures/threatened_species_uncertainty_by_production_target_plots_A4.pdf",
       combined_plot_threatened_multiple_production_targets,
       width = 20, height = 12, units = "in", 
       bg = "white")

#save all spp 
ggsave("Figures/allsp_chunk1.pdf",
       plot_chunk1,
       width = 20, height = 12, units = "in", 
       bg = "white")

ggsave("Figures/allsp_chunk2.pdf",
       plot_chunk2,
       width = 20, height = 12, units = "in", 
       bg = "white")

ggsave("Figures/allsp_chunk3.pdf",
       plot_chunk3,
       width = 20, height = 12, units = "in", 
       bg = "white")

#PNG ####

# Save the loser species combined plots as A4-sized output
ggsave("Figures/loser_species_uncertainty_by_production_target_plots_A4.png",
       combined_plot_losers_multiple_production_targets, 
       width = 20, height = 11.69, units = "in", 
       bg = "white")

# Save the threatened species uncertainty by production target plots as A4-sized output with a white background
ggsave("Figures/threatened_species_uncertainty_by_production_target_plots_A4.png",
       combined_plot_threatened_multiple_production_targets,
       width = 20, height = 12, units = "in", 
       bg = "white")

#save all spp 
ggsave("Figures/allsp_chunk1.png",
       plot_chunk1,
       width = 20, height = 12, units = "in", 
       bg = "white")

ggsave("Figures/allsp_chunk2.png",
       plot_chunk2,
       width = 20, height = 12, units = "in", 
       bg = "white")

ggsave("Figures/allsp_chunk3.png",
       plot_chunk3,
       width = 20, height = 12, units = "in", 
       bg = "white")


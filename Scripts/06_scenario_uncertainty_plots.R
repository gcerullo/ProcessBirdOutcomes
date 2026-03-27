#Build figure of uncertainty for whether plantations or logging are better for each each species 

#NOTES: 
#This script reads in BestScenario Inputs from AssessScenarioOutcomes.R script. 
#This, for each sp and production target, compared paired draws (ie iterations) and extracted whether 
#the scenario with the highest relative occupancy was treatment_strategy or logging or plantation. 
library(tidyverse)
library(cowplot)
library(data.table)
library(stringr)


#read in inputs 
#.......................................
#read in scenario performancy  
rds_folder <- "Outputs/BestScenarioUncertainty"
rds_files <- list.files(rds_folder, pattern = "*.rds", full.names = TRUE)

sppCategories <- readRDS("Outputs/sppCategories.rds") 

#select which starting landscape and scenario rules you want
rds_files
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
df_sum <- df_sum %>% left_join(sppCategories) %>%
  mutate(species = str_remove(species, "^Un_matched\\s*_")) %>% 
  mutate(species = str_remove(species, "^Un[-_]*\\s*matched\\s*-*\\s*")) %>%  
  mutate(species = str_replace_all(species, " ", ""))

#for scenarios where 100% of the time they o better in once habitat type, we still need to make sure they have data 
#for both plantations and logging 
hundred_percenters <- df_sum %>% 
  filter(percentage == 1) %>%  
  mutate(treatment_strategy = "plantation", 
         percentage = 0) 
df_sum <- df_sum %>%  rbind(hundred_percenters)

loser_spp <- df_sum %>% ungroup %>% filter(spp_category == "loser") %>%  
  select(species) %>%  unique() %>%  
  pull()

int1L_spp  <- df_sum %>% ungroup %>% filter(spp_category == "intermediate1L") %>%  
  select(species) %>%  unique() %>%  
  pull()

all_spp  <- df_sum %>% ungroup %>%   select(species) %>%  unique() %>%  
  pull()

#quick summaries - which species are generally better with plantation scenarios  
sp_prefer_plantation_dominated_production <- df_sum %>% filter(treatment_strategy =="plantation" & percentage>0.5) 

#plot function ####

generate_uncertainty_plot <- function(df_sum, species_group, treatment_order = c("logging", "plantation")) {
  
  df_filtered <- df_sum %>%
    filter(species %in% species_group) %>%  # Filter by species of interest
    
    # Group by species to calculate the maximum plantation percentage
    group_by(species) %>%
    mutate(max_plantation = max(percentage[treatment_strategy == "plantation"])) %>%
    ungroup() %>%
    
    # Arrange species by the maximum plantation percentage in descending order
    arrange(max_plantation) %>%
    mutate(species = factor(species, levels = unique(species))) %>%
    
    # Ensure treatment_strategy is a factor (ordering)
    mutate(treatment_strategy = factor(treatment_strategy, levels = treatment_order)) 
  
  # Plot
  ggplot(df_filtered, aes(x = species, y = percentage, fill = treatment_strategy)) +
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
}


loser_bird_uncertainty_plot <- generate_uncertainty_plot(df_sum, loser_spp)
int1_bird_uncertainty_plot <- generate_uncertainty_plot(df_sum, int1L_spp)
all_sp_uncertainty_plot <-  generate_uncertainty_plot(df_sum, all_spp)
# Define the number of chunks you want to split all spp into (e.g., 3)
num_chunks <- 2

# Split the all_spp vector into multiple chunks
chunked_spp <- split(all_spp, ceiling(seq_along(all_spp) / (length(all_spp) / num_chunks)))

# Generate plots for each chunk
plot_list <- lapply(chunked_spp, function(spp_chunk) {
  generate_uncertainty_plot(df_sum, spp_chunk)
})

all_sp_split1 <- plot_list[[1]]
all_sp_split2 <- plot_list[[2]]


#EXPORT FIGURES ####

#PDF ####

# Save the loser species combined plots as A4-sized output
ggsave("Figures/loser_dungbeetle_uncertainty_plot.pdf",
       loser_dungbeetle_uncertainty_plot,
       width = 15, height = 11.69, units = "in", 
       bg = "white")

ggsave("Figures/int1lgrp_dungbeetle_uncertainty_plot.pdf",
       int1_dungbeetle_uncertainty_plot,
       width = 15, height = 11.69, units = "in", 
       bg = "white")

ggsave("Figures/all_sp_uncertainty_plot.pdf",
       all_sp_uncertainty_plot,
       width = 12, height = 12, units = "in", 
       bg = "white")


ggsave("Figures/all_sp_chunk1.pdf",
       all_sp_split1,
       width = 12, height = 12, units = "in", 
       bg = "white")

ggsave("Figures/all_sp_chunk2.pdf",
       all_sp_split2,
       width = 12, height = 12, units = "in", 
       bg = "white")

#PNG ####
# Save the loser species combined plots as A4-sized output
ggsave("Figures/loser_dungbeetle_uncertainty_plot.png",
       loser_dungbeetle_uncertainty_plot,
       width = 15, height = 11.69, units = "in", 
       bg = "white")

ggsave("Figures/int1lgrp_dungbeetle_uncertainty_plot.png",
       int1_dungbeetle_uncertainty_plot,
       width = 15, height = 11.69, units = "in", 
       bg = "white")

ggsave("Figures/all_sp_uncertainty_plot.png",
       all_sp_uncertainty_plot,
       width = 12, height = 12, units = "in", 
       bg = "white")


ggsave("Figures/all_sp_chunk1.png",
       all_sp_split1,
       width = 12, height = 12, units = "in", 
       bg = "white")

ggsave("Figures/all_sp_chunk2.png",
       all_sp_split2,
       width = 12, height = 12, units = "in", 
       bg = "white")

# ##########################################
# Doesnt seem to be much of an effect of production target on uncertainty


# #For  loser species, plot uncertainty for multiple production targets 
# process_data <- function(df, production_threshold, sppCategories ) {
#   df_filtered <- df %>% filter(production_target > production_threshold)
#   
#   unique_combinations <- df_filtered %>%
#     group_by(species) %>%
#     count() %>%
#     ungroup() %>%
#     select(n) %>%
#     unique() %>%
#     pull()
#   
#   df_sum <- df_filtered %>%
#     unique() %>%
#     group_by(species, treatment_strategy) %>%
#     count() %>%
#     mutate(percentage = n / unique_combinations) %>%
#     left_join(sppCategories, by = "species") %>% 
#     #removed if unmarched in name of spp
#   
# 
#   loser_spp <- df_sum %>%
#     ungroup() %>%
#     filter(spp_category == "loser") %>%
#     select(species) %>%
#     unique() %>%
#     pull()
#   
#   sp_prefer_plantation_dominated_production <- df_sum %>%
#     filter(treatment_strategy == "plantation" & percentage > 0.5)
#   
#   list(df_sum = df_sum, loser_spp = loser_spp)
# }
# 
# 
# generate_plot <- function(df_sum, species_filter, show_species_labels = FALSE) {
#   df_sum %>%
#     filter(species %in% species_filter) %>%
#     group_by(species) %>%
#     mutate(max_plantation = max(percentage[treatment_strategy == "plantation"])) %>%
#     ungroup() %>%
#     arrange(max_plantation) %>%
#     mutate(species = factor(species, levels = unique(species))) %>%
#     mutate(treatment_strategy = factor(treatment_strategy, levels = c("logging", "plantation"))) %>%
#     ggplot(aes(x = species, y = percentage, fill = treatment_strategy)) +
#     geom_bar(stat = "identity", width = 0.7, color = "black", size = 0.3) +
#     geom_hline(yintercept = c(0.25, 0.5, 0.75), linetype = "dashed", color = "black", size = 0.7) +
#     coord_flip() +
#     scale_fill_manual(
#       values = c("logging" = "#E69F00", "plantation" = "#56B4E9"),
#       labels = c("Selective-logging Best", "Plantations Best")  # Legend labels
#       
#     ) +
#     labs(
#       x = NULL,
#       y = "Proportion",
#       fill = element_blank()
#     ) +
#     theme_minimal(base_size = 14) +
#     theme(
#       panel.grid.major.y = element_blank(),  # Remove horizontal grid lines
#       panel.grid.minor = element_blank(),
#       panel.grid.major.x = element_line(color = "gray80", size = 0.5),
#       axis.text.y = if (show_species_labels) element_text(face = "italic", size = 12) else element_blank(),  # Conditional y-axis labels
#       axis.title.x = element_text(size = 14),
#       legend.position = "top",
#       legend.title = element_text(size = 12),
#       legend.text = element_text(size = 12)
#     )
# }
# 
# 
# # Process data for 0.25, 0.5, and 0.75 thresholds
# resultalldata <- process_data(df, 0, sppCategories)
# result025 <- process_data(df, 0.25, sppCategories)
# result05 <- process_data(df, 0.5, sppCategories)
# result075 <- process_data(df, 0.75, sppCategories)
# 
# # Generate plots for loser species #####
# plotalldata_loser <- generate_plot(resultalldata$df_sum, resultalldata$loser_spp, show_species_labels = TRUE)
# plot025_loser <- generate_plot(result025$df_sum, result025$loser_spp, show_species_labels = FALSE)
# plot05_loser <- generate_plot(result05$df_sum, result05$loser_spp, show_species_labels = FALSE)
# plot75_loser <- generate_plot(result075$df_sum, result075$loser_spp, show_species_labels = FALSE)
# 
# 
# 
# #combined figure losers ####
# 
# #combined figure losers ####
# # Combine the three plots into a single figure
# combined_plot_losers_multiple_production_targets <- plot_grid(
#   plotalldata_loser + labs(title = "All Scenarios"),  # Add a title to each plot
#   plot05_loser + labs(title = "   Production > 0.5"),
#   plot75_loser + labs(title = "   Production > 0.75"),
#   ncol = 4,  # Arrange the plots in a single column
#   labels = c("A", "B", "C"),  # Add labels (optional)
#   label_size = 14,  # Size of the labels
#   rel_widths = c(2.5, 1, 1)  # Adjust the relative width of the first plot
# )
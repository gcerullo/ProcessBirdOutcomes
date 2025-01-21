#calculate the hunting threat status of birds 
library(tidyverse)
#If you go in the wide threat matrix csv then the "biological_resource_use_hunting" column you can get the scores 
#(based on the IUCN timing, scope and severity system) for the threat. 
#Anything above 6 is considered a medium or high impact threat by IUCN.

#The use and purpose csv is just what the IUCN record that species as being used as if helpful
#and the IUCN all threat assessments csv is the raw threat encoding for the matrix.


birds <- readRDS("inputs/sppCategoriesSept24.rds") %>% as.data.frame() %>%  
  rename(spp = species)

scientificNames <- read.csv("inputs/AllBorneoSpeciesTraits.csv") %>% 
  select(spp, LatinName,redlistCategory) %>%  
  rename(scientificName = LatinName)

threats <- read.csv("inputs/IUCN_wide_ThreatMatrix_Sept2024.csv") %>%  
  select(scientificName, biological_resource_use_hunting)

df <- scientificNames %>% left_join(threats) %>%  
  right_join(birds)


# Filter rows where biological_resource_use_hunting is NA
rows_with_na <- df %>%
  filter(is.na(biological_resource_use_hunting))

# Display rows with NA (if any exist)
print(rows_with_na)

threats %>%  filter(scientificName  == "Pellorneum bicolor") #ferruginious babbler
threats %>%  filter(scientificName  == "Pellorneum rostratum") #White-chested Babbler
threats %>%  filter(scientificName  == "Copsychus pyrropygus") #Rufous-tailed Shama
threats %>%  filter(scientificName  == "Tropicoperdix charltonii") #Chestnut-necklaced Partridge
threats %>%  filter(scientificName  == "Pellorneum malaccense") #Short-tailed Babbler
threats %>%  filter(scientificName  == "Copsychus malabaricus") # White-rumped Shama
threats %>%  filter(scientificName  == "Chloropicoides rafflesii") # Olive-backed Woodpecker
threats %>%  filter(scientificName  == "Erythropitta arquata") # blue-banded pitta
threats %>%  filter(scientificName  == "Lacedo pulchella") # banded kingfisher
threats %>%  filter(scientificName  == "Chrysophlegma miniaceum") # banded woodpecker
threats %>%  filter(scientificName  == "Ixodia erythropthalmos") # spectactled bulbul

# TO DO: Manually add biological_resource_use_hunting values for these species
df <- df %>%
  mutate(biological_resource_use_hunting = case_when(
    spp == "Ferruginous Babbler" ~ 0, 
    spp == "White-chested Babbler" ~ 0, 
    spp == "Rufous-tailed Shama" ~ 6, # 
    spp == "Chestnut-necklaced Partridge" ~ 5, 
    spp == "Short-tailed Babbler" ~ 0, #
    spp == "White-rumped Shama" ~ 5, #
    spp == "Olive-backed Woodpecker" ~ 0, 
    spp == "Blue-banded Pitta" ~ 0, #
    spp == "Banded Kingfisher" ~ 0,
    spp == "Banded Woodpecker" ~ 0,
    spp == "Banded Woodpecker" ~ 0,
    TRUE ~ biological_resource_use_hunting # Keep original values for others
  ))

# Update `redlistCategory` for specific species
df <- df %>%
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
  ))

#how many of birds of different categories are threatened by hunting?
x <- df %>% group_by(biological_resource_use_hunting, spp_category) %>% count()
y <- df %>% group_by(spp_category) %>% count()

#total 1L specalists
num1L <- df %>% filter(spp_category == "intermediate1L") %>%  count()
sp1Lthreatened <- df %>% filter(spp_category == "intermediate1L" & !redlistCategory== "Least Concern") %>% count()
#how many 1L species are hunted at least sometimes?
num1Lsomehunting <- df %>% filter(spp_category == "intermediate1L" & biological_resource_use_hunting > 0) %>%  count()
num1LthreatendSomeHunting <- num1Lsomehunting <- df %>% 
  filter(spp_category == "intermediate1L" & biological_resource_use_hunting > 0) %>% 
  filter(!redlistCategory== "Least Concern") %>% 
  count()

#% of 1L species that are hunted 
(num1Lsomehunting/sp1Lthreatened)*100

#% of 1L species that are hunted & threatened 
(num1LthreatendSomeHunting/num1L)*100 #same as above 

#total threatened spp #79
threatendSp <- df %>% filter(!redlistCategory== "Least Concern") 


threatendSp <- df %>% filter(!redlistCategory== "Least Concern") %>% 
  group_by(spp_category) %>%  
  count() %>%  
  mutate(percentageThreatened = n/79)


# 21.02.2023 
#This code: 
#1 Brings together all raw site-level point count data from forest and plantation sites coordinated by Cerullo in 2022 
#and carried out by Daniel Kong into one tidy dataframe, then... 
#2. Combines this with historic forest PC data collected by David Edwards and then
#3. comines with Simon Mitchells logged and old-growth forest data.

#these data are then joined with a LiDAR above-ground carbon and tree canopy height information from each point 
#LiDAR data are calculated for 50m and 100m buffers around each point in Google Earth Engine. 

#THere is a commented out section of code that also extract trait data for all species based on the AVONET portal 

rm(list = ls())

library(usethis)
library(stringr)
library(tidyr)
library(ggplot2)
library(data.table)
library(lubridate)
library(taxize)
library(fuzzyjoin)
library(tidyverse)
library(flocker)

#read in Data ####

#############  POINT COUNT DATA ###############

#1: Dave Historic Point Counts
DE_pcs <- read.csv( "RawData/DE_2008_2011_forest_pointCounts.csv") %>% 
  rename(Day = day)
#2 Daniel Kong plantation data  
plantations <- read.csv("RawData/plantationPCs_2022.csv")
#3 read in Simon Mitchell data
Simon <- read.csv("RawData/SimonMitchellSelectedPointCounts.csv")

#4 read in xa crosswalk that translates Simon and Daves spp to same matching names 
SppCrosswalk <- read.csv("Inputs/SimonDaveSpeciesNamesCrossWalk.csv",na.strings = "")

#5  Read in raw point count data from 2022 forest sites
file_list<-list.files('RawData/all_PC_forest_site_CSVs_2022',
                      pattern = '*.csv',
                      full.names = T)

#6 Get list of species that were not detected in Dave's point counts
undetectedSppDave <- DE_pcs %>% 
  mutate(abundance =  ifelse(is.na(abundance),0, abundance)) %>%  #replace NA abundance with 0
  group_by(spp) %>%
  summarise(sum(abundance)) %>%
  rename(totalSampledInd = 2) %>% 
  filter(totalSampledInd == 0) %>% 
  dplyr::select(spp)



################## LIDAR DATA ####################


Lidar <- read.csv("Inputs/GEE_AllPointCountsABC_CTH_50m_100m_buffer.csv") %>% 
  mutate(Habitat = case_when(
    Site == "LF1" ~ "Twice_logged",
    Site == "LF2" ~ "Twice_logged",
    Site == "LFE" ~ "Twice_logged", 
    Site == "RLF" ~  "Twice_logged", 
    TRUE ~ Habitat
  )) %>% 
  select(Site, point, Latitude, Longitude, ABC50, ABC100, CTH50, CTH100)


############ EXTRA PC DATE AND TIME INFORMATION ###############
#read in times and dates for Daniel
Daniel_DT <- read.csv("Inputs/PC_Dates_Times_2022.csv") %>% mutate(Site = case_when(
  Site == "GCAL1" ~ "AL1",
  Site == "GCAL2" ~ "AL2", 
  Site == "GCAL3" ~ "AL3", 
  Site == "GCEC1" ~ "EC1", 
  Site == "GCEC2" ~ "EC2", 
  Site == "GCEC3" ~ "EC3", 
  Site == "GC2L" ~ "GCL2", 
  TRUE ~ Site
))

#Dave missing dates and times 
DaveDates <- read.csv("Inputs/TimesDatesDavePointCounts.csv") %>% rename(Day = day) %>% select(-X) 

############### PLANTATION AGE ##################

#plantation age information
df_habitat <- data.table::fread("Inputs/Plantation_Habitat_Structure.csv") %>%
  as_tibble() %>%
  mutate(point_id = interaction(Site, Point)) %>%  
  select(point_id, Age)

############## Sp THREATS & DEPENDENCIES ###################

#iucn status and forest dependency:low,medium,high (none reclassified to low)  (non-resident = 1; we get rid of these 6 spp)
df_IUCN_FD <- data.table::fread("Inputs/SppForForIntegrityIUCN.csv") %>%
  filter(nonResident == 0)


################### Functions ##################

# Function to pivot longer and process data
process_data <- function(data, type) {
  data %>%
    rename(spp = Other.Information) %>%
    select(spp, Site, Habitat, Day, starts_with(type)) %>%
    pivot_longer(cols = -c(spp, Site, Habitat, Day),
                 names_to = "point",
                 names_prefix = paste0(type, "."),
                 values_to = type) %>%
    mutate(point = as.numeric(gsub(paste0(type, "."), "", point)) + 1) %>%
    filter(spp != "Point Count" & spp != "Species")
}



#CODE BEGINS ####
# 1. Organise the 2022 data #####

# Read and combine data files into a list
my_files <- map(file_list, read.csv) %>%
  setNames(nm = .)

#turn each raw datfilea sheet into an element of a list
my_files<-lapply(file_list, read.csv) %>% 
  setNames(nm = .)

#bring all together into one dataframe 
wide_df<- rbindlist(my_files, fill = TRUE)

# Get abundances 
Abundances <- wide_df %>%   
  rename(spp = Other.Information) %>%      #remove unneeded cols
  select(spp, Site, Habitat, Day, starts_with("abundance")) %>% 
  pivot_longer(cols = !c(spp,Site, Habitat, Day),
               names_to = "point", 
               names_prefix = "Abundance.",
               values_to = "abundance") %>% 
  
  #this bit of code renumbers points so that they are correct
  # turn "abundance" to 0 (point 1 is currently called abundance)
  mutate(point =  ifelse(point == "Abundance",0, point))  %>% 
  # reclassify column to numeric and add 1 to make points correct
  mutate(point = as.numeric(point)+1) %>% 
  #this bit of code removes species called "Point count" and "Species
  filter(spp != "Point Count") %>% 
  filter(spp != "Species")

#Get distances

Distances <- wide_df %>%   
  rename(spp = Other.Information) %>%      #remove unneeded cols
  select(spp, Site, Habitat, Day, starts_with("Distance")) 

# #check columns are numeric or pivot longer won't work!
 Distances$Distance.2 <- as.numeric(Distances$Distance.2)

Distances <- Distances %>% pivot_longer(cols = !c(spp,Site, Habitat, Day),
                                        names_to = "point", 
                                        names_prefix = "Distance.",
                                        values_to = "distance") %>% 
  #this bit of code renumbers points so that they are correct
  # turn "abundance" to 0 (point 1 is currently called abundance)
  mutate(point =  ifelse(point == "Distance",0, point))  %>% 
  #this bit of code removes species called "Point count" and "Species
  filter(spp != "Point Count") %>% 
  filter(spp != "Species") %>% 
  # reclassify column to numeric and add 1 to make points correct
  mutate(point = as.numeric(point)+1) 

#get flyovers
Flyovers <-  wide_df %>%   
  rename(spp = Other.Information) %>%      #remove unneeded cols
  select(spp, Site, Habitat, Day, starts_with("Flyover")) 

#make sure columns are numeric 
Flyovers$Flyover.1 <- as.numeric(Flyovers$Flyover.1)

Flyovers <- Flyovers %>% pivot_longer(cols = !c(spp,Site, Habitat, Day),
                                      names_to = "point", 
                                      names_prefix = "Flyover.",
                                      values_to = "flyover") %>% 
  #this bit of code renumbers points so that they are correct
  # turn "abundance" to 0 (point 1 is currently called abundance)
  mutate(point =  ifelse(point == "Flyover",0, point))  %>% 
  #this bit of code removes species called "Point count" and "Species
  filter(spp != "Point Count") %>% 
  filter(spp != "Species") %>% 
  # reclassify column to numeric and add 1 to make points correct
  mutate(point = as.numeric(point)+1) 


# heard or soon 
Heard.Seen <- wide_df %>%   
  rename(spp = Other.Information) %>%      #remove unneeded cols
  select(spp, Site, Habitat, Day, starts_with("H.S.HS")) %>% 
  pivot_longer(cols = !c(spp,Site, Habitat, Day),
               names_to = "point", 
               names_prefix = "H.S.HS.",
               values_to = "H.S.HS") %>% 
  #this bit of code renumbers points so that they are correct
  # turn "abundance" to 0 (point 1 is currently called abundance)
  mutate(point =  ifelse(point == "H.S.HS",0, point))  %>% 
  #this bit of code removes species called "Point count" and "Species
  filter(spp != "Point Count") %>% 
  filter(spp != "Species") %>% 
  # reclassify column to numeric and add 1 to make points correct
  mutate(point = as.numeric(point)+1)

#combine all Daniel's data  
forestPCs <- Abundances %>% left_join(Distances)%>% 
  left_join(Heard.Seen) %>% 
  left_join(Flyovers) 

forestPCs <- forestPCs %>% cbind(Sampler = "Daniel_Kong") %>% cbind(Year = 2022)

#rename "Primary" so that they are spelled the same 
forestPCs$Habitat <- gsub("Primary ",  "Primary", forestPCs$Habitat)


#Add in  2022 plantation data ####   
colnames(plantations)
colnames(forestPCs)

#replace plantation NAs in abundance with 0 
plantations <- plantations %>% mutate(abundance = ifelse(is.na(abundance), 0, abundance))

sum(plantations$abundance)
sum(forestPCs$abundance)


PCs2022 <- bind_rows(forestPCs, plantations) %>% select(-X)

#how many duplicates
PCs2022 %>% 
  group_by(Site, point, spp, Day, Habitat, Sampler) %>% 
  count() %>%  
  filter(n >1)

#checks
PCs2022 %>%  select(Site) %>% unique()
PCs2022 %>%  select(Habitat) %>% unique()

##remove strange Daniel Unique spp 
#. remove single hill blue flycatcher - likely an error
#. remove yell0w-bellied warbler - likely an error 

PCs2022 <- PCs2022 %>% filter(!spp == "Yellow-bellied Warbler")
PCs2022 <- PCs2022 %>% filter(!spp == "Red Jungle Fowl")
PCs2022 <- PCs2022 %>% filter(!spp == "Red jungle Fowl")
PCs2022 <- PCs2022 %>% filter(!spp == "Hill Blue-Flycatcher")
PCs2022 <- PCs2022 %>% filter(!spp == "NA")
PCs2022 <- PCs2022 %>% filter(!spp == "Javan Myna")

XX <- PCs2022 %>%  filter(Site == "GCL2"& point ==9 & Day==3)

#how many duplicates  
PCs2022 %>% 
  group_by(Site, point, spp, Day, Habitat, Sampler) %>% 
  count() %>%  
  filter(n >1)

#2.Organise Edwards Data ####

DE_pcs

#how many duplicates  
DE_pcs %>% 
  group_by(Site, point, spp, Day, Habitat, Sampler) %>% 
  count() %>%  
  filter(n >1)

#check colnames in Edwards and Kong PC to check spelling is same for cols 
colnames(DE_pcs)
colnames(PCs2022)


#combine edwards and kong point counts 
##NB; edward's: data doesn't have
#1. FLYOVERS
#2. DISTANCE OF OBSERVED BIRD TO OBSERVER
#3. WHETHER HEARD OR SEEN 
#so all of these columns always have NA in Edwards dataset 

DE_DK_PCs<- bind_rows(PCs2022, DE_pcs)


#check spelling of habitats is the same 
DE_DK_PCs %>%  select("Habitat") %>% unique()
DE_DK_PCs$Habitat = gsub("Once-logged",  "Once_logged", DE_DK_PCs$Habitat)
DE_DK_PCs$Habitat = gsub("Twice-logged",  "Twice_logged", DE_DK_PCs$Habitat)
DE_DK_PCs$Habitat = gsub("Primary ",  "Primary", DE_DK_PCs$Habitat)

#drop blank habitat columns 
DE_DK_PCs <- DE_DK_PCs %>%  filter(!is.na(Habitat)) %>% select(!X)

DanielSp <- DE_DK_PCs %>% filter(Sampler == "Daniel_Kong") %>% filter(abundance >0 ) %>% select(spp) %>% unique()
DaveSp <- DE_DK_PCs %>% filter(Sampler == "David_Edwards") %>% filter(abundance >0 ) %>% select(spp) %>% unique()
#write.csv(DaveSp, "DaveForestPCSppList.csv")

#check species that match and don't match between datasets 
species_match <- semi_join(DanielSp, DaveSp, by ="spp") #82 species match 
daniel_unique <- anti_join(DanielSp, DaveSp, by = "spp") # 11 spp unique to Daniel in forest 
dave_unique <- anti_join(DaveSp, DanielSp, by = "spp") # 49 spp unique to dave 
#write.csv(dave, "forestPCuniqueDanielSpp.csv")


#check the total abundnce of spps seen uniquely by Daniel - send to Dave to sense check IDs
daniel_unique_ab <- daniel_unique %>% left_join(DE_DK_PCs, by = "spp") %>%  filter(abundance >0 )
#write.csv(daniel_unique_ab, "forestPCuniqueDanielSpp.csv")

#3. Add in habitat information ####

#add in time since logging and restoration data
P_R_1L <- DE_DK_PCs %>% filter(Habitat == c("Primary", "Once_logged", "Restored"))

#Add in logging year ####
P_R_1L <- P_R_1L %>%mutate(Logging_year = case_when(Site == "GC1L" ~ 1960,
                                                    Site == "GC1L-2" ~ 1981,
                                                    Site == "GCR" ~ 1981,
                                                    Site == "GCR2" ~ 1991,
                                                    Site == "Danum_1_log" ~ 1989, 
                                                    Site == "INFAPRO_1" ~ 1988,
                                                    Site == "Malu_B_Plot" ~ 1985,
                                                    Site == "Takala_1L" ~ 1989,
                                                    Site == "West_US_1L" ~ 1987))

#Add year of most recently post-logging silviculture for infapro plots
P_R_1L <- P_R_1L %>% mutate(Restoration_Year = case_when(Site == "GCR" ~ 1994,
                                                         Site == "GCR2" ~ 2002,
                                                         Site == "INFAPRO_1" ~ 1992,
                                                         Site == "Malu_B_Plot" ~ 2002))


# calculate time since logging and restoration
P_R_1L <- P_R_1L %>% mutate(Time_Since_Logging = Year - Logging_year, 
                            Time_Since_Restoration = Year - Restoration_Year)


#add this information back into the full dataset
add_back <- P_R_1L %>% select(Site, Time_Since_Logging, Time_Since_Restoration,Restoration_Year, Logging_year) %>% unique()
DE_DK_PCs <- DE_DK_PCs %>% left_join(add_back, by = "Site")

#replace NAs in abundance column with 0 
DE_DK_PCs <- DE_DK_PCs %>% mutate(abundance = ifelse(is.na(abundance), 0, abundance))


#check we have all sites in the data-set
DE_DK_PCs %>% select(Site) %>% unique()

#4. Remove unsampled Species ####
#remove any species for which no abundance is ever recorded (e.g. 0)

DE_DK_PCs <- DE_DK_PCs %>%
  #find only non-0 species
  group_by(spp) %>%
  summarize(sum_abundance = sum(abundance)) %>%
  filter(sum_abundance > 0) %>%
  select(spp) %>% 
  left_join(DE_DK_PCs, by = "spp")

# #how many duplicates  
# DE_DK_PCs %>% 
#   group_by(Site, point, spp, Day, Habitat, Sampler) %>% 
#   count() %>%  
#   filter(n >1)

#write.csv(Outputs/DE_DK_PCs,"all2009_2022PC_data.csv")
#make a csv of all species that were sampled
#allSPP <- DE_DK_PCs %>% select(spp) %>% unique()
#
#write.csv(allSPP, "allSppDaveDkong2009_2022.csv")


#5.Organise Simon Mitchell data ####
Simon
SppCrosswalk


#correct species for which there are spelling-name duplicates in Simon's data 
NamesToCorrect <- SppCrosswalk %>% filter(!is.na(Two_SpellingSimon))
NamesToCorrect

#---------------------incorrect spelling----correct spelling-----------------------                     
Simon$Species = gsub("Blue-crowned_Hanging-parrot","Blue-crowned_Hanging-Parrot", Simon$Species)
Simon$Species = gsub("Bold-striped_Tit-Babbler","Bold-striped_Tit-babbler", Simon$Species)
Simon$Species = gsub("Bornean_Ground-cuckoo","Bornean_Ground-Cuckoo", Simon$Species)
Simon$Species = gsub("Chestnut-backed_Scimitar-Babbler","Chestnut-backed_Scimitar-babbler", Simon$Species)
Simon$Species = gsub("Fluffy-backed_Tit-Babbler","Fluffy-backed_Tit-babbler", Simon$Species)
Simon$Species = gsub("Grey-and-buff_Woodpecker","Grey-and-Buff_Woodpecker", Simon$Species)
Simon$Species = gsub("Grey-chested_Jungle-flycatcher","Grey-chested_Jungle-Flycatcher", Simon$Species)
Simon$Species = gsub("Grey-headed_babbler","Grey-headed_Babbler", Simon$Species)
Simon$Species = gsub("Grey-headed_Canary-Flycatcher","Grey-headed_Canary-flycatcher", Simon$Species)
Simon$Species = gsub("Horsefields_Babbler","Horsfields_Babbler", Simon$Species)

Simon <- Simon %>% rename(spp = Species)
SppCrosswalk<- SppCrosswalk %>% select(DaveSppNames,SimonSppNames)

#understand the data 
#summarise Simon's data 
# #7 unique sites 
# Simon %>% select(Block)  %>% unique()
# #between 9 and 11 points carried out at each site
# pointsPerSite <- Simon %>% select(Block, Safe_._) %>% unique() %>%  group_by(Block) %>% count()
# #between 1 and 5 visits to a point, depending on the site 
# #site RLF2 seems to have been visited only once 
# visitsPerPoint <- Simon %>% select(Block,Safe_._,Date) %>%  unique() %>% group_by(Block,Safe_._) %>% count()


#check and correct several errors in data ####
#1. helmeted hornbill sampled in 20/07/2017 has incorrectly been labelled as from Block DVC1 instead of DVC2
helmeted <- Simon %>% filter(Block == "DVFC1" & Safe_._ =="DVFC2-PC10") %>%  mutate(Block = "DVFC2") #filter and correct 
Simon <- Simon %>% filter(!(Block == "DVFC1" & Safe_._ =="DVFC2-PC10"))  #remove old inccorrect row 
Simon <- Simon %>% rbind(helmeted) #add corrected data back in 

#RLF 2 #looks like this was ONLY SAMLPED ONCE, so we will have to exclude (BUT MESSAGE SIMON)
RLF2 <- Simon %>% filter(Block == "RLF2") %>% select(Block, Safe_._, Date,Session) %>% unique()
RLF2 %>% group_by(Safe_._) %>% count()
Simon <- Simon %>% filter(!(Block == "RLF2")) #remove this site; was only sampled once

#DVC1 -
DVFC1 <- Simon %>% ungroup() %>%  filter(Block == "DVFC1") %>% select(Block, Safe_._, Date,Session,Time) %>% unique()
DVFC1 %>% group_by(Safe_._) %>% count()  #10 counts, visted 4 times 

#DVC2 -
DVFC2 <- Simon %>% ungroup() %>%  filter(Block == "DVFC2") %>% select(Block, Safe_._, Date, Session,Time) %>% unique()
DVFC2 %>% group_by(Safe_._) %>% count()  #10 counts, also visted 4 times 

#LF1 
LF1 <- Simon %>% filter(Block == "LF1") %>% select(Block, Safe_._, Date,Session,Time) %>% unique()
LF1 %>% group_by(Safe_._) %>% count()  #9 points sampled 4 times

#LF2 
LF2 <- Simon %>% filter(Block == "LF2") %>% select(Block, Safe_._, Date,Session,Time) %>% unique()
LF2 %>% group_by(Safe_._) %>% count() #9 points, sampled 4 or 3 times (686 only twice, it seems)
LF2 %>% filter(!(Date == "06/03/2015" )) %>% group_by(Safe_._) %>% count() #consecutive day counts only (could replace 686,688,692 data with points from other time)
lF2_868 <- Simon %>% filter(Block == "LF2" & Safe_._ == 686) %>% group_by(Session) %>% slice(1)
#LF2 IS A BIT WEIRD - LOTS OF MISSING DAY DATA FOR CONSECUTIVE DAYS - AND POINT 686 SEEMS TO HAVE BEEN 
#SAMPLED TWICE ON THE SAME DAY - SO I AM ALSO GOING TO REMOVE THIS SITE.


#LFE
LFE <- Simon %>% filter(Block == "LFE") %>% select(Block, Safe_._, Date, Session,Time) %>% unique()
LFE %>% group_by(Safe_._) %>% count() # 10 points; all sampled three times 

#RLF
RLF <- Simon %>% filter(Block == "RLF") %>% select(Block, Safe_._, Date,Session,Time) %>% unique()
RLF %>% group_by(Safe_._) %>% count() # 10 points; all sampled five times 

#drop2 days from RLF
RLF_sub <- RLF %>% filter(!(Date == "01/05/2014")) %>% filter(!(Date == "30/04/2014")) %>%  select(Block,Date,Safe_._,Session,Time)#remove 2014 data

#combine simon's primary sites
SimonP <- DVFC1 %>% rbind(DVFC2) %>% rbind(LF1) %>% rbind(LFE) %>% rbind(RLF_sub) 

#now Subset Simon's data; if we choose to (here I am subsetting by 3 days but keeping 4 days for primary)
SubsettedSimon <- SimonP %>% left_join(Simon, by = c("Block", "Safe_._", "Date","Time","Session"))

#last check on Simon's data 
X <- SubsettedSimon %>% select(Date, Time, Block, Safe_._, Session) %>% unique

#get information on median days of distance for birds 
Points_Not_Consecutive <- X %>% filter(Block == c("LFE", "LF1", "RLF")) 
Points_Not_Consecutive %>%  select(c("Block", "Safe_._")) %>% count()
#note that by subsetting the data, we are removing  547 individual bird detections
Simon %>% filter(!is.na(Number)) %>% summarise(sum(Number))
SubsettedSimon %>% filter(!is.na(Number)) %>% summarise(sum(Number))
#35 points not collected on consective days


#Match names between Simon and Dave ####

#rename cols and get data in same order 
SubsettedSimon <- SubsettedSimon %>%  rename(
  Site = Block, 
  point = Safe_._, 
  abundance = Number
) %>% select(
  spp, Site,point, abundance, Distance_Band, Time, Date
) %>% 
  rename(SimonSppNames = spp)

#Temporarily remove species uniquely sampled by simon to add in later  #HERE i MAY WANT TO REMOVE SWIFTLETS, SWIFT AND SWALLOWS ONLY RECORDED BY SIMON !!
SimonUniqueDetections <- SppCrosswalk %>% filter(is.na(DaveSppNames)) %>%
  select(SimonSppNames) %>% 
  left_join(SubsettedSimon, by = "SimonSppNames") 
SppCrosswalk <- SppCrosswalk %>% filter(!(is.na(DaveSppNames))) # remove spp for which dave doesnt have a name; we'll add these later 

#join data with crosswalk, applying Daves names  
SubsettedSimon <- SppCrosswalk %>%  left_join(SubsettedSimon, by = "SimonSppNames") %>%  
  filter(!(is.na(abundance))) %>%  
  select(-SimonSppNames) %>%  
  rename(spp = DaveSppNames)

#add simon's unique detection back in 
SubsettedSimon <- SimonUniqueDetections %>% rename(spp = SimonSppNames) %>% rbind(SubsettedSimon)

#Add in Habitat info for Simon's sites 
SubsettedSimon <- SubsettedSimon %>% cbind(Sampler = "SM") %>% 
  mutate(Habitat = case_when(
    Site == "DVFC1" ~ "Primary",
    Site == "DVFC2" ~ "Primary",
    Site == "LF1" ~ "Twice_logged",
    Site == "LF2" ~ "Twice_logged",
    Site == "LFE" ~ "Twice_logged", 
    Site == "RLF" ~  "Twice_logged")) %>% 
  filter(!(is.na(abundance)))

# extract number starting with 201 from date_column to make year column 
SubsettedSimon <- SubsettedSimon %>% 
  mutate(Year = str_extract(Date, "(?<=^|\\D)201\\d(?=\\D|$)"))

#how many duplicates ... quite a lot!
duplicates <- SubsettedSimon %>% 
  group_by(Site, point, spp, Date,  Habitat, Sampler) %>% 
  count() %>%  
  filter(n >1)

#what's going on with duplicates?
duplicate_pts <- duplicates %>%  select(Site, point, spp, Date) %>%  
  left_join(SubsettedSimon)

## Ok, so Simon Mitchell's bit of the dataset, birds at the same point and species but observed 
#in different distance bands are seperate rows; fix these duplicates 
SubsettedSimon <- SubsettedSimon %>%
  group_by(Site, point, spp,Date) %>%
  mutate(abundance = sum(abundance)) %>% 
  slice(1) # this selects only the top distance band (as abundances are the same now)

# #how many duplicates ... fixed!
# duplicates <- SubsettedSimon %>% 
#   group_by(Site, point, spp, Date,  Habitat, Sampler) %>% 
#   count() %>%  
#   filter(n >1)

#6. Combine ALL data ####
#combine David, Daniel and Simon Data
DE_DK_PCs$point <- as.character(DE_DK_PCs$point)
SubsettedSimon$Year <- as.numeric(SubsettedSimon$Year)
all_PCs2009_2022 <- DE_DK_PCs %>% bind_rows(SubsettedSimon)


#run checks
all_spp <- all_PCs2009_2022 %>% select(spp) %>% unique()
all_site <- all_PCs2009_2022 %>% select(Site) %>% unique()
all_hab_pts <- all_PCs2009_2022 %>% select(Site, Habitat,point) %>% unique() %>% group_by(Habitat) %>% count

#26,897 individual bird detections
sum(all_PCs2009_2022$abundance)

#export each site by traps by day measurment so that I can add (1) date (2) time (3) GPS
exportForGPS <- all_PCs2009_2022 %>% select(Site,Habitat,point, Sampler) %>% unique()
colnames(all_PCs2009_2022)
#write.csv(Outputs/exportForGPS, "BirdSitesForGPSPoints.csv")

#export bird data without LiDAR
#write_csv(Outputs/all_PCs2009_2022, "PCs_No_Lidar.csv")
#all_PCs2009_2022 <- read.csv("PCs_No_Lidar.csv")

#7.Add missing times and dates ####
#Add in times and dates for: 1) Daniel 2022 points and 2) Dave point counts
#combine dave and daniel dates 
MissingDates <- rbind(Daniel_DT, DaveDates)
MissingDates$point <- as.character(MissingDates$point)

colnames(all_PCs2009_2022)
colnames(MissingDates)

#add in Dave and Daniels times and Dates, mutating columns appropriately  

all_PCs2009_2022 <- all_PCs2009_2022 %>%
  left_join(MissingDates, by = c("Day", "point", "Site")) %>%
  mutate(Date = if_else(is.na(Date.x), as.character(Date.y), Date.x)) %>% 
  mutate(Time = if_else(is.na(Time.x), as.character(Time.y), Time.x)) %>% 
  select(-c(Time.x, Time.y, Date.x, Date.y))

#8 Add LiDAR for each points ####
#LiDar data is calculated in GEE and uses Asner 2021 (nominal year 2016) Canopy tree height and Above-ground carbon rasters to extract 
#for each point, and at a 50 and 100m buffer each of these values for all of the point counts 
Lidar

#remove "P" added to beginning of each "point" name so that it matches with data 
Lidar <- Lidar %>%  mutate(point = str_sub(point, start = 2)) 

#replace _ with with - to allow join 
Lidar <- Lidar %>%
  mutate(point = str_replace_all(point, "_", "-"))

Lidar$Site <- gsub("GC1L_2", "GC1L-2", Lidar$Site)

all_PCs2009_2022 <- all_PCs2009_2022 %>% ungroup() %>% left_join(Lidar, by = c("Site","point"))

#check we're not missing any lidar for any sites 
all_PCs2009_2022 %>% select(point,Site, ABC50) %>% unique() %>% filter(is.na(ABC50))

#Save all species data with LiDAR ####
#write.csv(Outputs/all_PCs2009_2022,"all_PCs2009_2022.csv")
#all_PCs2009_2022 <- read.csv("all_PCs2009_2022.csv")


# 8a. Add  trait data ####
# Uncomment here if you want to add trait information for species 
# #Add Latin Names ####
# #Add species names in Latin using taxize - nb only need to run once and then can
# #just use the traits output below: 
# traits <- read.csv("Outputs/AllBorneoSpeciesTraits.csv")
# 
# 
# # #use taxiza package to match species names to Birdlife Backbone for getting traits: 
# # allSPP <- all_PCs2009_2022 %>%
# #   ungroup() %>% 
# #   select(spp) %>% 
# #   unique()
# # 
# # #Use the get_tsn() function from the taxize package to get the Taxonomic Serial Number (TSN) for each bird name
# # #RUN ONCE!!
# # #bird_tsn <- lapply(allSPP$spp, get_tsn, taxon = "Aves", db = "itis")
# # #save output 
# # #saveRDS(bird_tsn,"Outputs/TaxiseOutputBirdlifeMatchedNames") 
# # bird_tsn <- read_rds("Outputs/TaxiseOutputBirdlifeMatchedNames")
# # 
# # #make a vector of taxon IDs
# # taxise_id <- unlist(bird_tsn)
# # 
# # #pull down the latin names for spp based on the unique identifier 
# # #bird_scientific <- taxise_id %>% id2name(db = "itis")
# # #saveRDS(Outputs/bird_scientific, "ScientificNamesFromTaxise")
# # 
# # #read in ScientificNames from Taxise: 
# # bird_scientific <-  readRDS("Outputs/ScientificNamesFromTaxise")
# # #bind list into one dataframe 
# # bird_scientific <- do.call(rbind, bird_scientific) %>% as.data.frame() 
# # 
# # #join to common names 
# # allSPP <- cbind(allSPP, bird_scientific)  
# # allSPP <- allSPP %>% rename(LatinName = name)
# # 
# # #species that came up as NA in the taxize database (export these and match them manually)
# # allSPP_NA <- allSPP %>% filter(is.na(LatinName)) %>% select(spp, LatinName)
# # 
# # #write.csv(allSPP_NA, "Outputs/allSppsNaToFindLatinNamesForToGetTraits.csv")
# # 
# # #  Add species traits from AVONET ####
# # 
# # #read in the Trait data from AVINET database (using the Birdlife names for birds)
# # traitsraw <- read.csv("Inputs/AVONET_birdlife.csv") %>% rename(LatinName = Species1, 
# #                                                         HabPref = Habitat)
# # 
# # #carry out a fuzzy left join 
# # traits_auto<- allSPP %>% fuzzy_left_join(traitsraw, 
# #                                          by = c("LatinName" = "LatinName"), 
# #                                          match_fun = str_detect) %>% 
# #   filter(!(is.na(LatinName.x))) #remove spps NA - we add these in from manual later
# # 
# # 
# # 
# # #read in the species I had to manually find the Birdlife Latin names for
# # #this includes (1)Spp that were NA in the taxize pulldown and (2) Species incorrectly named by taxize that don't match birdlife AVINET 
# # NaSpp <- read.csv("NaSppLatinNames.csv")
# # Natraits <- NaSpp %>% fuzzy_left_join(traitsraw, 
# #                                       by = c("LatinName" = "LatinName"), 
# #                                       match_fun = str_detect)
# # 
# # colnames(Natraits)
# # colnames(traits_auto)
# # 
# # #combine all trait data - making sure to replace spp in traits_automatic with manually corrected NaTraits
# # traits <- bind_rows(traits_auto, Natraits) %>% filter(!is.na(Sequence))
# # 
# # #clean trait information 
# # traits <- traits %>% select(spp, LatinName.x, 9:43) %>% rename(LatinName =LatinName.x ) %>% 
# #   select(-c(  "Avibase.ID1","Total.individuals" ,"Female","Male" ,"Unknown","Complete.measures", "Mass.Source" ,"Mass.Source",
# #               "Mass.Refs.Other","Inference", "Traits.inferred"))
# # #swiftlet Sp. is missing - remove 
# #
# #write.csv(traits, "AllBorneoSpeciesTraits.csv")
# 
# #join traits  with bird data #### 
# PointsWithTraits <- all_PCs2009_2022 %>% left_join(traits, by = "spp",relationship = "many-to-many")
# write.csv(PointsWithTraits, "Outputs/All2009_2022_PCs_withTrait_data")
# 
# #have all species been detected at least one? YES - all species have been detected
# x  <- PointsWithTraits %>% select(spp, abundance) %>% group_by(spp) %>% summarise(ab = sum(abundance))

#Final points with traits
#all_PCs2009_2022 <- PointsWithTraits



# 9. Add species forest dependencies

#check species that match and don't match between datasets 
species_match <- semi_join(df_IUCN_FD, all_PCs2009_2022, by ="spp") #211 species match 
dep_only <- anti_join(df_IUCN_FD, all_PCs2009_2022, by = "spp") # 11 spp unique to Daniel in forest 
missing_species <- all_PCs2009_2022 %>%
  anti_join(df_IUCN_FD, by = "spp") %>% select(spp) %>% unique()
missing_species

#remove the 4 species we don't have dependency data for (they're incorrect sp): 
# Remove missing species from all_PCs2009_2022
all_PCs2009_2022 <- all_PCs2009_2022 %>%
  filter(!(spp %in% missing_species$spp))

all_PCs2009_2022 <- all_PCs2009_2022 %>%
  left_join(df_IUCN_FD, by = "spp")

# 9. Filter distances for direct comparison ####
#filter out any of Daniel's data that is >100m away to enable matching across datesets 
all_PCs2009_2022 <- all_PCs2009_2022 %>%
  filter(!(Sampler == "Daniel_Kong" & distance > 100)) %>% 
  mutate(Time = sub("(:[0-9]{2}):[0-9]{2}$", "\\1", Time))## remove seconds in Simon mitchell's Time measure


#10. downstream corrections ####
##Check we're not missing times in unexpected places
xx <-  all_PCs2009_2022 %>%  select(point,Site,Sampler,Day,Time) %>% unique


#which columns have NAs - Simons PCs are missing day information (correct below)
all_PCs2009_2022 %>%
  summarise(across(everything(), ~any(is.na(.)))) %>%
  gather(key = "column", value = "has_na") %>%
  filter(has_na)

#GC1L-2, pt 9, day 2 is missing (we didn't #have time at the end of the field season to re-sample)
X <- all_PCs2009_2022 %>% ungroup()  %>% select(point,Site,Sampler,Day,Time) %>% unique %>% 
  filter(Day == 2 & Site == "GC1L-2" )

#Edit date column 
all_PCs2009_2022 <- all_PCs2009_2022 %>%
  as_tibble() %>%
  # convert date column to date format
  mutate(Date = as.Date(Date, format = "%d/%m/%Y")) 

## (1) fix timing formatting
all_PCs2009_2022 <- all_PCs2009_2022 %>%
  mutate(hours = gsub("^([0-9][0-9]).*", "\\1", Time), 
         minutes = gsub("^[0-9][0-9](.*)", "\\1", Time), 
         minutes = gsub("([0-9][0-9]):00", "\\1", minutes),
         minutes = gsub("\\.|\\:", "", minutes), 
         # as numeric
         hours = as.numeric(hours), 
         # note that a couple of visits are missing times: this needs fixing. When 
         # fixed remove this line 
         hours = ifelse(is.na(hours), 6, hours),
         minutes = as.numeric(minutes),
         hps = (hours + minutes/60) - 6, 
         # format date_time in order to sort out visit ID 
         minutes_char = as.character(minutes), 
         minutes_char = ifelse(nchar(minutes_char) == 1, 
                               paste0("0", minutes_char), 
                               minutes_char),
         date_time = as.POSIXct(paste0(Date, "-", hours, "-", minutes_char), 
                                format = "%Y-%m-%d-%H-%M")) %>%  
          mutate(point_id = interaction(Site, point))

## Note: worth visual checking that these split out hours and minutes properly
all_PCs2009_2022 %>%
  select(Site, point,Time, hours, minutes, date_time) %>%
  unique %>% 
  View()

# #how many duplicates?
# dup <- all_PCs2009_2022 %>% 
#   group_by(Site, point, spp, Date, Sampler) %>% 
#   count() %>% 
#   filter(n >1)


#2. Fix missing days (for Simon's part of the data)

date_info <- all_PCs2009_2022 %>%
  select(Site, point_id, Date, date_time, Time, Day, hps) %>%
  unique %>%
  group_by(Site, point_id) %>%
  # there are some point counts within the same hour.... worth checking on this
  mutate(Day = rank(date_time))

#add back in dates (all sites should have Day info now, including Simon)
all_PCs2009_2022 <- all_PCs2009_2022 %>% 
  select(-Day) %>% 
  left_join(date_info)

# #which columns have NAs - Simons PCs are missing day information (correct below)
all_PCs2009_2022 %>%
  summarise(across(everything(), ~any(is.na(.)))) %>%
  gather(key = "column", value = "has_na") %>%
  filter(has_na) #FIXED

#11. Create 0-filled dataset ####

## generate backbone dataframe with all point:visit:species combinations
df_backbone <- with(all_PCs2009_2022, expand.grid(point_id = unique(point_id), 
                                     Day = 1:4,
                                    spp = unique(spp)))

#Nb; we must differentiate between:
#(1) points-visits-species that were detected = 1 
#(2) point-visit-species that were technically possible, but abundance was 0 = 0
#(3) point-visit-species that were impossible as the visit wasn't made = NA


## make a dataframe of detections (e.g. points-visits-species= 1)
df_non0 <- all_PCs2009_2022 %>%
  filter(abundance > 0) %>% 
  mutate(point_visited = 1) %>%  
  select(Site,point_id, point_visited,spp,Day, abundance)


## join to create full dataframe where abundance is 0 if point visit was made 
## but species not detected, remains as abundance val if species detected, and NA if point visit 
## doesn't exist
df_full <- date_info %>%
  mutate(point_visited = 1) %>% # these are all the points visited (i.e. recorded 1s and 0s)
  left_join(df_backbone, .) %>% #take potential points, and combine with all point visited
                                #if day = Na, this is a point-visit-species that was impossible as the visit wasn't made
  
  left_join(., 
            df_non0[on =c("Site", "point_id", "Day", "spp","abundance")]) %>%
  mutate(abundance = ifelse(is.na(abundance) & point_visited==1, 0, abundance))

#check missing day*pt combos 
#good missing day 4 stuff only (most sites only sampled 3 days),
#plus GC1L-2.9  day 3 
#and GCL2.9 day  3 - this is an error, COME BACK TO THIS DAY*POINT

check <- df_full %>%
  filter(if_any(everything(), is.na)) %>% select(point_id, Day) %>% unique()


names(all_PCs2009_2022)
hab_covars <- all_PCs2009_2022 %>% select(point_id, Site, Year, Habitat,Sampler,
                                          Time_Since_Logging, Time_Since_Restoration,
                                          Latitude, Longitude, ABC50, ABC100) %>% unique()

sp_covars <-  all_PCs2009_2022 %>% select(spp, forestDependency, IUCN,nonResident) %>% unique()  

#add in the extra information 
full_birds_flockr <- df_full %>% 
  left_join(hab_covars) %>%  
  left_join(sp_covars) %>% 
#add in plantation age information 
  # join in plantation age info
  left_join(., df_habitat, by = c("point_id")) %>%
  rename(plantation_age = Age) 

names(full_birds_flockr)

#12. Export data ####

#Export full abundances dataframe
write.csv(all_PCs2009_2022, "Outputs/allDanielDaveSimonDetectionsWithLidar.csv")

#explort dataframe ready for flockr, in next script
write.csv(full_birds_flockr, "Outputs/birdDataForFlockr.csv")



=======
# 21.02.2023 
#This code: 
#1 Brings together all raw site-level point count data from forest and plantation sites coordinated by Cerullo in 2022 
#and carried out by Daniel Kong into one tidy dataframe, then... 
#2. Combines this with historic forest PC data collected by David Edwards and then
#3. comines with Simon Mitchells logged and old-growth forest data.

#these data are then joined with a LiDAR above-ground carbon and tree canopy height information from each point 
#LiDAR data are calculated for 50m and 100m buffers around each point in Google Earth Engine. 

#THere is a commented out section of code that also extract trait data for all species based on the AVONET portal 

rm(list = ls())

library(usethis)
library(stringr)
library(tidyr)
library(ggplot2)
library(data.table)
library(lubridate)
library(taxize)
library(fuzzyjoin)
library(tidyverse)
library(flocker)

#read in Data ####

#############  POINT COUNT DATA ###############

#1: Dave Historic Point Counts
DE_pcs <- read.csv( "RawData/DE_2008_2011_forest_pointCounts.csv") %>% 
  rename(Day = day)
#2 Daniel Kong plantation data  
plantations <- read.csv("RawData/plantationPCs_2022.csv")
#3 read in Simon Mitchell data
Simon <- read.csv("RawData/SimonMitchellSelectedPointCounts.csv")

#4 read in xa crosswalk that translates Simon and Daves spp to same matching names 
SppCrosswalk <- read.csv("Inputs/SimonDaveSpeciesNamesCrossWalk.csv",na.strings = "")

#5  Read in raw point count data from 2022 forest sites
file_list<-list.files('RawData/all_PC_forest_site_CSVs_2022',
                      pattern = '*.csv',
                      full.names = T)

#6 Get list of species that were not detected in Dave's point counts
undetectedSppDave <- DE_pcs %>% 
  mutate(abundance =  ifelse(is.na(abundance),0, abundance)) %>%  #replace NA abundance with 0
  group_by(spp) %>%
  summarise(sum(abundance)) %>%
  rename(totalSampledInd = 2) %>% 
  filter(totalSampledInd == 0) %>% 
  dplyr::select(spp)



################## LIDAR DATA ####################


Lidar <- read.csv("Inputs/GEE_AllPointCountsABC_CTH_50m_100m_buffer.csv") %>% 
  mutate(Habitat = case_when(
    Site == "LF1" ~ "Twice_logged",
    Site == "LF2" ~ "Twice_logged",
    Site == "LFE" ~ "Twice_logged", 
    Site == "RLF" ~  "Twice_logged", 
    TRUE ~ Habitat
  )) %>% 
  select(Site, point, Latitude, Longitude, ABC50, ABC100, CTH50, CTH100)


############ EXTRA PC DATE AND TIME INFORMATION ###############
#read in times and dates for Daniel
Daniel_DT <- read.csv("Inputs/PC_Dates_Times_2022.csv") %>% mutate(Site = case_when(
  Site == "GCAL1" ~ "AL1",
  Site == "GCAL2" ~ "AL2", 
  Site == "GCAL3" ~ "AL3", 
  Site == "GCEC1" ~ "EC1", 
  Site == "GCEC2" ~ "EC2", 
  Site == "GCEC3" ~ "EC3", 
  Site == "GC2L" ~ "GCL2", 
  TRUE ~ Site
))

#Dave missing dates and times 
DaveDates <- read.csv("Inputs/TimesDatesDavePointCounts.csv") %>% rename(Day = day) %>% select(-X) 

############### PLANTATION AGE ##################

#plantation age information
df_habitat <- data.table::fread("Inputs/Plantation_Habitat_Structure.csv") %>%
  as_tibble() %>%
  mutate(point_id = interaction(Site, Point)) %>%  
  select(point_id, Age)

############## Sp THREATS & DEPENDENCIES ###################

#iucn status and forest dependency:low,medium,high (none reclassified to low)  (non-resident = 1; we get rid of these 6 spp)
df_IUCN_FD <- data.table::fread("Inputs/SppForForIntegrityIUCN.csv") %>%
  filter(nonResident == 0)


################### Functions ##################

# Function to pivot longer and process data
process_data <- function(data, type) {
  data %>%
    rename(spp = Other.Information) %>%
    select(spp, Site, Habitat, Day, starts_with(type)) %>%
    pivot_longer(cols = -c(spp, Site, Habitat, Day),
                 names_to = "point",
                 names_prefix = paste0(type, "."),
                 values_to = type) %>%
    mutate(point = as.numeric(gsub(paste0(type, "."), "", point)) + 1) %>%
    filter(spp != "Point Count" & spp != "Species")
}



#CODE BEGINS ####
# 1. Organise the 2022 data #####

# Read and combine data files into a list
my_files <- map(file_list, read.csv) %>%
  setNames(nm = .)

#turn each raw datfilea sheet into an element of a list
my_files<-lapply(file_list, read.csv) %>% 
  setNames(nm = .)

#bring all together into one dataframe 
wide_df<- rbindlist(my_files, fill = TRUE)

# Get abundances 
Abundances <- wide_df %>%   
  rename(spp = Other.Information) %>%      #remove unneeded cols
  select(spp, Site, Habitat, Day, starts_with("abundance")) %>% 
  pivot_longer(cols = !c(spp,Site, Habitat, Day),
               names_to = "point", 
               names_prefix = "Abundance.",
               values_to = "abundance") %>% 
  
  #this bit of code renumbers points so that they are correct
  # turn "abundance" to 0 (point 1 is currently called abundance)
  mutate(point =  ifelse(point == "Abundance",0, point))  %>% 
  # reclassify column to numeric and add 1 to make points correct
  mutate(point = as.numeric(point)+1) %>% 
  #this bit of code removes species called "Point count" and "Species
  filter(spp != "Point Count") %>% 
  filter(spp != "Species")

#Get distances

Distances <- wide_df %>%   
  rename(spp = Other.Information) %>%      #remove unneeded cols
  select(spp, Site, Habitat, Day, starts_with("Distance")) 

# #check columns are numeric or pivot longer won't work!
 Distances$Distance.2 <- as.numeric(Distances$Distance.2)

Distances <- Distances %>% pivot_longer(cols = !c(spp,Site, Habitat, Day),
                                        names_to = "point", 
                                        names_prefix = "Distance.",
                                        values_to = "distance") %>% 
  #this bit of code renumbers points so that they are correct
  # turn "abundance" to 0 (point 1 is currently called abundance)
  mutate(point =  ifelse(point == "Distance",0, point))  %>% 
  #this bit of code removes species called "Point count" and "Species
  filter(spp != "Point Count") %>% 
  filter(spp != "Species") %>% 
  # reclassify column to numeric and add 1 to make points correct
  mutate(point = as.numeric(point)+1) 

#get flyovers
Flyovers <-  wide_df %>%   
  rename(spp = Other.Information) %>%      #remove unneeded cols
  select(spp, Site, Habitat, Day, starts_with("Flyover")) 

#make sure columns are numeric 
Flyovers$Flyover.1 <- as.numeric(Flyovers$Flyover.1)

Flyovers <- Flyovers %>% pivot_longer(cols = !c(spp,Site, Habitat, Day),
                                      names_to = "point", 
                                      names_prefix = "Flyover.",
                                      values_to = "flyover") %>% 
  #this bit of code renumbers points so that they are correct
  # turn "abundance" to 0 (point 1 is currently called abundance)
  mutate(point =  ifelse(point == "Flyover",0, point))  %>% 
  #this bit of code removes species called "Point count" and "Species
  filter(spp != "Point Count") %>% 
  filter(spp != "Species") %>% 
  # reclassify column to numeric and add 1 to make points correct
  mutate(point = as.numeric(point)+1) 


# heard or soon 
Heard.Seen <- wide_df %>%   
  rename(spp = Other.Information) %>%      #remove unneeded cols
  select(spp, Site, Habitat, Day, starts_with("H.S.HS")) %>% 
  pivot_longer(cols = !c(spp,Site, Habitat, Day),
               names_to = "point", 
               names_prefix = "H.S.HS.",
               values_to = "H.S.HS") %>% 
  #this bit of code renumbers points so that they are correct
  # turn "abundance" to 0 (point 1 is currently called abundance)
  mutate(point =  ifelse(point == "H.S.HS",0, point))  %>% 
  #this bit of code removes species called "Point count" and "Species
  filter(spp != "Point Count") %>% 
  filter(spp != "Species") %>% 
  # reclassify column to numeric and add 1 to make points correct
  mutate(point = as.numeric(point)+1)

#combine all Daniel's data  
forestPCs <- Abundances %>% left_join(Distances)%>% 
  left_join(Heard.Seen) %>% 
  left_join(Flyovers) 

forestPCs <- forestPCs %>% cbind(Sampler = "Daniel_Kong") %>% cbind(Year = 2022)

#rename "Primary" so that they are spelled the same 
forestPCs$Habitat <- gsub("Primary ",  "Primary", forestPCs$Habitat)


#Add in  2022 plantation data ####   
colnames(plantations)
colnames(forestPCs)

#replace plantation NAs in abundance with 0 
plantations <- plantations %>% mutate(abundance = ifelse(is.na(abundance), 0, abundance))

sum(plantations$abundance)
sum(forestPCs$abundance)


PCs2022 <- bind_rows(forestPCs, plantations) %>% select(-X)

#how many duplicates
PCs2022 %>% 
  group_by(Site, point, spp, Day, Habitat, Sampler) %>% 
  count() %>%  
  filter(n >1)

#checks
PCs2022 %>%  select(Site) %>% unique()
PCs2022 %>%  select(Habitat) %>% unique()

##remove strange Daniel Unique spp 
#. remove single hill blue flycatcher - likely an error
#. remove yell0w-bellied warbler - likely an error 

PCs2022 <- PCs2022 %>% filter(!spp == "Yellow-bellied Warbler")
PCs2022 <- PCs2022 %>% filter(!spp == "Red Jungle Fowl")
PCs2022 <- PCs2022 %>% filter(!spp == "Red jungle Fowl")
PCs2022 <- PCs2022 %>% filter(!spp == "Hill Blue-Flycatcher")
PCs2022 <- PCs2022 %>% filter(!spp == "NA")
PCs2022 <- PCs2022 %>% filter(!spp == "Javan Myna")

XX <- PCs2022 %>%  filter(Site == "GCL2"& point ==9 & Day==3)

#how many duplicates  
PCs2022 %>% 
  group_by(Site, point, spp, Day, Habitat, Sampler) %>% 
  count() %>%  
  filter(n >1)

#2.Organise Edwards Data ####

DE_pcs

#how many duplicates  
DE_pcs %>% 
  group_by(Site, point, spp, Day, Habitat, Sampler) %>% 
  count() %>%  
  filter(n >1)

#check colnames in Edwards and Kong PC to check spelling is same for cols 
colnames(DE_pcs)
colnames(PCs2022)


#combine edwards and kong point counts 
##NB; edward's: data doesn't have
#1. FLYOVERS
#2. DISTANCE OF OBSERVED BIRD TO OBSERVER
#3. WHETHER HEARD OR SEEN 
#so all of these columns always have NA in Edwards dataset 

DE_DK_PCs<- bind_rows(PCs2022, DE_pcs)


#check spelling of habitats is the same 
DE_DK_PCs %>%  select("Habitat") %>% unique()
DE_DK_PCs$Habitat = gsub("Once-logged",  "Once_logged", DE_DK_PCs$Habitat)
DE_DK_PCs$Habitat = gsub("Twice-logged",  "Twice_logged", DE_DK_PCs$Habitat)
DE_DK_PCs$Habitat = gsub("Primary ",  "Primary", DE_DK_PCs$Habitat)

#drop blank habitat columns 
DE_DK_PCs <- DE_DK_PCs %>%  filter(!is.na(Habitat)) %>% select(!X)

DanielSp <- DE_DK_PCs %>% filter(Sampler == "Daniel_Kong") %>% filter(abundance >0 ) %>% select(spp) %>% unique()
DaveSp <- DE_DK_PCs %>% filter(Sampler == "David_Edwards") %>% filter(abundance >0 ) %>% select(spp) %>% unique()
#write.csv(DaveSp, "DaveForestPCSppList.csv")

#check species that match and don't match between datasets 
species_match <- semi_join(DanielSp, DaveSp, by ="spp") #82 species match 
daniel_unique <- anti_join(DanielSp, DaveSp, by = "spp") # 11 spp unique to Daniel in forest 
dave_unique <- anti_join(DaveSp, DanielSp, by = "spp") # 49 spp unique to dave 
#write.csv(dave, "forestPCuniqueDanielSpp.csv")


#check the total abundnce of spps seen uniquely by Daniel - send to Dave to sense check IDs
daniel_unique_ab <- daniel_unique %>% left_join(DE_DK_PCs, by = "spp") %>%  filter(abundance >0 )
#write.csv(daniel_unique_ab, "forestPCuniqueDanielSpp.csv")

#3. Add in habitat information ####

#add in time since logging and restoration data
P_R_1L <- DE_DK_PCs %>% filter(Habitat == c("Primary", "Once_logged", "Restored"))

#Add in logging year ####
P_R_1L <- P_R_1L %>%mutate(Logging_year = case_when(Site == "GC1L" ~ 1960,
                                                    Site == "GC1L-2" ~ 1981,
                                                    Site == "GCR" ~ 1981,
                                                    Site == "GCR2" ~ 1991,
                                                    Site == "Danum_1_log" ~ 1989, 
                                                    Site == "INFAPRO_1" ~ 1988,
                                                    Site == "Malu_B_Plot" ~ 1985,
                                                    Site == "Takala_1L" ~ 1989,
                                                    Site == "West_US_1L" ~ 1987))

#Add year of most recently post-logging silviculture for infapro plots
P_R_1L <- P_R_1L %>% mutate(Restoration_Year = case_when(Site == "GCR" ~ 1994,
                                                         Site == "GCR2" ~ 2002,
                                                         Site == "INFAPRO_1" ~ 1992,
                                                         Site == "Malu_B_Plot" ~ 2002))


# calculate time since logging and restoration
P_R_1L <- P_R_1L %>% mutate(Time_Since_Logging = Year - Logging_year, 
                            Time_Since_Restoration = Year - Restoration_Year)


#add this information back into the full dataset
add_back <- P_R_1L %>% select(Site, Time_Since_Logging, Time_Since_Restoration,Restoration_Year, Logging_year) %>% unique()
DE_DK_PCs <- DE_DK_PCs %>% left_join(add_back, by = "Site")

#replace NAs in abundance column with 0 
DE_DK_PCs <- DE_DK_PCs %>% mutate(abundance = ifelse(is.na(abundance), 0, abundance))


#check we have all sites in the data-set
DE_DK_PCs %>% select(Site) %>% unique()

#4. Remove unsampled Species ####
#remove any species for which no abundance is ever recorded (e.g. 0)

DE_DK_PCs <- DE_DK_PCs %>%
  #find only non-0 species
  group_by(spp) %>%
  summarize(sum_abundance = sum(abundance)) %>%
  filter(sum_abundance > 0) %>%
  select(spp) %>% 
  left_join(DE_DK_PCs, by = "spp")

# #how many duplicates  
# DE_DK_PCs %>% 
#   group_by(Site, point, spp, Day, Habitat, Sampler) %>% 
#   count() %>%  
#   filter(n >1)

#write.csv(Outputs/DE_DK_PCs,"all2009_2022PC_data.csv")
#make a csv of all species that were sampled
#allSPP <- DE_DK_PCs %>% select(spp) %>% unique()
#
#write.csv(allSPP, "allSppDaveDkong2009_2022.csv")


#5.Organise Simon Mitchell data ####
Simon
SppCrosswalk


#correct species for which there are spelling-name duplicates in Simon's data 
NamesToCorrect <- SppCrosswalk %>% filter(!is.na(Two_SpellingSimon))
NamesToCorrect

#---------------------incorrect spelling----correct spelling-----------------------                     
Simon$Species = gsub("Blue-crowned_Hanging-parrot","Blue-crowned_Hanging-Parrot", Simon$Species)
Simon$Species = gsub("Bold-striped_Tit-Babbler","Bold-striped_Tit-babbler", Simon$Species)
Simon$Species = gsub("Bornean_Ground-cuckoo","Bornean_Ground-Cuckoo", Simon$Species)
Simon$Species = gsub("Chestnut-backed_Scimitar-Babbler","Chestnut-backed_Scimitar-babbler", Simon$Species)
Simon$Species = gsub("Fluffy-backed_Tit-Babbler","Fluffy-backed_Tit-babbler", Simon$Species)
Simon$Species = gsub("Grey-and-buff_Woodpecker","Grey-and-Buff_Woodpecker", Simon$Species)
Simon$Species = gsub("Grey-chested_Jungle-flycatcher","Grey-chested_Jungle-Flycatcher", Simon$Species)
Simon$Species = gsub("Grey-headed_babbler","Grey-headed_Babbler", Simon$Species)
Simon$Species = gsub("Grey-headed_Canary-Flycatcher","Grey-headed_Canary-flycatcher", Simon$Species)
Simon$Species = gsub("Horsefields_Babbler","Horsfields_Babbler", Simon$Species)

Simon <- Simon %>% rename(spp = Species)
SppCrosswalk<- SppCrosswalk %>% select(DaveSppNames,SimonSppNames)

#understand the data 
#summarise Simon's data 
# #7 unique sites 
# Simon %>% select(Block)  %>% unique()
# #between 9 and 11 points carried out at each site
# pointsPerSite <- Simon %>% select(Block, Safe_._) %>% unique() %>%  group_by(Block) %>% count()
# #between 1 and 5 visits to a point, depending on the site 
# #site RLF2 seems to have been visited only once 
# visitsPerPoint <- Simon %>% select(Block,Safe_._,Date) %>%  unique() %>% group_by(Block,Safe_._) %>% count()


#check and correct several errors in data ####
#1. helmeted hornbill sampled in 20/07/2017 has incorrectly been labelled as from Block DVC1 instead of DVC2
helmeted <- Simon %>% filter(Block == "DVFC1" & Safe_._ =="DVFC2-PC10") %>%  mutate(Block = "DVFC2") #filter and correct 
Simon <- Simon %>% filter(!(Block == "DVFC1" & Safe_._ =="DVFC2-PC10"))  #remove old inccorrect row 
Simon <- Simon %>% rbind(helmeted) #add corrected data back in 

#RLF 2 #looks like this was ONLY SAMLPED ONCE, so we will have to exclude (BUT MESSAGE SIMON)
RLF2 <- Simon %>% filter(Block == "RLF2") %>% select(Block, Safe_._, Date,Session) %>% unique()
RLF2 %>% group_by(Safe_._) %>% count()
Simon <- Simon %>% filter(!(Block == "RLF2")) #remove this site; was only sampled once

#DVC1 -
DVFC1 <- Simon %>% ungroup() %>%  filter(Block == "DVFC1") %>% select(Block, Safe_._, Date,Session,Time) %>% unique()
DVFC1 %>% group_by(Safe_._) %>% count()  #10 counts, visted 4 times 

#DVC2 -
DVFC2 <- Simon %>% ungroup() %>%  filter(Block == "DVFC2") %>% select(Block, Safe_._, Date, Session,Time) %>% unique()
DVFC2 %>% group_by(Safe_._) %>% count()  #10 counts, also visted 4 times 

#LF1 
LF1 <- Simon %>% filter(Block == "LF1") %>% select(Block, Safe_._, Date,Session,Time) %>% unique()
LF1 %>% group_by(Safe_._) %>% count()  #9 points sampled 4 times

#LF2 
LF2 <- Simon %>% filter(Block == "LF2") %>% select(Block, Safe_._, Date,Session,Time) %>% unique()
LF2 %>% group_by(Safe_._) %>% count() #9 points, sampled 4 or 3 times (686 only twice, it seems)
LF2 %>% filter(!(Date == "06/03/2015" )) %>% group_by(Safe_._) %>% count() #consecutive day counts only (could replace 686,688,692 data with points from other time)
lF2_868 <- Simon %>% filter(Block == "LF2" & Safe_._ == 686) %>% group_by(Session) %>% slice(1)
#LF2 IS A BIT WEIRD - LOTS OF MISSING DAY DATA FOR CONSECUTIVE DAYS - AND POINT 686 SEEMS TO HAVE BEEN 
#SAMPLED TWICE ON THE SAME DAY - SO I AM ALSO GOING TO REMOVE THIS SITE.


#LFE
LFE <- Simon %>% filter(Block == "LFE") %>% select(Block, Safe_._, Date, Session,Time) %>% unique()
LFE %>% group_by(Safe_._) %>% count() # 10 points; all sampled three times 

#RLF
RLF <- Simon %>% filter(Block == "RLF") %>% select(Block, Safe_._, Date,Session,Time) %>% unique()
RLF %>% group_by(Safe_._) %>% count() # 10 points; all sampled five times 

#drop2 days from RLF
RLF_sub <- RLF %>% filter(!(Date == "01/05/2014")) %>% filter(!(Date == "30/04/2014")) %>%  select(Block,Date,Safe_._,Session,Time)#remove 2014 data

#combine simon's primary sites
SimonP <- DVFC1 %>% rbind(DVFC2) %>% rbind(LF1) %>% rbind(LFE) %>% rbind(RLF_sub) 

#now Subset Simon's data; if we choose to (here I am subsetting by 3 days but keeping 4 days for primary)
SubsettedSimon <- SimonP %>% left_join(Simon, by = c("Block", "Safe_._", "Date","Time","Session"))

#last check on Simon's data 
X <- SubsettedSimon %>% select(Date, Time, Block, Safe_._, Session) %>% unique

#get information on median days of distance for birds 
Points_Not_Consecutive <- X %>% filter(Block == c("LFE", "LF1", "RLF")) 
Points_Not_Consecutive %>%  select(c("Block", "Safe_._")) %>% count()
#note that by subsetting the data, we are removing  547 individual bird detections
Simon %>% filter(!is.na(Number)) %>% summarise(sum(Number))
SubsettedSimon %>% filter(!is.na(Number)) %>% summarise(sum(Number))
#35 points not collected on consective days


#Match names between Simon and Dave ####

#rename cols and get data in same order 
SubsettedSimon <- SubsettedSimon %>%  rename(
  Site = Block, 
  point = Safe_._, 
  abundance = Number
) %>% select(
  spp, Site,point, abundance, Distance_Band, Time, Date
) %>% 
  rename(SimonSppNames = spp)

#Temporarily remove species uniquely sampled by simon to add in later  #HERE i MAY WANT TO REMOVE SWIFTLETS, SWIFT AND SWALLOWS ONLY RECORDED BY SIMON !!
SimonUniqueDetections <- SppCrosswalk %>% filter(is.na(DaveSppNames)) %>%
  select(SimonSppNames) %>% 
  left_join(SubsettedSimon, by = "SimonSppNames") 
SppCrosswalk <- SppCrosswalk %>% filter(!(is.na(DaveSppNames))) # remove spp for which dave doesnt have a name; we'll add these later 

#join data with crosswalk, applying Daves names  
SubsettedSimon <- SppCrosswalk %>%  left_join(SubsettedSimon, by = "SimonSppNames") %>%  
  filter(!(is.na(abundance))) %>%  
  select(-SimonSppNames) %>%  
  rename(spp = DaveSppNames)

#add simon's unique detection back in 
SubsettedSimon <- SimonUniqueDetections %>% rename(spp = SimonSppNames) %>% rbind(SubsettedSimon)

#Add in Habitat info for Simon's sites 
SubsettedSimon <- SubsettedSimon %>% cbind(Sampler = "SM") %>% 
  mutate(Habitat = case_when(
    Site == "DVFC1" ~ "Primary",
    Site == "DVFC2" ~ "Primary",
    Site == "LF1" ~ "Twice_logged",
    Site == "LF2" ~ "Twice_logged",
    Site == "LFE" ~ "Twice_logged", 
    Site == "RLF" ~  "Twice_logged")) %>% 
  filter(!(is.na(abundance)))

# extract number starting with 201 from date_column to make year column 
SubsettedSimon <- SubsettedSimon %>% 
  mutate(Year = str_extract(Date, "(?<=^|\\D)201\\d(?=\\D|$)"))

#how many duplicates ... quite a lot!
duplicates <- SubsettedSimon %>% 
  group_by(Site, point, spp, Date,  Habitat, Sampler) %>% 
  count() %>%  
  filter(n >1)

#what's going on with duplicates?
duplicate_pts <- duplicates %>%  select(Site, point, spp, Date) %>%  
  left_join(SubsettedSimon)

## Ok, so Simon Mitchell's bit of the dataset, birds at the same point and species but observed 
#in different distance bands are seperate rows; fix these duplicates 
SubsettedSimon <- SubsettedSimon %>%
  group_by(Site, point, spp,Date) %>%
  mutate(abundance = sum(abundance)) %>% 
  slice(1) # this selects only the top distance band (as abundances are the same now)

# #how many duplicates ... fixed!
# duplicates <- SubsettedSimon %>% 
#   group_by(Site, point, spp, Date,  Habitat, Sampler) %>% 
#   count() %>%  
#   filter(n >1)

#6. Combine ALL data ####
#combine David, Daniel and Simon Data
DE_DK_PCs$point <- as.character(DE_DK_PCs$point)
SubsettedSimon$Year <- as.numeric(SubsettedSimon$Year)
all_PCs2009_2022 <- DE_DK_PCs %>% bind_rows(SubsettedSimon)


#run checks
all_spp <- all_PCs2009_2022 %>% select(spp) %>% unique()
all_site <- all_PCs2009_2022 %>% select(Site) %>% unique()
all_hab_pts <- all_PCs2009_2022 %>% select(Site, Habitat,point) %>% unique() %>% group_by(Habitat) %>% count

#26,897 individual bird detections
sum(all_PCs2009_2022$abundance)

#export each site by traps by day measurment so that I can add (1) date (2) time (3) GPS
exportForGPS <- all_PCs2009_2022 %>% select(Site,Habitat,point, Sampler) %>% unique()
colnames(all_PCs2009_2022)
#write.csv(Outputs/exportForGPS, "BirdSitesForGPSPoints.csv")

#export bird data without LiDAR
#write_csv(Outputs/all_PCs2009_2022, "PCs_No_Lidar.csv")
#all_PCs2009_2022 <- read.csv("PCs_No_Lidar.csv")

#7.Add missing times and dates ####
#Add in times and dates for: 1) Daniel 2022 points and 2) Dave point counts
#combine dave and daniel dates 
MissingDates <- rbind(Daniel_DT, DaveDates)
MissingDates$point <- as.character(MissingDates$point)

colnames(all_PCs2009_2022)
colnames(MissingDates)

#add in Dave and Daniels times and Dates, mutating columns appropriately  

all_PCs2009_2022 <- all_PCs2009_2022 %>%
  left_join(MissingDates, by = c("Day", "point", "Site")) %>%
  mutate(Date = if_else(is.na(Date.x), as.character(Date.y), Date.x)) %>% 
  mutate(Time = if_else(is.na(Time.x), as.character(Time.y), Time.x)) %>% 
  select(-c(Time.x, Time.y, Date.x, Date.y))

#8 Add LiDAR for each points ####
#LiDar data is calculated in GEE and uses Asner 2021 (nominal year 2016) Canopy tree height and Above-ground carbon rasters to extract 
#for each point, and at a 50 and 100m buffer each of these values for all of the point counts 
Lidar

#remove "P" added to beginning of each "point" name so that it matches with data 
Lidar <- Lidar %>%  mutate(point = str_sub(point, start = 2)) 

#replace _ with with - to allow join 
Lidar <- Lidar %>%
  mutate(point = str_replace_all(point, "_", "-"))

Lidar$Site <- gsub("GC1L_2", "GC1L-2", Lidar$Site)

all_PCs2009_2022 <- all_PCs2009_2022 %>% ungroup() %>% left_join(Lidar, by = c("Site","point"))

#check we're not missing any lidar for any sites 
all_PCs2009_2022 %>% select(point,Site, ABC50) %>% unique() %>% filter(is.na(ABC50))

#Save all species data with LiDAR ####
#write.csv(Outputs/all_PCs2009_2022,"all_PCs2009_2022.csv")
#all_PCs2009_2022 <- read.csv("all_PCs2009_2022.csv")


# 8a. Add  trait data ####
# Uncomment here if you want to add trait information for species 
# #Add Latin Names ####
# #Add species names in Latin using taxize - nb only need to run once and then can
# #just use the traits output below: 
# traits <- read.csv("Outputs/AllBorneoSpeciesTraits.csv")
# 
# 
# # #use taxiza package to match species names to Birdlife Backbone for getting traits: 
# # allSPP <- all_PCs2009_2022 %>%
# #   ungroup() %>% 
# #   select(spp) %>% 
# #   unique()
# # 
# # #Use the get_tsn() function from the taxize package to get the Taxonomic Serial Number (TSN) for each bird name
# # #RUN ONCE!!
# # #bird_tsn <- lapply(allSPP$spp, get_tsn, taxon = "Aves", db = "itis")
# # #save output 
# # #saveRDS(bird_tsn,"Outputs/TaxiseOutputBirdlifeMatchedNames") 
# # bird_tsn <- read_rds("Outputs/TaxiseOutputBirdlifeMatchedNames")
# # 
# # #make a vector of taxon IDs
# # taxise_id <- unlist(bird_tsn)
# # 
# # #pull down the latin names for spp based on the unique identifier 
# # #bird_scientific <- taxise_id %>% id2name(db = "itis")
# # #saveRDS(Outputs/bird_scientific, "ScientificNamesFromTaxise")
# # 
# # #read in ScientificNames from Taxise: 
# # bird_scientific <-  readRDS("Outputs/ScientificNamesFromTaxise")
# # #bind list into one dataframe 
# # bird_scientific <- do.call(rbind, bird_scientific) %>% as.data.frame() 
# # 
# # #join to common names 
# # allSPP <- cbind(allSPP, bird_scientific)  
# # allSPP <- allSPP %>% rename(LatinName = name)
# # 
# # #species that came up as NA in the taxize database (export these and match them manually)
# # allSPP_NA <- allSPP %>% filter(is.na(LatinName)) %>% select(spp, LatinName)
# # 
# # #write.csv(allSPP_NA, "Outputs/allSppsNaToFindLatinNamesForToGetTraits.csv")
# # 
# # #  Add species traits from AVONET ####
# # 
# # #read in the Trait data from AVINET database (using the Birdlife names for birds)
# # traitsraw <- read.csv("Inputs/AVONET_birdlife.csv") %>% rename(LatinName = Species1, 
# #                                                         HabPref = Habitat)
# # 
# # #carry out a fuzzy left join 
# # traits_auto<- allSPP %>% fuzzy_left_join(traitsraw, 
# #                                          by = c("LatinName" = "LatinName"), 
# #                                          match_fun = str_detect) %>% 
# #   filter(!(is.na(LatinName.x))) #remove spps NA - we add these in from manual later
# # 
# # 
# # 
# # #read in the species I had to manually find the Birdlife Latin names for
# # #this includes (1)Spp that were NA in the taxize pulldown and (2) Species incorrectly named by taxize that don't match birdlife AVINET 
# # NaSpp <- read.csv("NaSppLatinNames.csv")
# # Natraits <- NaSpp %>% fuzzy_left_join(traitsraw, 
# #                                       by = c("LatinName" = "LatinName"), 
# #                                       match_fun = str_detect)
# # 
# # colnames(Natraits)
# # colnames(traits_auto)
# # 
# # #combine all trait data - making sure to replace spp in traits_automatic with manually corrected NaTraits
# # traits <- bind_rows(traits_auto, Natraits) %>% filter(!is.na(Sequence))
# # 
# # #clean trait information 
# # traits <- traits %>% select(spp, LatinName.x, 9:43) %>% rename(LatinName =LatinName.x ) %>% 
# #   select(-c(  "Avibase.ID1","Total.individuals" ,"Female","Male" ,"Unknown","Complete.measures", "Mass.Source" ,"Mass.Source",
# #               "Mass.Refs.Other","Inference", "Traits.inferred"))
# # #swiftlet Sp. is missing - remove 
# #
# #write.csv(traits, "AllBorneoSpeciesTraits.csv")
# 
# #join traits  with bird data #### 
# PointsWithTraits <- all_PCs2009_2022 %>% left_join(traits, by = "spp",relationship = "many-to-many")
# write.csv(PointsWithTraits, "Outputs/All2009_2022_PCs_withTrait_data")
# 
# #have all species been detected at least one? YES - all species have been detected
# x  <- PointsWithTraits %>% select(spp, abundance) %>% group_by(spp) %>% summarise(ab = sum(abundance))

#Final points with traits
#all_PCs2009_2022 <- PointsWithTraits



# 9. Add species forest dependencies

#check species that match and don't match between datasets 
species_match <- semi_join(df_IUCN_FD, all_PCs2009_2022, by ="spp") #211 species match 
dep_only <- anti_join(df_IUCN_FD, all_PCs2009_2022, by = "spp") # 11 spp unique to Daniel in forest 
missing_species <- all_PCs2009_2022 %>%
  anti_join(df_IUCN_FD, by = "spp") %>% select(spp) %>% unique()
missing_species

#remove the 4 species we don't have dependency data for (they're incorrect sp): 
# Remove missing species from all_PCs2009_2022
all_PCs2009_2022 <- all_PCs2009_2022 %>%
  filter(!(spp %in% missing_species$spp))

all_PCs2009_2022 <- all_PCs2009_2022 %>%
  left_join(df_IUCN_FD, by = "spp")

# 9. Filter distances for direct comparison ####
#filter out any of Daniel's data that is >100m away to enable matching across datesets 
all_PCs2009_2022 <- all_PCs2009_2022 %>%
  filter(!(Sampler == "Daniel_Kong" & distance > 100)) %>% 
  mutate(Time = sub("(:[0-9]{2}):[0-9]{2}$", "\\1", Time))## remove seconds in Simon mitchell's Time measure


#10. downstream corrections ####
##Check we're not missing times in unexpected places
xx <-  all_PCs2009_2022 %>%  select(point,Site,Sampler,Day,Time) %>% unique


#which columns have NAs - Simons PCs are missing day information (correct below)
all_PCs2009_2022 %>%
  summarise(across(everything(), ~any(is.na(.)))) %>%
  gather(key = "column", value = "has_na") %>%
  filter(has_na)

#GC1L-2, pt 9, day 2 is missing (we didn't #have time at the end of the field season to re-sample)
X <- all_PCs2009_2022 %>% ungroup()  %>% select(point,Site,Sampler,Day,Time) %>% unique %>% 
  filter(Day == 2 & Site == "GC1L-2" )

#Edit date column 
all_PCs2009_2022 <- all_PCs2009_2022 %>%
  as_tibble() %>%
  # convert date column to date format
  mutate(Date = as.Date(Date, format = "%d/%m/%Y")) 

## (1) fix timing formatting
all_PCs2009_2022 <- all_PCs2009_2022 %>%
  mutate(hours = gsub("^([0-9][0-9]).*", "\\1", Time), 
         minutes = gsub("^[0-9][0-9](.*)", "\\1", Time), 
         minutes = gsub("([0-9][0-9]):00", "\\1", minutes),
         minutes = gsub("\\.|\\:", "", minutes), 
         # as numeric
         hours = as.numeric(hours), 
         # note that a couple of visits are missing times: this needs fixing. When 
         # fixed remove this line 
         hours = ifelse(is.na(hours), 6, hours),
         minutes = as.numeric(minutes),
         hps = (hours + minutes/60) - 6, 
         # format date_time in order to sort out visit ID 
         minutes_char = as.character(minutes), 
         minutes_char = ifelse(nchar(minutes_char) == 1, 
                               paste0("0", minutes_char), 
                               minutes_char),
         date_time = as.POSIXct(paste0(Date, "-", hours, "-", minutes_char), 
                                format = "%Y-%m-%d-%H-%M")) %>%  
          mutate(point_id = interaction(Site, point))

## Note: worth visual checking that these split out hours and minutes properly
all_PCs2009_2022 %>%
  select(Site, point,Time, hours, minutes, date_time) %>%
  unique %>% 
  View()

# #how many duplicates?
# dup <- all_PCs2009_2022 %>% 
#   group_by(Site, point, spp, Date, Sampler) %>% 
#   count() %>% 
#   filter(n >1)


#2. Fix missing days (for Simon's part of the data)

date_info <- all_PCs2009_2022 %>%
  select(Site, point_id, Date, date_time, Time, Day, hps) %>%
  unique %>%
  group_by(Site, point_id) %>%
  # there are some point counts within the same hour.... worth checking on this
  mutate(Day = rank(date_time))

#add back in dates (all sites should have Day info now, including Simon)
all_PCs2009_2022 <- all_PCs2009_2022 %>% 
  select(-Day) %>% 
  left_join(date_info)

# #which columns have NAs - Simons PCs are missing day information (correct below)
all_PCs2009_2022 %>%
  summarise(across(everything(), ~any(is.na(.)))) %>%
  gather(key = "column", value = "has_na") %>%
  filter(has_na) #FIXED

#11. Create 0-filled dataset ####

## generate backbone dataframe with all point:visit:species combinations
df_backbone <- with(all_PCs2009_2022, expand.grid(point_id = unique(point_id), 
                                     Day = 1:4,
                                    spp = unique(spp)))

#Nb; we must differentiate between:
#(1) points-visits-species that were detected = 1 
#(2) point-visit-species that were technically possible, but abundance was 0 = 0
#(3) point-visit-species that were impossible as the visit wasn't made = NA


## make a dataframe of detections (e.g. points-visits-species= 1)
df_non0 <- all_PCs2009_2022 %>%
  filter(abundance > 0) %>% 
  mutate(point_visited = 1) %>%  
  select(Site,point_id, point_visited,spp,Day, abundance)


## join to create full dataframe where abundance is 0 if point visit was made 
## but species not detected, remains as abundance val if species detected, and NA if point visit 
## doesn't exist
df_full <- date_info %>%
  mutate(point_visited = 1) %>% # these are all the points visited (i.e. recorded 1s and 0s)
  left_join(df_backbone, .) %>% #take potential points, and combine with all point visited
                                #if day = Na, this is a point-visit-species that was impossible as the visit wasn't made
  
  left_join(., 
            df_non0[on =c("Site", "point_id", "Day", "spp","abundance")]) %>%
  mutate(abundance = ifelse(is.na(abundance) & point_visited==1, 0, abundance))

#check missing day*pt combos 
#good missing day 4 stuff only (most sites only sampled 3 days),
#plus GC1L-2.9  day 3 
#and GCL2.9 day  3 - this is an error, COME BACK TO THIS DAY*POINT

check <- df_full %>%
  filter(if_any(everything(), is.na)) %>% select(point_id, Day) %>% unique()


names(all_PCs2009_2022)
hab_covars <- all_PCs2009_2022 %>% select(point_id, Site, Year, Habitat,Sampler,
                                          Time_Since_Logging, Time_Since_Restoration,
                                          Latitude, Longitude, ABC50, ABC100) %>% unique()

sp_covars <-  all_PCs2009_2022 %>% select(spp, forestDependency, IUCN,nonResident) %>% unique()  

#add in the extra information 
full_birds_flockr <- df_full %>% 
  left_join(hab_covars) %>%  
  left_join(sp_covars) %>% 
#add in plantation age information 
  # join in plantation age info
  left_join(., df_habitat, by = c("point_id")) %>%
  rename(plantation_age = Age) 

names(full_birds_flockr)

#12. Export data ####

#Export full abundances dataframe
write.csv(all_PCs2009_2022, "Outputs/allDanielDaveSimonDetectionsWithLidar.csv")

#explort dataframe ready for flockr, in next script
write.csv(full_birds_flockr, "Outputs/birdDataForFlockr.csv")

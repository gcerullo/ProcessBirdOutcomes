
# Clean and format data for passing to flocker
rm(list = ls())

# housekeeping ----
#install.packages("remotes")
#remotes::install_github("jsocolar/flocker")
library(flocker); library(dplyr)

#read in data.
#NB - this is the tidy output of the Combine_Bird_Datasets script. 

df_birds <- read.csv("Outputs/birdDataForFlockr.csv") # the output of the Combine_Bird_Datasets script
names(df_birds)


# Format dataframe for passing to flocker ----

## make detection data wide-format
# note: if you get warnings about aggregating by length, then there is a 
# problem upstream. This warning indicates that there are at least some
# point:visit:species combinations that are not unique (which they should be) 
df_det_wf <- df_birds %>%
  mutate(det = ifelse(abundance == 0, 0, 1)) %>%
  reshape2::dcast(formula = point_id + spp ~ Day, value.var = "det") %>%
  rename(species = spp, d1 = `1`, d2 = `2`, d3 = `3`, d4 = `4`)

## make visit time numeric and make wide-format
df_time_wf <- df_birds %>%
  select(point_id, Day, hps) %>%
  mutate(hps_sc = scale(hps)) %>%
  unique %>% 
  reshape2::dcast(formula = point_id ~ Day, value.var = c("hps_sc"))  %>%
  rename(hps_sc1 = `1`, hps_sc2 = `2`, hps_sc3 = `3`, hps_sc4 = `4`) 

## sort out point covariates
df_cov <- df_birds %>%
  select(point_id, 
         site = Site,
         observer = Sampler,
         year = Year, 
         time_since_logging = Time_Since_Logging, 
         plantation_age,
         habitat = Habitat,
         ABC50) %>%
  unique() %>%  
  #remove NAs when extract site covariates
  filter(!is.na(site))

df_cov_sp <- df_birds %>%
  select(species = spp, dependency = forestDependency) %>% 
  unique() # %>% 
 #        family = Family1, scientific_name = LatinName) %>%
  

## full analysis dataframe 
df_af <- left_join(df_det_wf, df_time_wf) %>%
  left_join(., df_cov, by = "point_id") %>%
  left_join(., df_cov_sp) %>%
  mutate(time_since_logging_sc = scale(time_since_logging), 
         ABC50_sc = scale(ABC50), 
         plantation_age_sc = scale(plantation_age), 
         time_since_logging_sc = ifelse(is.na(time_since_logging_sc), 
                                        0, 
                                        time_since_logging_sc),
         ABC50_sc = ifelse(is.na(ABC50_sc), 
                           0, 
                           ABC50_sc),
         plantation_age_sc = ifelse(is.na(plantation_age_sc), 
                                    0, 
                                    plantation_age_sc), 
         time_since_logging = ifelse(is.na(time_since_logging), -99, time_since_logging),
         ABC50 = ifelse(is.na(ABC50), -99, ABC50),
         plantation_age = ifelse(is.na(plantation_age), -99, plantation_age)) %>%
  mutate(site_sp = interaction(site, species), 
         observer_sp = interaction(observer, species), 
         year_sp = interaction(year, species), 
         # habitat vars 
         primary = ifelse(habitat == "Primary", 1, -1),
         twice_logged = ifelse(habitat == "Twice_logged", 1, -1), 
         once_logged = ifelse(habitat == "Once_logged", 1, -1), 
         logged_restored = ifelse(habitat == "Restored", 1, -1),
         eucalyptus = ifelse(habitat == "Eucalyptus_pellita", 1, -1), 
         albizia = ifelse(habitat == "Albizia_falcataria", 1, -1),
         forestdep_high = ifelse(dependency == "high", 1, -1), 
         forestdep_med = ifelse(dependency == "medium", 1, -1), 
         forestdep_low = ifelse(dependency %in% c("low", "none"), 1, -1)
  )

df_af

# #Last checks 
# ###
# #NOTE
# colnames(df_af)
# # calculate the number of NAs in each column
# na_count <- colSums(is.na(df_af))
# # get the names of columns with NAs
# cols_with_na <- names(na_count[na_count > 0])
# # print the names of columns with NAs
# cols_with_na
# #which columns don't have d3 info?
# d3NA <- df_af %>% filter(is.na(d3)) #only GC1L-2.9 - good
# d4NA <- df_af %>% filter(is.na(d4)) #	Lots of points dont have d4 data, as
# #expected (most points were only sampled 3 times)
# hps_sc3NA <- df_af%>% filter(is.na(hps_sc3)) %>% select(point_id) %>% unique()
# Day 3; GC1L-2.9 is missing time data, good
# hps_sc4NA <- df_af%>% filter(is.na(hps_sc4)) %>% select(point_id) %>% unique()
# Lots of points dont have d4 time; as expected
# ##

#EXPORT spp names to get forest integrity data for 
#spp <- as.data.frame(df_af$species %>% unique) %>% rename("Species" = 1)
#write.csv(spp, "SppForForIntegrity.csv")

fd <- flocker::make_flocker_data(obs = as.matrix(select(df_af, d1:d4)), 
                                 unit_covs = select(df_af, 
                                                    species,
                                                    point_id,
                                                    habitat,
                                                    primary:albizia,
                                                    time_since_logging, time_since_logging_sc,
                                                    ABC50, ABC50_sc, 
                                                    plantation_age, plantation_age_sc,
                                                    year, year_sp, 
                                                    site, site_sp,
                                                    # family, 
                                                    # scientific_name,
                                                    dependency,
                                                    forestdep_high:forestdep_low,
                                                    observer, observer_sp), 
                                 list(time_of_day = select(df_af, hps_sc1:hps_sc4)))


saveRDS(fd, "Outputs/fd_28-05-24.rds")



fd_zi <- flocker::make_flocker_data(obs = as.matrix(select(df_af, d1:d4)), 
                                    unit_covs = select(df_af, 
                                                       point_id,
                                                       habitat,
                                                       primary:albizia,
                                                       time_since_logging, time_since_logging_sc,
                                                       ABC50, ABC50_sc, 
                                                       plantation_age, plantation_age_sc,
                                                       year, year_sp, 
                                                       site, site_sp,
                                                       species, 
                                                       dependency,
                                                       forestdep_high:forestdep_low,
                                                       observer, observer_sp))

saveRDS(fd_zi, "Outputs/fd_no_visit_cov_28-05-24.rds")


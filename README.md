# ProcessBirdOutcomes
Wrangle raw point count data, fit Bayesian models, etc. 


#1. Combine_Bird_Datasets.R
This code combines the following data into one dataframe: 
- DK 2022 Plantation Points Counts - coordinated by GRC
-  DK 2022 Forest Points Counts - coordinated by GRC
-  DE points counts
-  SM point counts

  After combining this data, it adds relevant logging age and time since harvesting information, along with species trait information, and point-level LiDAR information. 


#2. ForBirdForFlockr.R 
This code takes the output from 1 and organises the data so that it is ready to pass to the Flockr package, for running Bayesian multi-occupancy analyses


#3. FitModel.R 
This code takes the outout from 2 and fits the model structure. allowing bird occupancy o alter with habitat and age.

#AssessScenarioOutcomes.R
This code assesses bird outcomes for each scenario. 
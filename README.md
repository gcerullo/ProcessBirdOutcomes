# ProcessBirdOutcomes
Wrangle raw point count data, fit Bayesian models, etc. 

<<<<<<< HEAD
#1. Combine bird datasets.R
combines Dave, Simon, Daniel point counts 

#2. FormatBirdForFlockr.R
Prepares abundance outputs to be ready to give as occupancy to Flockr

#3. FitModel.R 
Fit the model structure for Bird occupancy, allowing occ to alter with habitat and age 



=======

#1. Combine_Bird_Datasets 
This code combines the following data into one dataframe: 
- DK 2022 Plantation Points Counts - coordinated by GRC
-  DK 2022 Forest Points Counts - coordinated by GRC
-  DE points counts
-  SM point counts

  After combining this data, it adds relevant logging age and time since harvesting information, along with species trait information, and point-level LiDAR information. 


#2. ForBirdForFlockr 
This code takes the output from 1 and organises the data so that it is ready to pass to the Flockr package, for running Bayesian multi-occupancy analyses


#3. FitModel 
This code takes the outout from 2 and fits the model structure. 
>>>>>>> bed8a2d345a3904c86efac280774496b5b94c709

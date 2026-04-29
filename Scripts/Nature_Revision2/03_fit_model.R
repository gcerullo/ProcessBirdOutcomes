# =============================================================================
# Self-notes — NR2 bird pipeline
# =============================================================================
# What I'm doing: I'm fitting my multi-species occupancy model in flocker/brms (cmdstanr), with the
#   priors and formula I settled on for habitat × forest-dependency and detection structure.
#
# What I need (inputs): `Outputs/NR2/models/fd_28-05-24.rds` (flocker data object).
#
# What I produce (outputs): `Outputs/NR2/models/fit.rds` (brms fit), CmdStan files under the same
#   models folder, and `Outputs/NR2/models/fit_backup.rds`.
# =============================================================================

#fit model 
#this code sets the model structure for the full multi-species Bayesian occupancy models

source("Scripts/Nature_Revision_2/00_config.R")
nr2_paths <- nr2_init(".", verbose = FALSE)

library(brms); library(flocker); library(dplyr)
# devtools::install_github("jsocolar/flocker")
# cmdstan path
# C:/Users/smills2/.cmdstan/cmdstan-2.32.2
fd <- readRDS(file.path(nr2_paths$models_dir, "fd_28-05-24.rds"))

prior_specification <- c(
  # intercept terms (i.e. average species )
  #set_prior("normal(-3,2)", dpar = "occ", coef = "albizia"),
  #set_prior("normal(-3,2)", dpar = "occ", coef = "eucalyptus"),
  #set_prior("normal(-3,2)", dpar = "occ", coef = "logged_restored"),
  #set_prior("normal(-3,2)", dpar = "occ", coef = "once_logged"),
  #set_prior("normal(-3,2)", dpar = "occ", coef = "twice_logged"),
  #set_prior("normal(-3,2)", dpar = "occ", coef = "primary"),
  # forest dependency interaction terms
  #set_prior("normal(0,2)", dpar = "occ", coef = "albizia:forestdep_high"),
  set_prior("normal(0,2)", dpar = "occ", coef = "albizia:forestdep_med"),
  set_prior("normal(0,2)", dpar = "occ", coef = "albizia:forestdep_low"),
  #set_prior("normal(0,2)", dpar = "occ", coef = "eucalyptus:forestdep_high"),
  set_prior("normal(0,2)", dpar = "occ", coef = "eucalyptus:forestdep_med"),
  set_prior("normal(0,2)", dpar = "occ", coef = "eucalyptus:forestdep_low"),
  #set_prior("normal(0,2)", dpar = "occ", coef = "logged_restored:forestdep_high"),
  set_prior("normal(0,2)", dpar = "occ", coef = "logged_restored:forestdep_med"),
  set_prior("normal(0,2)", dpar = "occ", coef = "logged_restored:forestdep_low"),
  #set_prior("normal(0,2)", dpar = "occ", coef = "once_logged:forestdep_high"),
  set_prior("normal(0,2)", dpar = "occ", coef = "once_logged:forestdep_med"),
  set_prior("normal(0,2)", dpar = "occ", coef = "once_logged:forestdep_low"),
  #set_prior("normal(0,2)", dpar = "occ", coef = "primary:forestdep_high"),
  set_prior("normal(0,2)", dpar = "occ", coef = "primary:forestdep_med"),
  set_prior("normal(0,2)", dpar = "occ", coef = "primary:forestdep_low"),
  #set_prior("normal(0,2)", dpar = "occ", coef = "twice_logged:forestdep_high"),
  set_prior("normal(0,2)", dpar = "occ", coef = "twice_logged:forestdep_med"),
  set_prior("normal(0,2)", dpar = "occ", coef = "twice_logged:forestdep_low"),
  # year
  #set_prior("normal(0,1)", dpar = "occ", coef = "factoryear2008"),
  #set_prior("normal(0,1)", dpar = "occ", coef = "factoryear2009"),
  #set_prior("normal(0,1)", dpar = "occ", coef = "factoryear2011"),
  #set_prior("normal(0,1)", dpar = "occ", coef = "factoryear2015"),
  #set_prior("normal(0,1)", dpar = "occ", coef = "factoryear2017"),
  #set_prior("normal(0,1)", dpar = "occ", coef = "factoryear2022"),
  # forest dep
  #set_prior("normal(0,.5)", dpar = "occ", coef = "forestdep_high"),
  #set_prior("normal(0,.5)", dpar = "occ", coef = "forestdep_low"),
  #set_prior("normal(0,.5)", dpar = "occ", coef = "forestdep_med"),
  # interactions
  set_prior("normal(0,2)", dpar = "occ", class = "b"), 
  set_prior("normal(0,3)", dpar = "occ", class = "sd"), 
  ## detection
  set_prior("normal(0,2)", class = "b", dpar = ""),
  set_prior("normal(0,3)", class = "sd", dpar = "")
)

fit <- flock(f_occ = ~ 0 + # don't fit intercept (as -1:1 coded)
               # year plus year x species intercepts
               (1|year) + (1|year_sp) + 
               # site plus site x species intercepts
               (1|site) + (1|site_sp) + 
               # species random intercepts for each habitat type
               (0 + primary + twice_logged + once_logged + logged_restored + 
                  eucalyptus + albizia + logged_restored:time_since_logging_sc + 
                  once_logged:time_since_logging_sc + eucalyptus:plantation_age_sc +
                  albizia:plantation_age_sc|species) + 
               # bird-life forest dependency 
               # dependency:habitat interactions
               forestdep_high:primary + 
               forestdep_high:twice_logged + 
               forestdep_high:once_logged + forestdep_high:once_logged:time_since_logging_sc +
               forestdep_high:logged_restored + forestdep_high:logged_restored:time_since_logging_sc + 
               forestdep_high:eucalyptus + forestdep_high:eucalyptus:plantation_age_sc +  
               forestdep_high:albizia + forestdep_high:albizia:plantation_age_sc + 
               #
               forestdep_med:primary + 
               forestdep_med:logged_restored + forestdep_med:logged_restored:time_since_logging_sc +
               forestdep_med:once_logged + forestdep_med:once_logged:time_since_logging_sc +
               forestdep_med:twice_logged + 
               forestdep_med:eucalyptus + forestdep_med:eucalyptus:plantation_age_sc +  
               forestdep_med:albizia + forestdep_med:albizia:plantation_age_sc + 
               #
               forestdep_low:primary + 
               forestdep_low:logged_restored + forestdep_low:logged_restored:time_since_logging_sc +
               forestdep_low:once_logged + forestdep_low:once_logged:time_since_logging_sc +
               forestdep_low:twice_logged + 
               forestdep_low:eucalyptus + forestdep_low:eucalyptus:plantation_age_sc +  
               forestdep_low:albizia + forestdep_low:albizia:plantation_age_sc,
             # additional components that might be worth including
             # occupancy in each habitat type can vary according to body mass
             #log_body_mass + log_body_mass:habitat_type +
             # dietary guild
             # random intercept can vary by family
             # (1 + twice_logged + once_logged + logged_restored + eucalyptus + 
             #      albizia|family)
             f_det = ~ 0 + 
               primary + ABC50_sc + 
               twice_logged + 
               once_logged + 
               logged_restored + 
               eucalyptus + eucalyptus:plantation_age_sc + 
               albizia + albizia:plantation_age_sc +
               time_of_day + 
               observer + 
               (1|observer_sp) + 
               (1 + time_of_day|species),
             prior = prior_specification, 
             flocker_data = fd, 
             sample_prior = "yes",
             save_warmup = TRUE,
             chains = 4, cores = 4,
             file = file.path(nr2_paths$models_dir, "fit.rds"),
             output_dir = nr2_paths$models_dir,
             output_basename = "Borneo_v4",
             backend = "cmdstanr"
)

# save ----
saveRDS(fit, file.path(nr2_paths$models_dir, "fit_backup.rds"))
summary(fit)

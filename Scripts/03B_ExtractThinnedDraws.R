# generate predicted occupancy for 500 thinned draws for model propagating


# packages
library(flocker); library(brms); library(dplyr); library(ggplot2)

fit <- readRDS("outputs/fit_2024-07-12.rds")
fd <- readRDS('outputs/fd_2024-07-12.rds')

# get fd data
fd_data <- fd$data[1:fd$data$ff_n_unit[1],]

# Get plantation and logging age variables ----
## check scaling coefs are correct----
# plantation age
plantation_age <- fd_data$plantation_age
plantation_age[plantation_age == -99] <- NA
plantation_age_sc <- scale(plantation_age)
plantation_age_sc[is.na(plantation_age_sc)] <- 0
all(plantation_age_sc - fd_data$plantation_age_sc == 0)

# time since logging
time_since_logging <- fd_data$time_since_logging
time_since_logging[time_since_logging == -99] <- NA
time_since_logging_sc <- scale(time_since_logging)
time_since_logging_sc[is.na(time_since_logging_sc)] <- 0
all(time_since_logging_sc - fd_data$time_since_logging_sc == 0)
# all good so extract scaling info for plantation age and time since logging

## extract plantation scale and center----
plant_age_cent <- attr(plantation_age_sc, "scaled:center")
plant_age_scale <- attr(plantation_age_sc, "scaled:scale")

## extract time since logging scale and center ----
time_since_logging_cent <- attr(time_since_logging_sc, "scaled:center")
time_since_logging_scale <- attr(time_since_logging_sc, "scaled:scale")

## generate variables for prediction ----
plantation_age_seq <- tibble(plantation_age = 0:13) %>%
    mutate(plantation_age_sc = (plantation_age - plant_age_cent)/plant_age_scale)

time_since_logging_seq <- tibble(time_since_logging = 19:62) %>%
    mutate(time_since_logging_sc = 
               (time_since_logging - time_since_logging_cent)/time_since_logging_scale)

# create variable combinations for predicting ----
# get all cols that aren't used in prediction but need to be in dataframe anyway
# get all unique species and species x habitat variables
pred_data <- fd_data %>%
    select(habitat, species, 
           dependency, forestdep_high, forestdep_med, forestdep_low, 
           primary, once_logged, twice_logged, logged_restored, eucalyptus, albizia) %>%
    unique %>%
    mutate(time_of_day = 0, ABC50_sc = 0, 
           observer = "SM", 
           observer_sp = fd$data$observer_sp[1], 
           year = fd$data$year[1], 
           year_sp = fd$data$year_sp[1], 
           site = fd$data$site[1],
           site_sp = fd$data$site_sp[1], 
           ff_y = 0, ff_n_unit = fd$data$n_unit[1], ff_n_rep = 4, ff_Q = 1, 
           ff_rep_index1 = 1, ff_rep_index2 = 1, ff_rep_index3 = 1, ff_rep_index4 = 1,
           y = 0,
           n_unit = fd$data$n_unit[1], n_rep = 4,  Q = 0, 
           rep_index1 = 1, rep_index2 = 1, rep_index3 = 1,
           rep_index4 = 1)

# extract just logged habitat categories and generate time since logging combns
pred_time_since_logging <- pred_data %>%
    filter(habitat %in% c("Once_logged", "Restored")) %>%
    replicate(nrow(time_since_logging_seq), ., FALSE) %>%
    bind_rows(., .id = "id") %>%
    mutate(time_since_logging = (19:62)[as.integer(id)]) %>%
    left_join(., time_since_logging_seq) %>%
    mutate(plantation_age = NA, plantation_age_sc = 0)

# extract plantation categories and generate plantation age combns
pred_plantation_age <- pred_data %>%
    filter(habitat %in% c("Eucalyptus_pellita", "Albizia_falcataria")) %>%
    replicate(nrow(plantation_age_seq), ., FALSE) %>%
    bind_rows(., .id = "id") %>%
    mutate(plantation_age = (0:13)[as.integer(id)]) %>%
    left_join(., plantation_age_seq) %>%
    mutate(time_since_logging = NA, time_since_logging_sc = 0)

pred_primary <- pred_data %>%
    filter(habitat == 'Primary') %>%
    mutate(time_since_logging = NA, time_since_logging_sc = 0, 
           plantation_age = NA, plantation_age_sc = 0)

preds_primary <- fitted_flocker(fit, components = "occ", new_data = pred_primary, 
                                draw_ids = seq(1, 4000, 8))

preds_out <- as_tibble(preds_primary$linpred_occ) %>%
    setNames(paste0("draw_", 1:500)) %>%
    bind_cols(pred_primary, .)

preds_primary_out_summ <- tibble(mid = matrixStats::rowMeans2(preds_primary$linpred_occ), 
                                 lwr = matrixStats::rowQuantiles(preds_primary$linpred_occ, probs = .1),
                                 upr = matrixStats::rowQuantiles(preds_primary$linpred_occ, probs = .9)) %>%
    bind_cols(pred_primary, .)


# Knock these 5 categories (above) out of the prediction dataframe and then 
# add the dataframes with the range of ages/time since logging back in
pred_data_full <- pred_data %>%
    filter(!(habitat %in% c("Eucalyptus_pellita", "Albizia_falcataria", 
                            "Once_logged", "Restored"))) %>%
    mutate(time_since_logging = NA, time_since_logging_sc = 0, 
           plantation_age = NA, plantation_age_sc = 0) %>%
    bind_rows(., pred_time_since_logging) %>%
    bind_rows(., pred_plantation_age) %>% 
    bind_rows(pred_primary) %>%
    select(-id)

# generate predictions ----
preds <- fitted_flocker(fit, components = "occ", new_data = pred_data_full, 
                        draw_ids = seq(1, 4000, 8))

# drop all the redundant cols to produce a reduced dataframe 
pdat_red <- pred_data_full %>%
    select(-(ff_y:rep_index4))

# summarise predictions
out <- as_tibble(preds$linpred_occ) %>%
    setNames(paste0("draw_", 1:500)) %>%
    bind_cols(pdat_red, .)

out_summ <- tibble(mid = matrixStats::rowMeans2(preds$linpred_occ), 
                   lwr = matrixStats::rowQuantiles(preds$linpred_occ, probs = .1),
                   upr = matrixStats::rowQuantiles(preds$linpred_occ, probs = .9)) %>%
    bind_cols(pdat_red, .)

out_summ2 <- replicate(4, preds_primary_out_summ, FALSE) %>%
    bind_rows(.id='id') %>%
    mutate(time_since_logging = 0, 
           plantation_age = -5,
           habitat = c("Eucalyptus_pellita", "Albizia_falcataria", 
                       "Once_logged", "Restored")[as.integer(id)]) %>%
    bind_rows(out_summ)  %>%
    mutate(dependency_label = case_when(dependency == "none" ~ "low", 
                                        TRUE ~ dependency), 
           dependency_label = paste0(toupper(gsub('(^[a-z]).*', '\\1', dependency_label)), 
                                     gsub('^.(.*)', '\\1', dependency_label)),
           dependency_label = factor(dependency_label, levels = c("High", "Medium", "Low")), 
           habitat = gsub("_", " ", habitat))

# Get plantation and logging categories and plot these
plantation_pred_df <- out_summ2  %>%
    filter(habitat %in% c("Eucalyptus pellita", "Albizia falcataria"))

logging_pred_df <- out_summ2  %>%
    filter(habitat %in% c("Once logged", "Restored")) 

plantation_pred_df %>%  
    filter(plantation_age >= 0) %>%
    ggplot(aes(plantation_age, mid, group=species)) +
    geom_line(alpha=.5, col='grey0') +
    geom_point(data = plantation_pred_df %>% filter(plantation_age == -5), 
               alpha=.5, col='grey0') +
    geom_line(data = plantation_pred_df %>% filter(plantation_age <= 0), 
              lty = 'longdash', 
              alpha=.5, col='grey0') +
    facet_grid(habitat~dependency_label) +
    theme_bw() +
    theme(strip.text = element_text(hjust=0, face="bold"), 
          strip.background = element_blank(), 
          axis.text = element_text(colour="black"), 
          panel.grid = element_blank()) +
    labs(y = "P(occupancy)", x = "Plantation age") +
    scale_x_continuous(breaks = c(-5, 0, 5, 10), 
                       labels = c('Primary', 0, 5, 10))
ggsave("figures/plantation_age_estimates.png", units="mm", height=150, width=230)

logging_pred_df %>%  
    filter(time_since_logging >= 19) %>%
    ggplot(aes(time_since_logging, mid, group=species)) +
    geom_line(alpha=.5, col='grey0') +
    geom_point(data = logging_pred_df %>% filter(time_since_logging < 14), 
               alpha=.5, col='grey0') +
    geom_line(data = logging_pred_df %>% filter(time_since_logging <= 19), 
              lty = 'longdash', 
              alpha=.5, col='grey0') +
    facet_grid(habitat~dependency_label) +
    theme_bw() +
    theme(strip.text = element_text(hjust=0, face="bold"), 
          strip.background = element_blank(), 
          axis.text = element_text(colour="black"), 
          panel.grid = element_blank()) +
    labs(y = "P(occupancy)", x = "Time since logging") +
    scale_x_continuous(breaks = c(0, 20, 40, 60), 
                       labels = c('Primary', 20, 40, 60))
ggsave("figures/time_since_logging_estimates.png", units="mm", height=150, width=230)
    
# out_summ %>%
#     mutate(dependency_label = case_when(dependency == "none" ~ "low", 
#                                         TRUE ~ dependency), 
#            dependency_label = factor(dependency_label, levels = c("high", "medium", "low"))) %>%
#     filter(habitat %in% c("Once_logged", "Restored")) %>% 
#     ggplot(aes(time_since_logging, mid, group=species)) +
#     geom_line() +
#     facet_grid(habitat~dependency_label) +
#     theme(strip.text = element_text(hjust=0, face="bold"), 
#           strip.background = element_blank(), 
#           axis.text = element_text(colour="black")) +
#     labs(y = "P(occupancy)", x = "Time since logging")
# ggsave("figures/time_since_logging_estimates.png", units="mm", height=150, width=230)
# 
# out_summ %>%
#     mutate(dependency_label = case_when(dependency == "none" ~ "low", 
#                                         TRUE ~ dependency), 
#            dependency_label = factor(dependency_label, levels = c("high", "medium", "low"))) %>%
#     arrange(mid) %>%
#     mutate(species_id = factor(1:n())) %>%
#     filter(habitat %in% c("Primary")) %>% 
#     ggplot(aes(species_id, mid, col = dependency)) +
#     geom_point() +
#     # facet_grid(habitat~dependency_label) +
#     theme(strip.text = element_text(hjust=0, face="bold"), 
#           strip.background = element_blank(), 
#           axis.text = element_text(colour="black")) +
#     labs(y = "P(occupancy)", x = "Time since logging")
# ggsave("figures/time_since_logging_estimates.png", units="mm", height=150, width=230)

# save outputs ----
saveRDS(out, "outputs/predicted_occupancy_500_draws.rds")
saveRDS(out_summ, "outputs/predicted_occupancy_500_draws_summarised.rds")

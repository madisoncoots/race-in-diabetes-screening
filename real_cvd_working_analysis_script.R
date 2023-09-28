# Author: Madison Coots
# Date: September 14, 2023
# ======================
# Code for all figures, tables, and statistics 
# in Coots et al. (2023)

library(tidyverse)
library(readr)
library(janitor)
library(stats)

directory_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
save_path <- here::here(directory_path, 'figures/')

source(here::here(directory_path, 'colors.R'))

data <- readRDS(here::here(directory_path, 'data/processed', 'cvd_data.rds'))

theme_set(theme_bw(base_size = 15))

# ===========================================================================================
#====================================== Real Approach =======================================
# ====================================== Models =============================================

# ==============================================================================
# ============================ Synthetic data set ==============================

# Inputs to ASCVD Risk Model: 
# Gender (gender)
# Age (ridageyr)
# Race (race)
# Total cholesterol (lbxtc)
# LDL cholesterol (lbdldl)
# HDL cholesterol (lbdhdd)
# Treatment with statin (statins)
# Systolic blood pressure (sys_bp)
# Diastolic blood pressure (dias_bp)
# Treatment for hypertension (hypertension_treatment)
# History of diabetes (diabetes)
# Current smoker (smokes)
# Aspirin therapy (aspirin)

race_blind_formula <- cvd ~ gender + ridageyr + lbxtc + lbdldl + 
  lbdhdd + statins + sys_bp + dias_bp + hypertension_treatment + diabetes + smokes + 
  aspirin 
race_blind_model <- glm(race_blind_formula,
                          data = data,
                          family = "binomial",
                          weights = round(wtmec8yr/1000)) # glm complains when weights aren't ints
race_blind_model_pred <- data %>%
  mutate(race_blind_model_pred = predict(race_blind_model, newdata = ., type = "response")) %>%
  select(seqn, race_blind_model_pred)
  

# race_aware_formula <- cvd ~ gender + ridageyr  + lbxtc + lbdldl + 
#   lbdhdd + race*statins + sys_bp + dias_bp + hypertension_treatment + diabetes + race*smokes + 
#   race*aspirin

race_aware_formula <- cvd ~ gender + ridageyr  + lbxtc + lbdldl +
  lbdhdd + statins + sys_bp + dias_bp + hypertension_treatment + diabetes + smokes +
  aspirin + race

race_aware_model <- glm(race_aware_formula,
                          data = data,
                          family = "binomial",
                          weights = round(wtmec8yr/1000)) # glm complains when weights aren't ints
race_aware_model_pred <- data %>%
  mutate(race_aware_model_pred = predict(race_aware_model, newdata = ., type = "response")) %>%
  select(seqn, race_aware_model_pred)

large_model_formula <- cvd ~ gender + ridageyr + lbxtc + lbdldl +
  lbdhdd + sys_bp + diabetes + smokes + statins + hypertension_treatment +
  aspirin + dias_bp + race*income + bmxbmi
large_model <- glm(large_model_formula,
                   data = data,
                   family = "binomial",
                   weights = round(wtmec8yr/1000)) # glm complains when weights aren't ints
large_model_pred <- data %>%
  mutate(p_cvd = predict(large_model, newdata = ., type = "response")) %>%
  select(seqn, p_cvd)

# ===========================================================================================
# ================================= Plotting Functions ======================================

calibrate_model_pred <- function(model, data) {
  # --------------------------------------------
  # Function to first calibrate the model pred
  # using isotonic regression.
  #
  # Returns a df with the calibrated risk scores
  # and observation id.
  # --------------------------------------------
  uncalibrated_pred <- predict(model, newdata = data, type = "response")
  data <- data %>%
    mutate(uncalibrated_pred = uncalibrated_pred) %>%
    drop_na(uncalibrated_pred, cvd)
  # Train an isotonic model for each race group
  race_groups = unique(data$race)
  return_df <- data.frame()
  for (r in race_groups) {
    subset <- data %>% filter(race == r)
    iso_fit <- as.stepfun(isoreg(subset$uncalibrated_pred, subset$cvd))
    subset <- subset %>%
      mutate(calibrated_pred = iso_fit(uncalibrated_pred)) %>%
      select(seqn, calibrated_pred)
    return_df <- bind_rows(return_df, subset)
  }
  return(return_df)
}

calibrate_blind_model_pred <- function(model, data) {
  # --------------------------------------------
  # Function to first calibrate the blind model 
  # pred using isotonic regression.
  #
  # Returns a df with the calibrated risk scores
  # and observation id.
  # --------------------------------------------
  uncalibrated_pred <- predict(model, newdata = data, type = "response")
  data <- data %>%
    mutate(uncalibrated_pred = uncalibrated_pred) %>%
    drop_na(uncalibrated_pred, cvd)
  iso_fit <- as.stepfun(isoreg(data$uncalibrated_pred, data$cvd))
  data <- data %>% mutate(calibrated_pred = iso_fit(uncalibrated_pred)) %>%
    select(seqn, calibrated_pred)
  return(data)
}

get_smoothed_plot_data <- function(pred_df, data, risk_lower_bound, risk_upper_bound) {
  # --------------------------------------------
  # Function to smooth noise in the CVD rates
  # within risk score buckets.
  #
  # Returns a df with the calibrated risk scores
  # and smoothed CVD rates, by race.
  # --------------------------------------------
  data <- data %>%
    inner_join(pred_df, by = c("seqn"))
  smoothing_model <- glm(cvd ~ calibrated_pred*race, 
                         data = data, 
                         family = "binomial",
                         weights = round(wtmec8yr/1000)
                         )
  plot_data <- data.frame(
    calibrated_pred = seq(risk_lower_bound, risk_upper_bound, by = 0.01),
    race = rep(c("Black", "White", "Hispanic"), each = ((risk_upper_bound - risk_lower_bound) * 100) + 1)
  ) %>%
    mutate(smoothed_cvd = predict(smoothing_model, newdata = ., type = "response"))
  return(plot_data)
}



# ===========================================================================================
# ======================================= Figures ===========================================

# ===========================================================================================
# ================================= binning w/ smoothing ====================================
# ===========================================================================================

incidence_upper_bound <- 0.5
risk_score_upper_bound <- 0.4

data %>%
  inner_join(race_blind_model_pred, by = c("seqn")) %>%
  select(race, cvd, race_blind_model_pred, wtmec8yr) %>%
  mutate(bin = floor((race_blind_model_pred  + 0.0025) * 100 * 2) / 2 / 100) %>% # round to the nearest 0.005
  drop_na() %>%
  group_by(race, bin) %>%
  summarize(n_in_bin = sum(wtmec8yr),
            bin_avg = sum(race_blind_model_pred * wtmec8yr) / sum(wtmec8yr),
            cvd_prev = sum(as.integer(cvd) * wtmec8yr) / sum(wtmec8yr)) %>%
  ggplot(aes(x=bin_avg, y=cvd_prev, color=race)) +
  geom_vline(xintercept=0.2) +
  geom_smooth(se=FALSE, method="lm") +
  # geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
  xlab("Race-unaware predicted risk") +
  ylab("Observed CVD rate") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.0, incidence_upper_bound, 0.04)) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0.0, risk_score_upper_bound, 0.04)) +
  coord_cartesian(xlim = c(0, risk_score_upper_bound), ylim = c(0, incidence_upper_bound)) +
  theme(legend.title = element_blank(),
        legend.position = c(0.15, 0.84)) +
  scale_color_manual(values=group_color_map,
                     breaks =group_names)

data %>%
  inner_join(race_aware_model_pred, by = c("seqn")) %>%
  select(race, cvd, race_aware_model_pred, wtmec8yr) %>%
  mutate(bin = floor((race_aware_model_pred  + 0.0025) * 100 * 2) / 2 / 100) %>% # round to the nearest 0.005
  drop_na() %>%
  group_by(race, bin) %>%
  summarize(n_in_bin = sum(wtmec8yr),
            bin_avg = sum(race_aware_model_pred * wtmec8yr) / sum(wtmec8yr),
            cvd_prev = sum(as.integer(cvd) * wtmec8yr) / sum(wtmec8yr)) %>%
  ggplot(aes(x=bin_avg, y=cvd_prev, color=race)) +
  geom_vline(xintercept=0.2) +
  geom_smooth(se=FALSE, method="lm") +
  # geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
  xlab("Race-aware predicted risk") +
  ylab("Observed CVD rate") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.0, incidence_upper_bound, 0.04)) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0.0, risk_score_upper_bound, 0.04)) +
  coord_cartesian(xlim = c(0, risk_score_upper_bound), ylim = c(0, incidence_upper_bound)) +
  theme(legend.title = element_blank(),
        legend.position = c(0.15, 0.84)) +
  scale_color_manual(values=group_color_map,
                     breaks =group_names)


# ===========================================================================================
# ================================ binning w/ large model ===================================
# ===========================================================================================

data %>%
  inner_join(race_blind_model_pred, by = c("seqn")) %>%
  inner_join(large_model_pred, by = c("seqn")) %>%
  select(race, cvd, race_blind_model_pred, p_cvd, wtmec8yr) %>%
  mutate(bin = round(race_blind_model_pred / 0.025) * 0.025, # round to the nearest 0.025
         bucket = ntile(race_blind_model_pred, 10)) %>% 
  drop_na() %>%
  group_by(race, bin) %>%
  summarize(n_in_bin = sum(wtmec8yr),
            bin_avg = sum(race_blind_model_pred * wtmec8yr) / sum(wtmec8yr),
            cvd_prev = sum(p_cvd * wtmec8yr) / sum(wtmec8yr)) %>%
  ggplot(aes(x=bin_avg, y=cvd_prev, color=race)) +
  geom_vline(xintercept=0.2) +
  # geom_smooth(se=FALSE) +
  geom_line() +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
  xlab("Race-unaware predicted risk") +
  ylab("Observed CVD rate") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.0, incidence_upper_bound, 0.05)) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0.0, risk_score_upper_bound, 0.05)) +
  coord_cartesian(xlim = c(0, risk_score_upper_bound), ylim = c(0, incidence_upper_bound)) +
  theme(legend.title = element_blank(),
        legend.position = c(0.15, 0.84)) +
  scale_color_manual(values=group_color_map,
                     breaks =group_names)

data %>%
  inner_join(race_aware_model_pred, by = c("seqn")) %>%
  inner_join(large_model_pred, by = c("seqn")) %>%
  select(race, cvd, race_aware_model_pred, p_cvd, wtmec8yr) %>%
  mutate(bin = round(race_aware_model_pred / 0.025) * 0.025,
         bucket = ntile(race_aware_model_pred, 10)) %>% # round to the nearest 0.005
  drop_na() %>%
  group_by(race, bin) %>%
  summarize(n_in_bin = sum(wtmec8yr),
            bin_avg = sum(race_aware_model_pred * wtmec8yr) / sum(wtmec8yr),
            cvd_prev = sum(p_cvd * wtmec8yr) / sum(wtmec8yr)) %>%
  ggplot(aes(x=bin_avg, y=cvd_prev, color=race)) +
  geom_vline(xintercept=0.2) +
  # geom_smooth(se=FALSE) +
  geom_line() +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
  xlab("Race-aware predicted risk") +
  ylab("Observed CVD rate") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.0, incidence_upper_bound, 0.05)) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0.0, risk_score_upper_bound, 0.05)) +
  coord_cartesian(xlim = c(0, risk_score_upper_bound), ylim = c(0, incidence_upper_bound)) +
  theme(legend.title = element_blank(),
        legend.position = c(0.15, 0.84)) +
  scale_color_manual(values=group_color_map,
                     breaks =group_names)

# ===========================================================================================
# ============================ decile bucketing w/ large model ==============================
# ===========================================================================================

data %>%
  inner_join(race_blind_model_pred, by = c("seqn")) %>%
  inner_join(large_model_pred, by = c("seqn")) %>%
  select(race, cvd, race_blind_model_pred, p_cvd, wtmec8yr) %>%
  mutate(bin = round(race_blind_model_pred / 0.025) * 0.025, # round to the nearest 0.025
         bucket = ntile(race_blind_model_pred, 10)) %>% 
  drop_na() %>%
  group_by(race, bucket) %>%
  summarize(n_in_bin = sum(wtmec8yr),
            bin_avg = sum(race_blind_model_pred * wtmec8yr) / sum(wtmec8yr),
            cvd_prev = sum(p_cvd * wtmec8yr) / sum(wtmec8yr)) %>%
  ggplot(aes(x=bin_avg, y=cvd_prev, color=race)) +
  geom_vline(xintercept=0.2) +
  # geom_smooth(se=FALSE) +
  geom_line() +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
  xlab("Race-unaware predicted risk") +
  ylab("Observed CVD rate") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.0, incidence_upper_bound, 0.05)) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0.0, risk_score_upper_bound, 0.05)) +
  coord_cartesian(xlim = c(0, risk_score_upper_bound), ylim = c(0, incidence_upper_bound)) +
  theme(legend.title = element_blank(),
        legend.position = c(0.15, 0.84)) +
  scale_color_manual(values=group_color_map,
                     breaks =group_names)

data %>%
  inner_join(race_aware_model_pred, by = c("seqn")) %>%
  inner_join(large_model_pred, by = c("seqn")) %>%
  select(race, cvd, race_aware_model_pred, p_cvd, wtmec8yr) %>%
  mutate(bin = round(race_aware_model_pred / 0.025) * 0.025,
         bucket = ntile(race_aware_model_pred, 10)) %>% # round to the nearest 0.005
  drop_na() %>%
  group_by(race, bucket) %>%
  summarize(n_in_bin = sum(wtmec8yr),
            bin_avg = sum(race_aware_model_pred * wtmec8yr) / sum(wtmec8yr),
            cvd_prev = sum(p_cvd * wtmec8yr) / sum(wtmec8yr)) %>%
  ggplot(aes(x=bin_avg, y=cvd_prev, color=race)) +
  geom_vline(xintercept=0.2) +
  # geom_smooth(se=FALSE) +
  geom_line() +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
  xlab("Race-aware predicted risk") +
  ylab("Observed CVD rate") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.0, incidence_upper_bound, 0.05)) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0.0, risk_score_upper_bound, 0.05)) +
  coord_cartesian(xlim = c(0, risk_score_upper_bound), ylim = c(0, incidence_upper_bound)) +
  theme(legend.title = element_blank(),
        legend.position = c(0.15, 0.84)) +
  scale_color_manual(values=group_color_map,
                     breaks =group_names)

# ===========================================================================================
# ================================ isotonic + smoothing =====================================
# ===========================================================================================
calibrated_race_blind_df <- calibrate_blind_model_pred(race_blind_model, data)
race_blind_calibration_plot_data <- get_smoothed_plot_data(calibrated_race_blind_df, data, 0, 0.5)
race_blind_calibration_plot_data %>% head()

race_blind_calibration_plot_data %>%
# data %>%
#   select(seqn, race, cvd, wtmec8yr) %>%
#   inner_join(calibrated_race_blind_df, by = c("seqn")) %>%
#   inner_join(large_model_pred, by = c("seqn")) %>%
#   mutate(calibrated_pred_bin = floor((calibrated_pred  + 0.0025) * 100 * 2) / 2 / 100) %>% # round to the nearest 0.005
#   drop_na() %>%
#   group_by(race, calibrated_pred_bin) %>%
#   summarize(n_in_bin = sum(wtmec8yr),
#             calibrated_pred = sum(calibrated_pred * wtmec8yr) / sum(wtmec8yr),
#             smoothed_cvd = sum(p_cvd * wtmec8yr) / sum(wtmec8yr)) %>%
  ggplot(aes(x=calibrated_pred, y=smoothed_cvd, color=race)) +
  geom_vline(xintercept=0.2) +
  geom_smooth(linewidth=0.75, se = FALSE, span = 5) +
  # geom_line(linewidth=0.75) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
  xlab("Race-unaware predicted risk") +
  ylab("Observed CVD rate") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.0, incidence_upper_bound, 0.04)) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0.0, risk_score_upper_bound, 0.05)) +
  coord_cartesian(xlim = c(0.0, risk_score_upper_bound), ylim = c(0, incidence_upper_bound)) +
  theme(legend.title = element_blank(),
        legend.position = c(0.15, 0.84)) +
  scale_color_manual(values=group_color_map,
                     breaks = group_names)

calibrated_race_aware_df <- calibrate_model_pred(race_aware_model, data)
race_aware_calibration_plot_data <- get_smoothed_plot_data(calibrated_race_aware_df, data, 0, 0.5)
race_aware_calibration_plot_data %>% head()

race_aware_calibration_plot_data %>%
# data %>%
#   select(seqn, race, cvd, wtmec8yr) %>%
#   inner_join(calibrated_race_aware_df, by = c("seqn")) %>%
#   inner_join(large_model_pred, by = c("seqn")) %>%
#   mutate(calibrated_pred_bin = floor((calibrated_pred  + 0.0025) * 100 * 2) / 2 / 100) %>% # round to the nearest 0.005
#   drop_na() %>%
#   group_by(race, calibrated_pred_bin) %>%
#   summarize(n_in_bin = sum(wtmec8yr),
#             calibrated_pred = sum(calibrated_pred * wtmec8yr) / sum(wtmec8yr),
#             smoothed_cvd = sum(p_cvd * wtmec8yr) / sum(wtmec8yr)) %>%
  ggplot(aes(x=calibrated_pred, y=smoothed_cvd, color=race)) +
  geom_vline(xintercept=0.2) +
  geom_smooth(linewidth=0.75, se = FALSE, span = 5) +
  # geom_line(linewidth=0.75) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
  xlab("Race-aware predicted risk") +
  ylab("Observed CVD rate") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.0, incidence_upper_bound, 0.04)) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0.0, risk_score_upper_bound, 0.05)) +
  coord_cartesian(xlim = c(0.0, risk_score_upper_bound), ylim = c(0, incidence_upper_bound)) +
  theme(legend.title = element_blank(),
        legend.position = c(0.15, 0.84)) +
  scale_color_manual(values=group_color_map,
                     breaks = group_names)

# ===========================================================================================
# ===================================== geom_smooth =========================================
# ===========================================================================================


data %>%
  inner_join(race_aware_model_pred, by = c("seqn")) %>%
  select(cvd, race, race_aware_model_pred) %>%
  mutate(cvd = as.integer(cvd)) %>%
  ggplot(aes(x = race_aware_model_pred, y = cvd, color=race)) +
  geom_smooth(se = FALSE) +
  geom_vline(xintercept=0.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
  xlab("Race-aware predicted risk") +
  ylab("Observed CVD rate") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.0, incidence_upper_bound, 0.04)) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0.0, risk_score_upper_bound, 0.05)) +
  coord_cartesian(xlim = c(0.0, risk_score_upper_bound), ylim = c(0, incidence_upper_bound)) +
  theme(legend.title = element_blank(),
        legend.position = c(0.15, 0.84)) +
  scale_color_manual(values=group_color_map,
                     breaks = group_names)


data %>%
  inner_join(race_blind_model_pred, by = c("seqn")) %>%
  select(cvd, race, race_blind_model_pred) %>%
  mutate(cvd = as.integer(cvd)) %>%
  ggplot(aes(x = race_blind_model_pred, y = cvd, color=race)) +
  geom_smooth(se = FALSE) +
  geom_vline(xintercept=0.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
  xlab("Race-unaware predicted risk") +
  ylab("Observed CVD rate") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.0, incidence_upper_bound, 0.04)) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0.0, risk_score_upper_bound, 0.05)) +
  coord_cartesian(xlim = c(0.0, risk_score_upper_bound), ylim = c(0, incidence_upper_bound)) +
  theme(legend.title = element_blank(),
        legend.position = c(0.15, 0.84)) +
  scale_color_manual(values=group_color_map,
                     breaks = group_names)

summary(race_aware_model)


# ===========================================================================================
# ======================= Figure 5: Sensitivity analysis plot ===============================
# ===========================================================================================

sensitivity_analysis <- function(data,
                                 race_aware_model_pred,
                                 race_blind_model_pred,
                                 large_model_pred,
                                 utility_vals) {
  population_benfits <- data.frame()
  for (val in utility_vals) {
    
    # ---1)--- Compute optimal thresh implied from utility value
    implied_thresh <- 1/val
    
    # ---2)--- Compute new decisions_and_utilities table
    decisions_and_utilities <-
      data %>%
      select(seqn, race, weights = wtmec8yr) %>%
      inner_join(large_model_pred, by = c("seqn")) %>%
      inner_join(race_aware_model_pred,  by = c("seqn")) %>%
      inner_join(race_blind_model_pred, by = c("seqn")) %>%
      mutate(
        race_blind_decision = race_blind_model_pred >= implied_thresh,
        race_aware_decision = race_aware_model_pred >= implied_thresh,
        expected_race_blind_utility = (screening_cost + val * p_cvd) * race_blind_decision,
        expected_race_aware_utility = (screening_cost + val * p_cvd) * race_aware_decision,
        utility_diff = (expected_race_aware_utility - expected_race_blind_utility) # in units of dollars (approx)
      ) %>%
      drop_na(utility_diff)
    
    # ---3)--- Compute and save aggregate race_aware_benefit
    population_benfit <-
      decisions_and_utilities %>%
      group_by(race) %>%
      summarize(population_benefit = sum((utility_diff) * weights)/sum(weights)) %>%
      mutate(utility_val = val)
    
    population_benfits <- bind_rows(population_benfits, population_benfit)
  }
  return(population_benfits)
}

utility_vals <- seq(2, 50, 1)
population_benefits <- sensitivity_analysis(data,
                                            race_aware_model_pred,
                                            race_blind_model_pred,
                                            large_model_pred,
                                            utility_vals)

sensitivity_analysis_points <-
  population_benefits %>%
  mutate(thresholds = 1/utility_val)

fixed_point <-
  sensitivity_analysis_points %>%
  mutate(diff = abs(thresholds - screening_thresh)) %>%
  group_by(race) %>%
  filter(diff == min(diff))

line_order <- sensitivity_analysis_points %>%
  group_by(race) %>%
  summarize(mean_benefit = mean(population_benefit)) %>%
  arrange(as.character(race)) %>%
  mutate(alph_index = row_number()) %>%
  arrange(desc(mean_benefit)) %>%
  pull(alph_index)

# This provides the color map in the right order for the legend
ordered_group_color_map <- group_color_map[line_order]
ordered_group_names <- group_names[line_order]

x_axis_label <- expression(paste(italic("r")))

sensitivity_analysis_points %>%
  ggplot(aes(x = utility_val, y = population_benefit, color = race)) +
  geom_vline(xintercept = fixed_point$utility_val) +
  geom_line() +
  geom_point(data = fixed_point, aes(x = utility_val, y = population_benefit, color = race)) +
  geom_hline(data = fixed_point, aes(yintercept = population_benefit, color = race), linetype = "dashed") +
  labs(x = x_axis_label,
       y = "Benefit from race-aware model") +
  scale_color_manual(values=ordered_group_color_map,
                     breaks = ordered_group_names) +
  theme(legend.title = element_blank(),
        legend.position = c(0.8, 0.84))


# ===========================================================================================
# ====================================== Tables =============================================

# ========================= Table 1: Utility gains by group  ========================

screening_thresh <- 0.2
utility_reward <- 5
screening_cost <- -1

utility_gains_by_group <- 
  data %>% 
  select(seqn, race, weights = wtmec8yr) %>%
  inner_join(large_model_pred, by = c("seqn")) %>%
  inner_join(race_aware_model_pred,  by = c("seqn")) %>%
  inner_join(race_blind_model_pred, by = c("seqn")) %>%
  mutate(race_aware_risk_score = race_aware_model_pred,
         race_blind_risk_score = race_blind_model_pred,
         race_aware_decision = race_aware_risk_score > screening_thresh,
         race_blind_decision = race_blind_risk_score > screening_thresh,
         expected_race_aware_utility = (screening_cost + utility_reward * p_cvd) * race_aware_decision,
         expected_race_blind_utility = (screening_cost + utility_reward * p_cvd) * race_blind_decision,
         utility_difference = expected_race_aware_utility - expected_race_blind_utility
  ) %>%
  drop_na() %>% # drop observations that have an NA prediction due to missing data
  group_by(race) %>%
  summarize(average_utility_diff = sum(utility_difference * weights)/sum(weights)) %>%
  arrange(desc(average_utility_diff))

utility_gains_by_group

# ===========================================================================================
# ==================================== Statistics ===========================================

# ======================= Overall average utility gain  ========================

overall_utility_gain <- 
  data %>% 
  select(seqn, race, weights = wtmec8yr) %>%
  inner_join(large_model_pred, by = c("seqn")) %>%
  inner_join(race_aware_model_pred,  by = c("seqn")) %>%
  inner_join(race_blind_model_pred, by = c("seqn")) %>%
  mutate(race_aware_risk_score = race_aware_model_pred,
         race_blind_risk_score = race_blind_model_pred,
         race_aware_decision = race_aware_risk_score > screening_thresh,
         race_blind_decision = race_blind_risk_score > screening_thresh,
         expected_race_aware_utility = (screening_cost + utility_reward * p_cvd) * race_aware_decision,
         expected_race_blind_utility = (screening_cost + utility_reward * p_cvd) * race_blind_decision,
         utility_difference = expected_race_aware_utility - expected_race_blind_utility
  ) %>%
  drop_na() %>% # drop observations that have an NA prediction due to missing data
  summarize(average_utility_diff = sum(utility_difference * weights)/sum(weights)) %>%
  arrange(desc(average_utility_diff))

# ====== Pct. of people with the same screening decision under both models =====
data %>%
  select(seqn, race, wtmec8yr) %>%
  inner_join(race_aware_model_pred,  by = c("seqn")) %>%
  inner_join(race_blind_model_pred, by = c("seqn")) %>%
  mutate(race_blind_risk_score = race_blind_model_pred,
         race_aware_risk_score = race_aware_model_pred) %>%
  drop_na(race) %>%
  mutate(race_blind_screening_decision = race_blind_risk_score > screening_thresh,
         race_aware_screening_decision = race_aware_risk_score > screening_thresh,
         same_decision = (race_blind_screening_decision == race_aware_screening_decision)) %>%
  group_by(race) %>%
  summarize(pct_blind_decision = sum(race_blind_screening_decision * wtmec8yr) / sum(wtmec8yr) * 100,
            pct_aware_decision = sum(race_aware_screening_decision * wtmec8yr) / sum(wtmec8yr) * 100,
            pct_same_decision = sum(same_decision * wtmec8yr) / sum(wtmec8yr) * 100) %>%
  arrange(pct_same_decision) %>%
  select(race, pct_same_decision)

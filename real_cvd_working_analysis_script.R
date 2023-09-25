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

data <- readRDS(here::here(directory_path, 'data/processed', 'cvd_data.rds')) %>%
  filter(race != "Asian")

theme_set(theme_bw(base_size = 15))

# ===========================================================================================
#====================================== Real Approach =======================================
# ====================================== Models =============================================

race_blind_formula <- cvd ~ gender + ridageyr + lbxtc + lbdldl + 
  lbdhdd + statins + sys_bp + dias_bp + hypertension_treatment + diabetes + smokes + 
  aspirin 
race_blind_model <- glm(race_blind_formula,
                          data = data,
                          family = "binomial",
                          weights = round(wtmec8yr/1000)) # glm complains when weights aren't ints
race_blind_model_pred <- predict(race_blind_model, newdata = data, type = "response")

race_aware_formula <- cvd ~ gender + ridageyr  + lbxtc + lbdldl + 
  lbdhdd + race*statins + sys_bp + dias_bp + hypertension_treatment + diabetes + race*smokes + 
  race*aspirin

race_aware_model <- glm(race_aware_formula,
                          data = data,
                          family = "binomial",
                          weights = round(wtmec8yr/1000)) # glm complains when weights aren't ints
race_aware_model_pred <- predict(race_aware_model, newdata = data, type = "response")

# large_model_formula <- cvd ~ race + (gender + ridageyr + lbxtc + lbdldl + 
#   lbdhdd + sys_bp + diabetes + smokes +
#     felt_depressed + income + health_insurance + food_security)
# large_model <- glm(large_model_formula,
#                    data = data,
#                    family = "binomial",
#                    weights = round(wtmec8yr/1000)) # glm complains when weights aren't ints
# large_model_pred <- data %>%
#   mutate(p_cvd = predict(large_model, newdata = data, type = "response")) %>%
#   select(seqn, p_cvd)

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
    race = rep(c("Black", "White", "Hispanic", "Asian"), each = ((risk_upper_bound - risk_lower_bound) * 100) + 1)
  ) %>%
    mutate(smoothed_cvd = predict(smoothing_model, newdata = ., type = "response"))
  return(plot_data)
}


# ===========================================================================================
# ======================================= Figures ===========================================

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
        legend.position = c(0.35, 0.84)) +
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
        legend.position = c(0.35, 0.84)) +
  scale_color_manual(values=group_color_map,
                     breaks = group_names)



data.frame(race_aware_pred = race_aware_model_pred,
           cvd = as.integer(data$cvd),
           race = data$race) %>% 
  filter(race != "Asian") %>%
  ggplot(aes(x = race_aware_pred, y = cvd, color=race)) +
  geom_smooth(se = FALSE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray")

# hist(race_aware_model_pred)

data.frame(race_blind_pred = race_blind_model_pred,
           cvd = as.integer(data$cvd),
           race = data$race) %>% 
  filter(race != "Asian") %>%
  ggplot(aes(x = race_blind_pred, y = cvd, color=race)) +
  geom_smooth(se = FALSE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray")

summary(race_aware_model)

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
  inner_join(calibrated_race_aware_df %>% rename(race_aware_model_pred = calibrated_pred), by = c("seqn")) %>%
  inner_join(calibrated_race_blind_df %>% rename(race_blind_model_pred = calibrated_pred), by = c("seqn")) %>%
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

# ===========================================================================================
# ==================================== Statistics ===========================================

# ======================= Overall average utility gain  ========================



overall_utility_gain <- 
  data %>% 
  select(seqn, race, weights = wtmec8yr) %>%
  inner_join(large_model_pred, by = c("seqn")) %>%
  inner_join(calibrated_race_aware_df %>% rename(race_aware_model_pred = calibrated_pred), by = c("seqn")) %>%
  inner_join(calibrated_race_blind_df %>% rename(race_blind_model_pred = calibrated_pred), by = c("seqn")) %>%
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

# Author: Madison Coots
# Date: September 14, 2023
# ======================
# Code for all figures, tables, and statistics 
# in Coots et al. (2023)

library(tidyverse)
library(readr)
library(janitor)

directory_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
save_path <- here::here(directory_path, 'figures/')

source(here::here(directory_path, 'colors.R'))

data <- readRDS(here::here(directory_path, 'data/processed', 'cvd_data.rds'))
synthetic_data <- readRDS(here::here(directory_path, 'data/processed', 'synthetic_cvd_data.rds'))

theme_set(theme_bw(base_size = 15))

# ===========================================================================================
#==================================== Synthetic Approach ====================================
# ====================================== Models =============================================

race_blind_formula <- cvd ~ gender + ridageyr + lbxtc + lbdldl + 
  lbdhdd + sys_bp + diabetes + smokes
race_blind_model <- glm(race_blind_formula,
                          data = synthetic_data,
                          family = "binomial",
                          weights = round(wtmec8yr/1000)) # glm complains when weights aren't ints
race_blind_model_pred <- predict(race_blind_model, newdata = synthetic_data, type = "response")

race_aware_formula <- cvd ~ race + gender + ridageyr + lbxtc + lbdldl + 
  lbdhdd + sys_bp + diabetes + smokes
race_aware_model <- glm(race_aware_formula,
                          data = synthetic_data,
                          family = "binomial",
                          weights = round(wtmec8yr/1000)) # glm complains when weights aren't ints
race_aware_model_pred <- predict(race_aware_model, newdata = synthetic_data, type = "response")

large_model_formula <- cvd ~ race + gender + ridageyr + lbxtc + lbdldl + 
  lbdhdd + sys_bp + diabetes + smokes + felt_depressed + income + 
  health_insurance + food_security
large_model <- glm(large_model_formula,
                   data = synthetic_data,
                   family = "binomial",
                   weights = round(wtmec8yr/1000)) # glm complains when weights aren't ints
large_model_pred <- predict(large_model, newdata = synthetic_data, type = "response")

# ===========================================================================================
# ====================================== Figures ============================================

get_smoothed_plot_data <- function(predictions, data, risk_lower_bound, risk_upper_bound) {
  smoothing_data <- data.frame(
    risk_score = predictions,
    cvd = data$cvd,
    race = data$race
  )
  smoothing_model <- glm(cvd ~ risk_score*race, data = smoothing_data, family = "binomial")
  plot_data <- data.frame(
    risk_score = seq(risk_lower_bound, risk_upper_bound, by = 0.01),
    race = rep(c("Black", "White", "Hispanic", "Asian"), each = ((risk_upper_bound - risk_lower_bound) * 100) + 1)
  ) %>%
    mutate(smoothed_cvd = predict(smoothing_model, newdata = ., type = "response"))
  return(plot_data)
}

risk_score_upper_bound <- 0.6
incidence_upper_bound <- 0.6
race_blind_calibration_plot_data <- get_smoothed_plot_data(race_blind_model_pred, synthetic_data, 0, risk_score_upper_bound)

race_blind_calibration_plot_data %>%
  ggplot(aes(x=risk_score, y=smoothed_cvd, color=race)) +
  geom_vline(xintercept=0.2) +
  geom_line() +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
  xlab("Race-unaware predicted risk") +
  ylab("Observed CVD rate") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.0, incidence_upper_bound, 0.04)) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0.0, risk_score_upper_bound, 0.06)) +
  coord_cartesian(xlim = c(0.1, risk_score_upper_bound), ylim = c(0, incidence_upper_bound)) +
  theme(legend.title = element_blank(),
        legend.position = c(0.35, 0.84)) 


race_aware_calibration_plot_data <- get_smoothed_plot_data(race_aware_model_pred, synthetic_data, 0, 0.6)

race_aware_calibration_plot_data %>%
  ggplot(aes(x=risk_score, y=smoothed_cvd, color=race)) +
  geom_vline(xintercept=0.2) +
  geom_line() +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
  xlab("Race-aware predicted risk") +
  ylab("Observed CVD rate") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.0, incidence_upper_bound, 0.04)) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0.0, risk_score_upper_bound, 0.06)) +
  coord_cartesian(xlim = c(0.1, risk_score_upper_bound), ylim = c(0, incidence_upper_bound)) +
  theme(legend.title = element_blank(),
        legend.position = c(0.35, 0.84)) 

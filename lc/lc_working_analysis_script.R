# Author: Madison Coots
# Date: November 2, 2023
# ======================
# Code for all figures, tables, and statistics 
# in Coots et al. (2023)

library(tidyverse)
library(readr)
library(janitor)
library(stats)

directory_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
save_path <- here::here(directory_path, 'figures/')
read_path <- "Documents/harvard/research/lung_cancer/data/processed/all_data_with_lc_risk.rds"

source(here::here(directory_path, 'colors.R'))

data <- readRDS(read_path) %>%
  rename(race = race_str)

theme_set(theme_bw(base_size = 15))

# ===========================================================================================
# ======================================= Figures ===========================================

risk_score_upper_bound <- 0.05
incidence_upper_bound <- 0.05
screening_thresh <- 0.015
# 
# ===========================================================================================
# ============================== Race-aware calibration plot ================================
# ===========================================================================================

data %>%
  select(race, race_aware_risk) %>%
  mutate(bin = floor((race_aware_risk  + 0.0025) * 100 * 2) / 2 / 100,
         bucket = ntile(race_aware_risk / 0.025, 10)) %>%
  group_by(race, bin) %>%
  summarize(n_in_bin = n(),
            bin_avg = mean(race_aware_risk),
            cvd_prev = mean(race_aware_risk)) %>%
  ggplot(aes(x=bin_avg, y=cvd_prev, color=race)) +
  geom_vline(xintercept=screening_thresh) +
  geom_line() + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
  xlab("Race-aware LC risk") +
  ylab("True LC Risk") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.0, incidence_upper_bound, 0.01)) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0.0, risk_score_upper_bound, 0.01)) +
  coord_cartesian(xlim = c(0, risk_score_upper_bound), ylim = c(0, incidence_upper_bound)) +
  theme(legend.title = element_blank(),
        legend.position = c(0.18, 0.81)) + 
  scale_color_manual(values=group_color_map,
                     breaks =group_names)

# ===========================================================================================
# ============================== Race-blind calibration plot ================================
# ===========================================================================================

data %>%
  select(race, race_blind_risk, race_aware_risk) %>%
  mutate(bin = floor((race_blind_risk  + 0.0025) * 100 * 2) / 2 / 100,
         bucket = ntile(race_blind_risk / 0.025, 10)) %>%
  group_by(race, bin) %>%
  summarize(n_in_bin = n(),
            bin_avg = mean(race_blind_risk),
            cvd_prev = mean(race_aware_risk)) %>%
  ggplot(aes(x=bin_avg, y=cvd_prev, color=race)) +
  geom_vline(xintercept=screening_thresh) +
  geom_line() + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
  xlab("Race-blind LC risk") +
  ylab("True LC Risk") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.0, incidence_upper_bound, 0.01)) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0.0, risk_score_upper_bound, 0.01)) +
  coord_cartesian(xlim = c(0, risk_score_upper_bound), ylim = c(0, incidence_upper_bound)) +
  theme(legend.title = element_blank(),
        legend.position = c(0.18, 0.81)) + 
  scale_color_manual(values=group_color_map,
                     breaks =group_names)

# ===========================================================================================
# ====================================== Scatter plot =======================================
# ===========================================================================================

data %>%
  select(race, race_blind_risk, race_aware_risk) %>%
  ggplot(aes(x=race_blind_risk, y=race_aware_risk, color=race)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
  geom_point(shape = 1) +
  annotate("rect", xmin = -1, xmax = screening_thresh, ymin = -1, ymax = screening_thresh,
           alpha = 0.6, fill="white") +
  annotate("rect", xmin = screening_thresh, xmax = 1, ymin = screening_thresh, ymax = 1,
           alpha = 0.6, fill="white") +
  geom_vline(xintercept=screening_thresh) +
  geom_hline(yintercept=screening_thresh) +
  xlab("Race-blind LC risk") +
  ylab("True LC risk") +
  scale_color_manual(values=group_color_map,
                     breaks =group_names) + 
  theme(legend.title = element_blank(),
        legend.position = c(0.15, 0.86)) +
  coord_cartesian(xlim = c(0, risk_score_upper_bound), ylim = c(0, risk_score_upper_bound)) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0.0, risk_score_upper_bound, 0.01)) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.0, risk_score_upper_bound, 0.01))

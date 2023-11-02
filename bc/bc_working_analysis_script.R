# Author: Madison Coots
# Date: October 31, 2023
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

data <- readRDS(here::here(directory_path, 'data/processed', 'all_data_with_bc_risk.rds')) %>%
  rename(race = race_str)

theme_set(theme_bw(base_size = 15))

# ===========================================================================================
# ======================================= Figures ===========================================

risk_score_upper_bound <- 0.025
incidence_upper_bound <- 0.025
screening_thresh <- 0.0166
# 
# ===========================================================================================
# ============================== Race-aware calibration plot ================================
# ===========================================================================================

data %>%
  select(race, race_aware_risk, wtmec8yr) %>%
  mutate(bin = floor((race_aware_risk  + 0.0025) * 100 * 2) / 2 / 100,
         bucket = ntile(race_aware_risk / 0.025, 10)) %>%
  group_by(race, bin) %>%
  summarize(n_in_bin = sum(wtmec8yr),
            bin_avg = sum(race_aware_risk * wtmec8yr) / sum(wtmec8yr),
            cvd_prev = sum(race_aware_risk * wtmec8yr) / sum(wtmec8yr)) %>%
  ggplot(aes(x=bin_avg, y=cvd_prev, color=race)) +
  geom_vline(xintercept=screening_thresh) +
  geom_line() + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
  xlab("Race-aware BC risk") +
  ylab("True BC Risk") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.0, incidence_upper_bound, 0.005)) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0.0, risk_score_upper_bound, 0.005)) +
  coord_cartesian(xlim = c(0, risk_score_upper_bound), ylim = c(0, incidence_upper_bound)) +
  theme(legend.title = element_blank(),
        legend.position = c(0.18, 0.81)) + 
  scale_color_manual(values=group_color_map,
                     breaks =group_names)

# ===========================================================================================
# ============================== Race-blind calibration plot ================================
# ===========================================================================================

data %>%
  select(race, race_blind_risk, race_aware_risk, wtmec8yr) %>%
  mutate(bin = floor((race_blind_risk  + 0.0025) * 100 * 2) / 2 / 100,
         bucket = ntile(race_blind_risk / 0.025, 10)) %>%
  group_by(race, bin) %>%
  summarize(n_in_bin = sum(wtmec8yr),
            bin_avg = sum(race_blind_risk * wtmec8yr) / sum(wtmec8yr),
            cvd_prev = sum(race_aware_risk * wtmec8yr) / sum(wtmec8yr)) %>%
  ggplot(aes(x=bin_avg, y=cvd_prev, color=race)) +
  geom_vline(xintercept=screening_thresh) +
  geom_line() + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
  xlab("Race-blind BC risk") +
  ylab("True BC Risk") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.0, incidence_upper_bound, 0.005)) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0.0, risk_score_upper_bound, 0.005)) +
  coord_cartesian(xlim = c(0, risk_score_upper_bound), ylim = c(0, incidence_upper_bound)) +
  theme(legend.title = element_blank(),
        legend.position = c(0.18, 0.81)) + 
  scale_color_manual(values=group_color_map,
                     breaks =group_names)

# ===========================================================================================
# ====================================== Scatter plot =======================================
# ===========================================================================================

data %>%
  select(race, race_blind_risk, race_aware_risk, wtmec8yr) %>%
  ggplot(aes(x=race_blind_risk, y=race_aware_risk, color=race)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
  geom_point(shape = 1) +
  annotate("rect", xmin = -1, xmax = screening_thresh, ymin = -1, ymax = screening_thresh,
           alpha = 0.6, fill="white") +
  annotate("rect", xmin = screening_thresh, xmax = 1, ymin = screening_thresh, ymax = 1,
           alpha = 0.6, fill="white") +
  geom_vline(xintercept=screening_thresh) +
  geom_hline(yintercept=screening_thresh) +
  xlab("Race-blind BC risk") +
  ylab("True BC risk") +
  scale_color_manual(values=group_color_map,
                     breaks =group_names) + 
  theme(legend.title = element_blank(),
        legend.position = c(0.15, 0.86)) +
  coord_cartesian(xlim = c(0, risk_score_upper_bound), ylim = c(0, risk_score_upper_bound)) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0.0, risk_score_upper_bound, 0.005)) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.0, risk_score_upper_bound, 0.005))
  
# ===========================================================================================
# ============================== Race-blind risk distribution ===============================
# ===========================================================================================

data %>%
  select(race, race_blind_risk, race_aware_risk, wtmec8yr) %>%
  ggplot(aes(x = race_blind_risk, weight = wtmec8yr, fill = race)) +
  facet_wrap(vars(fct_rev(race)), ncol = 2) +
  geom_histogram(binwidth = 0.001, aes(y = after_stat(count/tapply(count, PANEL, sum)[PANEL]))) +
  geom_vline(xintercept=screening_thresh) + 
  scale_fill_manual(values=group_color_map,
                    breaks =group_names) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.0, 1, 0.02)) +
  scale_x_continuous(labels = scales::percent) +
  coord_cartesian(xlim = c(0, 0.03), ylim = c(0, 0.18)) +
  xlab("Race-blind BC risk") +
  ylab("Population proportion (%)") + 
  theme(legend.title = element_blank(),
        legend.position = c(0.85, 0.86))

# ===========================================================================================
# ============================== Race-aware risk distribution ===============================
# ===========================================================================================

data %>%
  select(race, race_blind_risk, race_aware_risk, wtmec8yr) %>%
  ggplot(aes(x = race_aware_risk, weight = wtmec8yr, fill = race)) +
  facet_wrap(vars(fct_rev(race)), ncol = 2) +
  geom_histogram(binwidth = 0.001, aes(y = after_stat(count/tapply(count, PANEL, sum)[PANEL]))) +
  geom_vline(xintercept=screening_thresh) + 
  scale_fill_manual(values=group_color_map,
                    breaks =group_names) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.0, 1, 0.02)) +
  scale_x_continuous(labels = scales::percent) +
  coord_cartesian(xlim = c(0, 0.03), ylim = c(0, 0.18)) +
  xlab("Race-aware BC risk") +
  ylab("Population proportion (%)") + 
  theme(legend.title = element_blank(),
        legend.position = c(0.85, 0.86))
  
# ===========================================================================================
# ==================================== Statistics ===========================================
# ====== Pct. of people with the same screening decision under both models =====
data %>%
  select(ID, race, race_blind_risk, race_aware_risk, wtmec8yr) %>%
  mutate(race_blind_screening_decision = race_blind_risk > screening_thresh,
         race_aware_screening_decision = race_aware_risk > screening_thresh,
         same_decision = (race_blind_screening_decision == race_aware_screening_decision)) %>%
  group_by(race) %>%
  summarize(pct_blind_decision = sum(race_blind_screening_decision * wtmec8yr) / sum(wtmec8yr) * 100,
            pct_aware_decision = sum(race_aware_screening_decision * wtmec8yr) / sum(wtmec8yr) * 100,
            pct_same_decision = sum(same_decision * wtmec8yr) / sum(wtmec8yr) * 100) %>%
  arrange(pct_same_decision) %>%
  select(race, pct_same_decision)


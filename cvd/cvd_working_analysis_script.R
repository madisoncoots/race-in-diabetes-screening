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

data <- readRDS(here::here(directory_path, 'data/processed', 'all_data_with_ascvd_2013_PCE.rds')) %>%
  mutate(race = case_when(race == "black" ~ "Black",
                          race == "white" ~ "White",
                          TRUE ~ race))

theme_set(theme_bw(base_size = 15))


# ===========================================================================================
# ======================================= Figures ===========================================

risk_score_upper_bound <- 0.24
incidence_upper_bound <- 0.32
screening_thresh <- 0.075
# ===========================================================================================
# ============================== Race-aware calibration plot ================================
# ===========================================================================================

data %>%
  select(race, race_aware_ascvd_risk, wtmec8yr) %>%
  mutate(bin = round(race_aware_ascvd_risk / 0.025) * 0.025,
         bucket = ntile(race_aware_ascvd_risk / 0.025, 10)) %>%
  group_by(race, bin) %>%
  summarize(n_in_bin = sum(wtmec8yr),
            bin_avg = sum(race_aware_ascvd_risk * wtmec8yr) / sum(wtmec8yr),
            cvd_prev = sum(race_aware_ascvd_risk * wtmec8yr) / sum(wtmec8yr)) %>%
  ggplot(aes(x=bin_avg, y=cvd_prev, color=race)) +
  geom_vline(xintercept=screening_thresh) +
  geom_line() + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
  xlab("Race-aware ASCVD risk") +
  ylab("True CVD Risk") +
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
# ============================== Race-blind calibration plot ================================
# ===========================================================================================

data %>%
  select(race, race_blind_ascvd_risk, race_aware_ascvd_risk, wtmec8yr) %>%
  mutate(bin = round(race_blind_ascvd_risk / 0.025) * 0.025,
         bucket = ntile(race_blind_ascvd_risk / 0.025, 10)) %>%
  group_by(race, bin) %>%
  summarize(n_in_bin = sum(wtmec8yr),
            bin_avg = sum(race_blind_ascvd_risk * wtmec8yr) / sum(wtmec8yr),
            cvd_prev = sum(race_aware_ascvd_risk * wtmec8yr) / sum(wtmec8yr)) %>%
  ggplot(aes(x=bin_avg, y=cvd_prev, color=race)) +
  geom_vline(xintercept=screening_thresh) +
  geom_line() + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
  xlab("Race-blind ASCVD risk") +
  ylab("True CVD Risk") +
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
# ========================== Race-gender-blind calibration plot =============================
# ===========================================================================================

data %>%
  select(race, gender, race_gender_blind_ascvd_risk, race_aware_ascvd_risk, wtmec8yr) %>%
  mutate(bin = round(race_gender_blind_ascvd_risk / 0.025) * 0.025,
         bucket = ntile(race_gender_blind_ascvd_risk / 0.025, 10)) %>%
  group_by(race, gender, bin) %>%
  summarize(n_in_bin = sum(wtmec8yr),
            bin_avg = sum(race_gender_blind_ascvd_risk * wtmec8yr) / sum(wtmec8yr),
            cvd_prev = sum(race_aware_ascvd_risk * wtmec8yr) / sum(wtmec8yr)) %>%
  ggplot(aes(x=bin_avg, y=cvd_prev, color=race, linetype=gender)) +
  geom_vline(xintercept=screening_thresh) +
  geom_line() + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
  xlab("Race-gender-blind ASCVD risk") +
  ylab("True CVD Risk") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.0, incidence_upper_bound, 0.04)) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0.0, risk_score_upper_bound, 0.04)) +
  coord_cartesian(xlim = c(0, risk_score_upper_bound), ylim = c(0, incidence_upper_bound)) +
  theme(legend.title = element_blank(),
        legend.position = c(0.15, 0.7)) +
  scale_color_manual(values=group_color_map,
                     breaks =group_names)

# ===========================================================================================
# ====================================== Scatter plot =======================================
# ===========================================================================================

data %>%
  select(race, race_blind_ascvd_risk, race_aware_ascvd_risk, wtmec8yr) %>%
  ggplot(aes(x=race_blind_ascvd_risk, y=race_aware_ascvd_risk, color=race)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
  geom_point(shape = 1) +
  annotate("rect", xmin = -1, xmax = screening_thresh, ymin = -1, ymax = screening_thresh,
           alpha = 0.6, fill="white") +
  annotate("rect", xmin = screening_thresh, xmax = 1, ymin = screening_thresh, ymax = 1,
           alpha = 0.6, fill="white") +
  geom_vline(xintercept=screening_thresh) +
  geom_hline(yintercept=screening_thresh) +
  xlab("Race-blind ASCVD risk") +
  ylab("Race-aware ASCVD risk") +
  scale_color_manual(values=group_color_map,
                     breaks =group_names) + 
  theme(legend.title = element_blank(),
        legend.position = c(0.11, 0.9)) +
  coord_cartesian(xlim = c(0, risk_score_upper_bound), ylim = c(0, incidence_upper_bound)) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.0, incidence_upper_bound, 0.04)) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0.0, risk_score_upper_bound, 0.04))

# ===========================================================================================
# ===================================== Histogram plot ======================================
# ===========================================================================================

data %>%
  select(race, race_blind_ascvd_risk, race_aware_ascvd_risk, wtmec8yr) %>%
  mutate(score_diff = race_aware_ascvd_risk - race_blind_ascvd_risk) %>%
  ggplot(aes(x = score_diff, weight = wtmec8yr, fill = race)) +
  facet_wrap(vars(fct_rev(race)), ncol = 2) +
  geom_histogram(binwidth = 0.01, aes(y = after_stat(count/tapply(count, PANEL, sum)[PANEL]))) +
  scale_fill_manual(values=group_color_map,
                     breaks =group_names) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.0, 0.9, 0.1)) +
  scale_x_continuous(labels = scales::percent) +
  coord_cartesian(xlim = c(-0.2, 0.2), ylim = c(0, 0.9)) +
  xlab("Race-aware risk - race-blind risk") +
  ylab("Population proportion (%)") + 
  theme(legend.title = element_blank(),
        legend.position = c(0.85, 0.86))


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

# sensitivity_analysis_points %>%
#   ggplot(aes(x = utility_val, y = population_benefit, color = race)) +
#   geom_vline(xintercept = fixed_point$utility_val) +
#   geom_line() +
#   geom_point(data = fixed_point, aes(x = utility_val, y = population_benefit, color = race)) +
#   geom_hline(data = fixed_point, aes(yintercept = population_benefit, color = race), linetype = "dashed") +
#   labs(x = x_axis_label,
#        y = "Benefit from race-aware model") +
#   scale_color_manual(values=ordered_group_color_map,
#                      breaks = ordered_group_names) +
#   theme(legend.title = element_blank(),
#         legend.position = c(0.8, 0.84))


# ===========================================================================================
# ====================================== Tables =============================================

# ========================= Table 1: Utility gains by group  ========================

screening_thresh <- 0.2
utility_reward <- 5
screening_cost <- -1

utility_gains_by_group <- 
  data %>% 
  select(seqn, race, race_blind_ascvd_risk, race_aware_ascvd_risk, wtmec8yr) %>%
  mutate(p_cvd = race_aware_ascvd_risk,
         race_aware_decision = race_aware_ascvd_risk > screening_thresh,
         race_blind_decision = race_blind_ascvd_risk > screening_thresh,
         expected_race_aware_utility = (screening_cost + utility_reward * p_cvd) * race_aware_decision,
         expected_race_blind_utility = (screening_cost + utility_reward * p_cvd) * race_blind_decision,
         utility_difference = expected_race_aware_utility - expected_race_blind_utility
  ) %>%
  group_by(race) %>%
  summarize(average_utility_diff = sum(utility_difference * wtmec8yr)/sum(wtmec8yr)) %>%
  arrange(desc(average_utility_diff))

utility_gains_by_group

# ===========================================================================================
# ==================================== Statistics ===========================================

# ======================= Overall average utility gain  ========================

overall_utility_gain <- 
  data %>% 
  select(seqn, race, race_blind_ascvd_risk, race_aware_ascvd_risk, wtmec8yr) %>%
  mutate(p_cvd = race_aware_ascvd_risk,
         race_aware_decision = race_aware_ascvd_risk > screening_thresh,
         race_blind_decision = race_blind_ascvd_risk > screening_thresh,
         expected_race_aware_utility = (screening_cost + utility_reward * p_cvd) * race_aware_decision,
         expected_race_blind_utility = (screening_cost + utility_reward * p_cvd) * race_blind_decision,
         utility_difference = expected_race_aware_utility - expected_race_blind_utility
  ) %>%
  summarize(average_utility_diff = sum(utility_difference * wtmec8yr)/sum(wtmec8yr)) %>%
  arrange(desc(average_utility_diff))

# ====== Pct. of people with the same screening decision under both models =====
data %>%
  select(seqn, race, race_blind_ascvd_risk, race_aware_ascvd_risk, wtmec8yr) %>%
  mutate(race_blind_screening_decision = race_blind_ascvd_risk > screening_thresh,
         race_aware_screening_decision = race_aware_ascvd_risk > screening_thresh,
         same_decision = (race_blind_screening_decision == race_aware_screening_decision)) %>%
  group_by(race) %>%
  summarize(pct_blind_decision = sum(race_blind_screening_decision * wtmec8yr) / sum(wtmec8yr) * 100,
            pct_aware_decision = sum(race_aware_screening_decision * wtmec8yr) / sum(wtmec8yr) * 100,
            pct_same_decision = sum(same_decision * wtmec8yr) / sum(wtmec8yr) * 100) %>%
  arrange(pct_same_decision) %>%
  select(race, pct_same_decision)

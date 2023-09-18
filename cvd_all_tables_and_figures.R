# Author: Madison Coots
# Date: June 5, 2023
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

theme_set(theme_bw(base_size = 15))

# ===========================================================================================
# ====================================== Models =============================================

# Risk factors for CVD: including older age, high blood pressure, 
# current smoking, abnormal lipid levels, diabetes, obesity, and physical inactivity
# PAQ706 - Days physically active at least 60 min. <--- something weird about why this is NA when we 
# inner join with bp_and_chol

# Inputs to ASCVD Risk Model: 
# Gender (gender)
# Age (ridageyr)
# Race (race)
# Total cholesterol (lbxtc)
# LDL cholesterol (lbdldl)
# HDL cholesterol (lbdhdd)
# Treatment with statin (statins)
# Systolic blood pressure (sys_bp)
# Treatment for hypertension (hypertension_treatment)
# History of diabetes (diabetes)
# Current smoker (smokes)
# Aspirin therapy (aspirin)

# This is our race-aware logistic regression model
# race_aware_formula <- cvd ~ (ridageyr*bmxbmi) + race + as.factor(ntile(data$bmxbmi, 10))
race_aware_formula <- cvd ~ ridageyr + bpq020 + smokes + bpq080 + diabetes + bmxbmi + gender + race
race_aware_model <- glm(race_aware_formula,
                        data = data,
                        family = "binomial",
                        weights = round(wtmec8yr/1000)) # glm complains when weights aren't ints
race_aware_model_pred <- predict(race_aware_model, newdata = data, type = "response")

# This is our race-blind logistic regression model
race_blind_formula <- cvd ~ ridageyr + bpq020 + smokes + bpq080 + diabetes + bmxbmi + gender
race_blind_model <- glm(race_blind_formula,
                        data = data,
                        family = "binomial",
                        weights = round(wtmec8yr/1000)) # glm complains when weights aren't ints
race_blind_model_pred <- predict(race_blind_model, newdata = data, type = "response")

# We use this race-aware model to approximate ground-truth diabetes incidence rates in subsequent analysis
# mcq300a is if close relative had heart attack
large_model_formula <- cvd ~ race + ridageyr + bpq020 + smokes + bpq080 + diabetes + bmxbmi + gender +
  mcq300a + felt_depressed + income + health_insurance

large_model <- glm(large_model_formula,
                   data = data,
                   family = "binomial",
                   weights = round(wtmec8yr/1000)) # glm complains when weights aren't ints

large_model_pred <- predict(large_model, newdata = data, type = "response")

# We use this model to smooth out the utility curve in Figure 3
smoothed_model_formula <- diabetes ~ race*poly(race_blind_risk_score, 3)
smoothed_model <- glm(smoothed_model_formula,
                      data = data %>% mutate(race_blind_risk_score = race_blind_model_pred),
                      family = "binomial",
                      weights = round(wtmec8yr/1000))

smoothed_model_pred <- predict(smoothed_model, newdata = data %>% mutate(race_blind_risk_score = race_blind_model_pred), type = "response")

# This is our kitchen sink race-blind model used to illustrate that the miscalibration
# issue is not resolved with the inclusion of additional covariates likely correlated
# with race
extended_race_blind_formula <- cvd ~ ridageyr + bpq020 + smokes + bpq080 + diabetes + bmxbmi + gender +
  mcq300a + felt_depressed + income + health_insurance


extended_race_blind_model <- glm(extended_race_blind_formula,
                                 data = data,
                                 family = "binomial",
                                 weights = round(wtmec8yr/1000)) # glm complains when weights aren't ints

extended_race_blind_model_pred <- predict(extended_race_blind_model, newdata = data, type = "response")

# ===========================================================================================
# ====================================== Figures ============================================

# ============================ Figure 1: Calibration plot ===========================

smoothing_data <- data.frame(cvd = data$cvd, 
                            race_blind_model_pred = race_blind_model_pred,
                            race = data$race)
smoothing_model <- glm(cvd ~ race_blind_model_pred + race, data = smoothing_data)
smoothed_pred <- predict(smoothing_model, newdata = smoothing_data, type = "response")

# race_blind_calibration_plot_data <- data %>%
#   mutate(risk_score = smoothed_pred,
#          est_cvd_prob = large_model_pred) %>%
#   filter(!is.na(risk_score),
#          !is.na(est_cvd_prob)) %>%
#   select(race, risk_score, est_cvd_prob, wtmec8yr) %>%
#   mutate(risk_score_bin = floor((risk_score  + 0.0025) * 100 * 2) / 2 / 100) %>% # round to the nearest 0.005
#   group_by(race, risk_score_bin) %>%
#   summarize(n_in_bin = sum(wtmec8yr),
#             bin_avg_risk_score = sum(risk_score * wtmec8yr) / sum(wtmec8yr),
#             cvd_prev = sum(est_cvd_prob * wtmec8yr) / sum(wtmec8yr))

race_aware_smoothing_data <- data.frame(cvd = data$cvd, 
                             race_aware_model_pred = race_aware_model_pred,
                             race = data$race)
race_aware_smoothing_model <- glm(cvd ~ race_aware_model_pred*race, data = race_aware_smoothing_data)
smoothed_pred <- predict(smoothing_model, newdata = smoothing_data, type = "response")

# race_aware_calibration_plot_data <- data %>%
#   mutate(risk_score = race_aware_model_pred,
#          est_cvd_prob = large_model_pred) %>%
#   filter(!is.na(risk_score),
#          !is.na(est_cvd_prob)) %>%
#   select(race, risk_score, est_cvd_prob, wtmec8yr) %>%
#   mutate(risk_score_bin = floor((risk_score  + 0.0025) * 100 * 2) / 2 / 100) %>% # round to the nearest 0.005
#   group_by(race, risk_score_bin) %>%
#   summarize(n_in_bin = sum(wtmec8yr),
#             bin_avg_risk_score = sum(risk_score * wtmec8yr) / sum(wtmec8yr),
#             cvd_prev = sum(est_cvd_prob * wtmec8yr) / sum(wtmec8yr))

# This chunk determines the vertical order of the lines in the plot
# so that we can have the order of the lines in the legend reflect
# the order of the lines in the plot so that it is easier to read
risk_score_upper_bound <- 0.2
incidence_upper_bound <- 0.24
line_order <- race_blind_calibration_plot_data %>%
  # Make sure x-range lines up with what is visualized in plot
  filter(risk_score_bin < risk_score_upper_bound) %>%
  group_by(race) %>%
  summarize(mean_prev = mean(cvd_prev)) %>%
  arrange(as.character(race)) %>%
  mutate(alph_index = row_number()) %>%
  arrange(desc(mean_prev)) %>%
  pull(alph_index)

# This provides the color map in the right order for the legend
ordered_group_color_map <- group_color_map[line_order]
ordered_group_names <- group_names[line_order]


df <- data.frame(
  race_blind_model_pred = seq(0, 0.2, by = 0.01),
  race = rep(c("Black", "White", "Hispanic", "Asian"), each = 21)
) %>%
  mutate(smoothed_cvd = predict(smoothing_model, newdata = ., type = "response"))

df %>%
  ggplot(aes(x=race_blind_model_pred, y=smoothed_cvd, color=race)) +
  geom_vline(xintercept=0.1) +
  geom_line() +
  # geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
  xlab("Race-unaware predicted risk") +
  ylab("Observed cvd rate") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.0, incidence_upper_bound, 0.04)) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0.0, risk_score_upper_bound, 0.02)) +
  coord_cartesian(xlim = c(0, risk_score_upper_bound), ylim = c(0, incidence_upper_bound)) +
  theme(legend.title = element_blank(),
        legend.position = c(0.35, 0.84)) +
  scale_color_manual(values=ordered_group_color_map,
                     breaks = ordered_group_names)

ggsave(paste(save_path, "race_blind_calibration_plot.pdf", sep = ""),
       width = 5.25,
       height = 5)



race_aware_df <- data.frame(
  race_aware_model_pred = seq(0, 0.2, by = 0.01),
  race = rep(c("Black", "White", "Hispanic", "Asian"), each = 21)
) %>%
  mutate(smoothed_cvd = predict(race_aware_smoothing_model, newdata = ., type = "response"))

race_aware_df %>%
  ggplot(aes(x=race_aware_model_pred, y=smoothed_cvd, color=race)) +
  geom_vline(xintercept=0.1) +
  geom_line() +
  # geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
  xlab("Race-aware predicted risk") +
  ylab("Observed cvd rate") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.0, incidence_upper_bound, 0.04)) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0.0, risk_score_upper_bound, 0.02)) +
  coord_cartesian(xlim = c(0, risk_score_upper_bound), ylim = c(0, incidence_upper_bound)) +
  theme(legend.position = "none") +
  scale_color_manual(values=group_color_map,
                     breaks = c("White", "Hispanic", "Black", "Asian"))

ggsave(paste(save_path, "race_aware_calibration_plot.pdf", sep = ""),
       width = 5.25,
       height = 5)

# ========================= Figure 2: Subgroup analysis plot ========================

utility_reward <- 70
screening_cost <- -1
optimal_thresh <- 1 / (utility_reward)

subgroup_analysis_data <- 
  data %>%
  select(race, ridageyr, bmxbmi, diabetes, weights = wtmec8yr) %>%
  mutate(p_diabetes = large_model_pred,
         race_aware_risk_score = race_aware_model_pred,
         race_blind_risk_score = race_blind_model_pred,
         race_aware_decision = race_aware_risk_score > optimal_thresh,
         race_blind_decision = race_blind_risk_score > optimal_thresh,
         expected_race_aware_utility = (screening_cost + utility_reward * p_diabetes) * race_aware_decision,
         expected_race_blind_utility = (screening_cost + utility_reward * p_diabetes) * race_blind_decision,
         utility_difference = expected_race_aware_utility - expected_race_blind_utility
  )

subgroup_plot_data <- 
  subgroup_analysis_data %>%
  filter(!is.na(p_diabetes)) %>%
  mutate(rounded_bmi = round(bmxbmi),
         age_bin = case_when(
           ridageyr < 30 ~ "20s_younger",
           (ridageyr >= 30) & (ridageyr < 40) ~ "30s",
           (ridageyr >= 40) & (ridageyr < 50) ~ "40s",
           (ridageyr >= 50) & (ridageyr < 60) ~ "50s",
           (ridageyr >= 60) & (ridageyr < 70) ~ "60s",
           ridageyr >= 30 ~ "70s_plus"
         ),
         bmi_bin = ntile(bmxbmi, 10)) %>%
  group_by(age_bin, race, bmi_bin) %>% 
  summarize(mean_utility_difference = sum(utility_difference * weights)/sum(weights),
            mean_race_blind_utility = sum(expected_race_blind_utility * weights)/sum(weights),
            prop_screened_race_aware_model = sum(race_aware_decision * weights) / sum(weights),
            prop_screened_race_blind_model = sum(race_blind_decision * weights) / sum(weights),
            group_size = sum(weights),
            group_obs = n()) %>%
  arrange(desc(abs(mean_utility_difference)))

subgroup_plot_data %>%
  ggplot(aes(x = mean_race_blind_utility, y = mean_utility_difference, 
             size = group_size, color = race)) +
  facet_wrap(vars(fct_rev(race))) + 
  geom_hline(yintercept = 0) +
  annotate("rect", xmin = -1, xmax = 70, ymin = 2, ymax = 0,
           alpha = .1) +
  geom_point(shape = 1) +
  coord_cartesian(xlim=c(-1,70), ylim = c(-0.5,1)) +
  xlab("Avg. race-unaware utility") +
  ylab("Avg. utility diff.\n(race-aware - race-unaware)") +
  scale_color_manual(values=group_color_map,
                     breaks = c("White", "Hispanic", "Black", "Asian")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "none")

ggsave(paste(save_path, "subgroup_plot.pdf", sep = ""),
       width = 5.25,
       height = 5)

# ======================== Figure 3: Utility differences plot =======================

race_blind_utilities <- 
  data %>% 
  select(race, weights = wtmec8yr, diabetes) %>%
  mutate(smoothed_model_pred = smoothed_model_pred,
         race_blind_risk_score = race_blind_model_pred,
         screening_utility = (screening_cost + utility_reward * smoothed_model_pred))

race_blind_utilities_plot_data <- 
  race_blind_utilities %>%
  drop_na() %>%
  filter(race == "Asian" | race == "White") 

asian_area <- 
  race_blind_utilities_plot_data %>%
  filter(race == "Asian") %>%
  filter(race_blind_risk_score >= 0,
         race_blind_risk_score <= optimal_thresh)

white_area <- 
  race_blind_utilities_plot_data %>%
  filter(race == "White") %>%
  filter(race_blind_risk_score >= 0,
         race_blind_risk_score >= optimal_thresh,
         screening_utility <= 0)

race_blind_utilities_plot_data %>%
  ggplot(aes(x = race_blind_risk_score, y = screening_utility*100, color = race)) +
  annotate("rect", xmin = optimal_thresh, xmax = 1, ymin = -20*100, ymax = 20*100,
           alpha = .075) + 
  geom_line() +
  geom_ribbon(data = asian_area, aes(ymin=0,ymax=screening_utility*100), fill="darkgray", alpha=0.95, show.legend = FALSE) +
  geom_ribbon(data = white_area, aes(ymin=screening_utility*100,ymax=0), fill="darkgray", alpha=0.95, show.legend = FALSE) +
  geom_vline(xintercept=optimal_thresh) +
  geom_hline(yintercept=0) +
  coord_cartesian(xlim = c(0, risk_score_upper_bound), ylim = c(-500,1800)) +
  scale_color_manual(values=group_color_map,
                     breaks = c("Asian", "White")) +
  xlab("Race-unaware risk score") +
  ylab("Utility ($)")  +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0.0, risk_score_upper_bound + 0.001, 0.02)) +
  theme(legend.title = element_blank(),
        legend.position = c(0.15, 0.89))

ggsave(paste(save_path, "race_blind_utilities_plot.pdf", sep = ""),
       width = 5.25,
       height = 5)

# ====================== Figure 4: Risk score distribution plot =====================

binned_optimal_thresh <- floor((optimal_thresh  + 0.0025) * 100 * 2) / 2 / 100
white_screening_utility_crossing_point <- 
  race_blind_utilities %>%
  filter(race == "White") %>%
  mutate(diff = abs(0 - screening_utility)) %>%
  filter(diff == min(diff)) %>%
  pull(race_blind_risk_score)

binned_crossing_point <- floor((white_screening_utility_crossing_point  + 0.0025) * 100 * 2) / 2 / 100

risk_scores <- 
  data.frame(race_blind_risk_score = race_blind_model_pred,
             race_aware_risk_score = race_aware_model_pred,
             smoothed_model_pred,
             race = data$race,
             weights = data$wtmec8yr) %>%
  filter(!is.na(race)) %>%
  mutate(blind_screened = race_blind_risk_score > optimal_thresh,
         aware_screened = race_aware_risk_score > optimal_thresh,
         wrong = blind_screened != aware_screened,
         wrong = if_else(wrong, "wrong", "ok"),
         screening_utility = (screening_cost + utility_reward * smoothed_model_pred)) %>%
  mutate(fill_asian = (race == "Asian") & (race_blind_risk_score >= 0) & (race_blind_risk_score <= binned_optimal_thresh),
         fill_white = (race == "White") & (race_blind_risk_score >= 0) & (race_blind_risk_score >= binned_optimal_thresh) & (race_blind_risk_score <= binned_crossing_point),
         fill = fill_white | fill_asian,
         fill = if_else(fill, "Error", "No error"))

risk_scores %>%
  filter(race == "Asian" | race == "White") %>%
  ggplot(aes(x = race_blind_risk_score, weight = weights)) +
  facet_wrap(vars(fct_rev(race)), ncol = 1) + 
  annotate("rect", xmin = 0.015, xmax = 1, ymin = -1, ymax = 1,
           alpha = .075) + 
  # take a look at freq-poly
  geom_histogram(binwidth = 0.0025, boundary = 0, aes(y = after_stat(count/tapply(count, PANEL, sum)[PANEL]), color = race,),show.legend = FALSE, size = 0.8) +
  geom_histogram(binwidth = 0.0025, boundary = 0, aes(y = after_stat(count/tapply(count, PANEL, sum)[PANEL]), fill = fill), show.legend = FALSE, size = 5) +
  xlab("Race-unaware risk score") +
  ylab("Population proportion") + 
  geom_vline(xintercept=0.015) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.0, 0.1, 0.02)) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0.0, 0.1, 0.02)) +
  coord_cartesian(xlim = c(0, risk_score_upper_bound + 0.001), ylim = c(0, 0.07)) +
  theme(legend.title = element_blank(),
        legend.position = c(0.78, 0.93),
        legend.background = element_blank()) +
  scale_fill_manual(values = c("Error" = "darkgray", "No error" = "white"),
                    breaks = c("Error", "No error")) +
  scale_color_manual(values=group_color_map,
                     breaks = c("Asian", "White"))

ggsave(paste(save_path, "histogram_plot.pdf", sep = ""),
       width = 5.25,
       height = 5)

# ======================= Figure 5: Sensitivity analysis plot =======================

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
      data.frame(
        race = data$race,
        diabetes = data$diabetes,
        race_blind_risk_score = race_blind_model_pred,
        race_aware_risk_score = race_aware_model_pred,
        p_diabetes = large_model_pred,
        weights = data$wtmec8yr) %>%
      mutate(
        race_blind_decision = race_blind_risk_score >= implied_thresh,
        race_aware_decision = race_aware_risk_score >= implied_thresh,
        expected_race_blind_utility = (screening_cost + val * p_diabetes) * race_blind_decision,
        expected_race_aware_utility = (screening_cost + val * p_diabetes) * race_aware_decision,
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

utility_vals <- seq(30, 110, 1)
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
  mutate(diff = abs(thresholds - optimal_thresh)) %>%
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

ggsave(paste(save_path, "sensitivity_analysis.pdf", sep = ""),
       width = 5.25,
       height = 5)

# =============== Figure 6: Kitchen sink race-blind calibration plot ===============

extended_race_blind_calibration_plot_data <- data %>%
  mutate(risk_score = extended_race_blind_model_pred,
         est_diabetes_prob = large_model_pred) %>%
  filter(!is.na(risk_score),
         !is.na(est_diabetes_prob)) %>%
  select(race, risk_score, est_diabetes_prob, wtmec8yr) %>%
  mutate(risk_score_bin = floor((risk_score  + 0.0025) * 100 * 2) / 2 / 100) %>% # round to the nearest 0.005
  group_by(race, risk_score_bin) %>%
  summarize(n_in_bin = sum(wtmec8yr),
            bin_avg_risk_score = sum(risk_score * wtmec8yr) / sum(wtmec8yr),
            diabetes_prev = sum(est_diabetes_prob * wtmec8yr) / sum(wtmec8yr))

risk_score_upper_bound <- 0.05
line_order <- extended_race_blind_calibration_plot_data %>%
  # Make sure x-range lines up with what is visualized in plot
  filter(risk_score_bin < risk_score_upper_bound) %>%
  group_by(race) %>%
  summarize(mean_prev = mean(diabetes_prev)) %>%
  arrange(as.character(race)) %>%
  mutate(alph_index = row_number()) %>%
  arrange(desc(mean_prev)) %>%
  pull(alph_index)

# This provides the color map in the right order for the legend
ordered_group_color_map <- group_color_map[line_order]
ordered_group_names <- group_names[line_order]

extended_race_blind_calibration_plot_data %>%
  ggplot(aes(x=bin_avg_risk_score, y=diabetes_prev, color=race)) +
  geom_vline(xintercept=0.015) +
  geom_line() +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "darkgray") +
  xlab("Extended race-unaware predicted risk") +
  ylab("Observed diabetes rate") +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0.0, 0.24, 0.04)) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0.0, risk_score_upper_bound, 0.02)) +
  coord_cartesian(xlim = c(0, risk_score_upper_bound), ylim = c(0, 0.24)) +
  theme(legend.title = element_blank(),
        legend.position =  c(0.35, 0.84)) +
  scale_color_manual(values=ordered_group_color_map,
                     breaks = ordered_group_names)

ggsave(paste(save_path, "extended_race_blind_calibration_plot.pdf", sep = ""),
       width = 5.25,
       height = 5)

# ===========================================================================================
# ====================================== Tables =============================================

# ========================= Table 1: Utility gains by group  ========================

utility_gains_by_group <- 
  data %>% 
  select(race, weights = wtmec8yr) %>%
  mutate(p_diabetes = large_model_pred,
         race_aware_risk_score = race_aware_model_pred,
         race_blind_risk_score = race_blind_model_pred,
         race_aware_decision = race_aware_risk_score > optimal_thresh,
         race_blind_decision = race_blind_risk_score > optimal_thresh,
         expected_race_aware_utility = (screening_cost + utility_reward * p_diabetes) * race_aware_decision,
         expected_race_blind_utility = (screening_cost + utility_reward * p_diabetes) * race_blind_decision,
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
  select(race, weights = wtmec8yr) %>%
  mutate(p_diabetes = large_model_pred,
         race_aware_risk_score = race_aware_model_pred,
         race_blind_risk_score = race_blind_model_pred,
         race_aware_decision = race_aware_risk_score > optimal_thresh,
         race_blind_decision = race_blind_risk_score > optimal_thresh,
         expected_race_aware_utility = (screening_cost + utility_reward * p_diabetes) * race_aware_decision,
         expected_race_blind_utility = (screening_cost + utility_reward * p_diabetes) * race_blind_decision,
         utility_difference = expected_race_aware_utility - expected_race_blind_utility
  ) %>%
  drop_na() %>% # drop observations that have an NA prediction due to missing data
  summarize(average_utility_diff = sum(utility_difference * weights)/sum(weights)) %>%
  arrange(desc(average_utility_diff))

# ========== The number of Asian people with a race-blind score of ~1% =========
race_blind_calibration_plot_data %>%
  filter(race == "Asian", risk_score_bin == 0.01) %>%
  pull(n_in_bin)

# ====== Pct. of people with the same screening decision under both models =====
data %>%
  mutate(race_blind_risk_score = race_blind_model_pred,
         race_aware_risk_score = race_aware_model_pred) %>%
  drop_na(race) %>%
  mutate(race_blind_screening_decision = race_blind_risk_score > optimal_thresh,
         race_aware_screening_decision = race_aware_risk_score > optimal_thresh,
         same_decision = (race_blind_screening_decision == race_aware_screening_decision)) %>%
  group_by(race) %>%
  summarize(pct_blind_decision = sum(race_blind_screening_decision * wtmec8yr) / sum(wtmec8yr) * 100,
            pct_aware_decision = sum(race_aware_screening_decision * wtmec8yr) / sum(wtmec8yr) * 100,
            pct_same_decision = sum(same_decision * wtmec8yr) / sum(wtmec8yr) * 100) %>%
  arrange(pct_same_decision)

# ============= Resource-constrained overall average utility gain ==============

# Computes scarcity threshold when we limit to the top K% 
compute_scarcity_thresh_using_pct <- function(data, risk_score, pct_ppl_we_can_screen) {
  data %>%
    select(race, weights = wtmec8yr) %>%
    mutate(risk_score = risk_score) %>%
    drop_na(risk_score) %>%
    arrange(desc(risk_score)) %>%
    mutate(cumulative_sum = cumsum(weights),
           cumulative_pct = cumulative_sum / sum(weights) * 100) %>%
    filter(cumulative_pct >= pct_ppl_we_can_screen) %>%
    pull(risk_score) %>%
    first(default = NA)
}

# Can only treat test 50% of ppl
race_blind_scarcity_thresh <- compute_scarcity_thresh_using_pct(data, 
                                                                race_blind_model_pred, 
                                                                pct_ppl_we_can_screen = 50)
race_aware_scarcity_thresh <- compute_scarcity_thresh_using_pct(data, 
                                                                race_aware_model_pred, 
                                                                pct_ppl_we_can_screen = 50)

# Overall avg. utility diff
data %>% 
  select(race, weights = wtmec8yr) %>%
  mutate(p_diabetes = large_model_pred,
         race_aware_risk_score = race_aware_model_pred,
         race_blind_risk_score = race_blind_model_pred,
         race_aware_decision = race_aware_risk_score > race_aware_scarcity_thresh,
         race_blind_decision = race_blind_risk_score > race_blind_scarcity_thresh,
         expected_race_aware_utility = (screening_cost + utility_reward * p_diabetes) * race_aware_decision,
         expected_race_blind_utility = (screening_cost + utility_reward * p_diabetes) * race_blind_decision,
         utility_difference = expected_race_aware_utility - expected_race_blind_utility
  ) %>%
  drop_na() %>% # drop observations that have an NA prediction due to missing data
  summarize(average_utility_diff = sum(utility_difference * weights) / sum(weights),
            average_race_blind_utility = sum(expected_race_blind_utility * weights) / sum(weights),
            average_race_aware_utility = sum(expected_race_aware_utility * weights) / sum(weights)) %>%
  arrange(desc(average_utility_diff))

# Avg. utility diff by group
data %>% 
  select(race, weights = wtmec8yr) %>%
  mutate(p_diabetes = large_model_pred,
         race_aware_risk_score = race_aware_model_pred,
         race_blind_risk_score = race_blind_model_pred,
         race_aware_decision = race_aware_risk_score > race_aware_scarcity_thresh,
         race_blind_decision = race_blind_risk_score > race_blind_scarcity_thresh,
         expected_race_aware_utility = (screening_cost + utility_reward * p_diabetes) * race_aware_decision,
         expected_race_blind_utility = (screening_cost + utility_reward * p_diabetes) * race_blind_decision,
         utility_difference = expected_race_aware_utility - expected_race_blind_utility
  ) %>%
  drop_na() %>% # drop observations that have an NA prediction due to missing data
  group_by(race) %>%
  summarize(average_utility_diff = sum(utility_difference * weights) / sum(weights)) %>%
  arrange(desc(average_utility_diff))

# ===============================================================================


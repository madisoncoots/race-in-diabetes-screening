# Author: Madison Coots
# Date: September 27, 2023
# ----------------------

library(tidyverse)
library(readr)
library(janitor)
library(stats)

directory_path <- dirname(rstudioapi::getActiveDocumentContext()$path)
save_path <- here::here(directory_path, 'figures/')

source(here::here(directory_path, 'colors.R'))
source(here::here(directory_path, '2018_ascvd_constants.R'))

data <- readRDS(here::here(directory_path, 'data/processed', 'cvd_data.rds'))

# ======================================================================
# Function that constructs the features used to compute 10-yr ASCVD risk
# ======================================================================
make_model_data <- function(data) {
  model_data <- data %>%
    mutate(
      is_black = (race == "Black"),
      sq_sys_bp = sys_bp^2,
      tc_hdl_ratio = lbxtc / lbdhdd,
      age_if_black = if_else(is_black, ridageyr, 0),
      sys_bp_if_ht_treat = if_else(hypertension_treatment, sys_bp, 0),
      sys_bp_if_black = if_else(is_black, sys_bp, 0),
      black_and_ht_treatment = (is_black & hypertension_treatment),
      age_times_sys_bp = ridageyr * sys_bp,
      black_and_diabetes = (is_black & diabetes),
      black_and_smokes = (is_black & smokes),
      tc_hdl_ratio_if_black = if_else(is_black, tc_hdl_ratio, 0),
      sys_bp_if_black_and_hp_treatment = sys_bp_if_black * hypertension_treatment,
      age_times_sys_bp_if_black = ridageyr * sys_bp_if_black) %>%
    mutate_if(is.logical, as.integer)
  return(model_data)
}

model_data <- make_model_data(data)
 


# ======================================================================
# Function that predicts 10-yr ASCVD risk given input data
# ======================================================================
predict_10_yr_cvd_risk <- function(coef, data) {
  coef_no_intercept = coef[-1]
  covariates <- names(coef_no_intercept) # names of the covariates minus the intercept
  data <- data %>% 
    select(any_of(covariates))
  formula <- reformulate(paste(c("1 + ", paste(names(coef_no_intercept), collapse = " + ")), collapse = " "))
  model_data <- model.matrix(formula, data)[,]
  dot_product <- c(model_data %*% coef)
  ascvd_risk <- 1 / (1 + exp(-dot_product))
  return(ascvd_risk)
}

# ======================================================================
# Test cases
# ======================================================================

test_observation <- make_model_data(
  data.frame(ridageyr = 43,
             lbxtc = 169,
             lbdhdd = 73,
             sys_bp = 102,
             hypertension_treatment = FALSE,
             smokes= TRUE,
             diabetes = FALSE,
             race = "Black")
)

# Black woman (should be ~1.8%)
predict_10_yr_cvd_risk(coef = women_coef,
                       data = test_observation)

predict_10_yr_cvd_risk(coef = women_coef,
                       data = model_data) %>% head()

# Black man (should be ~3.5%)
# (There is a mistake in the documentation that leads to the stated estimate
# being ~3.4% but the computed value of tc_hdl_ratio is incorrect.)
predict_10_yr_cvd_risk(coef = men_coef,
                       data = test_observation)


# ======================================================================
# Making race-aware ASCVD predictions (standard)
# ======================================================================

women_data <- model_data %>%
  filter(gender == "Woman") %>%
  mutate(race_aware_ascvd_risk = predict_10_yr_cvd_risk(coef = women_coef,
                                                        data = .))

men_data <- model_data %>%
  filter(gender == "Man")  %>%
  mutate(race_aware_ascvd_risk = predict_10_yr_cvd_risk(coef = men_coef,
                                                        data = .))

all_data_with_race_aware_ascvd <- bind_rows(
  women_data,
  men_data
)

# ======================================================================
# Making race-blind ASCVD predictions (non-standard)
# ======================================================================

race_breakdown <- model_data %>%
  group_by(race) %>%
  summarize(n = sum(wtmec8yr)) %>%
  ungroup() %>%
  mutate(prop = n / sum(n))

prop_black <- race_breakdown %>% filter(race == "Black") %>% pull(prop)
prop_white <- race_breakdown %>% filter(race == "White") %>% pull(prop)

# Approximated from breakdown of study cohort used for training the revised PCEs
prop_black <- 0.3
prop_white <- 0.7

women_data <- model_data %>%
  filter(gender == "Woman") %>%
  mutate(white_ascvd_risk = predict_10_yr_cvd_risk(coef = women_coef,
                                                   data = mutate(., is_black = 0)),
         black_ascvd_risk = predict_10_yr_cvd_risk(coef = women_coef,
                                                   data = mutate(., is_black = 1)),
         race_blind_ascvd_risk = (prop_white * white_ascvd_risk + prop_black * black_ascvd_risk))

men_data <- model_data %>%
  filter(gender == "Man") %>%
  mutate(white_ascvd_risk = predict_10_yr_cvd_risk(coef = women_coef,
                                                   data = mutate(., is_black = 0)),
         black_ascvd_risk = predict_10_yr_cvd_risk(coef = women_coef,
                                                   data = mutate(., is_black = 1)),
         race_blind_ascvd_risk = (prop_white * white_ascvd_risk + prop_black * black_ascvd_risk))

all_data_with_race_blind_ascvd <- bind_rows(
  women_data,
  men_data
)  

# ======================================================================
# Saving data and both predictions
# ======================================================================

all_data_with_both_ascvd <-
  all_data_with_race_aware_ascvd %>%
  left_join(all_data_with_race_blind_ascvd %>%
              select(seqn, race_blind_ascvd_risk),
            by = c("seqn"))
  
saveRDS(all_data_with_both_ascvd, file = paste(data_object_write_path, "all_data_with_ascvd_2018_PCE.rds", sep = ""))  
  
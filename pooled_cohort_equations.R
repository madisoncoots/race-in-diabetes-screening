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
source(here::here(directory_path, 'ascvd_constants.R'))

data <- readRDS(here::here(directory_path, 'data/processed', 'cvd_data.rds'))

# ======================================================================
# Function that constructs the features used to compute 1--yr ASCVD risk
# ======================================================================
make_model_data <- function(data) {
  model_data <- data %>%
    mutate(ln_age = log(ridageyr),
           ln_age_sq = ln_age^2,
           ln_tchol = log(lbxtc),
           ln_age_ln_tchol = ln_age * ln_tchol,
           ln_hdl = log(lbdhdd),
           ln_age_ln_hdl = ln_age * ln_hdl,
           ln_treated_sys_bp = if_else(
             hypertension_treatment, log(sys_bp), 0
           ),
           ln_age_ln_treated_sys_bp = ln_age * ln_treated_sys_bp,
           ln_untreated_sys_bp = if_else(
             !hypertension_treatment, log(sys_bp), 0
           ),
           ln_age_ln_untreated_sys_bp = ln_age * ln_untreated_sys_bp,
           ln_age_smokes = ln_age * smokes)  %>%
    mutate_if(is.logical, as.integer)
  return(model_data)
}

model_data <- make_model_data(data)

# ======================================================================
# Function that predicts 10-yr ASCVD risk given input data
# ======================================================================
predict_10_yr_cvd_risk <- function(coef, data, mean_val, baseline_survival) {
  covariates <- names(coef)
  data <- data %>% 
    select(any_of(covariates))
  formula <- reformulate(paste(c("-1 + ", paste(names(coef), collapse = " + ")), collapse = " "))
  model_data <- model.matrix(formula, data)[,]
  dot_product <- c(model_data %*% coef)
  ten_yr_risk <- 1 - (baseline_survival^exp(dot_product - mean_val))
  return(ten_yr_risk)
}

# ======================================================================
# Test cases
# ======================================================================

test_observation <- make_model_data(
  data.frame(ridageyr = 55,
             lbxtc = 213,
             lbdhdd = 50,
             sys_bp = 120,
             hypertension_treatment = FALSE,
             smokes= FALSE,
             diabetes = FALSE)
)

# White women (should be ~2.1%)
predict_10_yr_cvd_risk(coef = white_women_coef,
                       data = test_observation,
                       mean_val = white_women_mean_val,
                       baseline_survival = white_women_baseline_survival)

# Black women (should be ~3.0%)
predict_10_yr_cvd_risk(coef = black_women_coef,
                       data = test_observation,
                       mean_val = black_women_mean_val,
                       baseline_survival = black_women_baseline_survival)

# White men (should be ~5.3%)
predict_10_yr_cvd_risk(coef = white_men_coef,
                       data = test_observation,
                       mean_val = white_men_mean_val,
                       baseline_survival = white_men_baseline_survival)

# Black men (should be ~6.1%)
predict_10_yr_cvd_risk(coef = black_men_coef,
                       data = test_observation,
                       mean_val = black_men_mean_val,
                       baseline_survival = black_men_baseline_survival)


# ======================================================================
# Making and saving predictions
# ======================================================================

white_women_data <- model_data %>%
  filter(race == "White", gender == "Woman") %>%
  mutate(ascvd_risk = predict_10_yr_cvd_risk(coef = white_women_coef,
                                             data = .,
                                             mean_val = white_women_mean_val,
                                             baseline_survival = white_women_baseline_survival))

black_women_data <- model_data %>%
  filter(race == "Black", gender == "Woman")  %>%
  mutate(ascvd_risk = predict_10_yr_cvd_risk(coef = black_women_coef,
                                             data = .,
                                             mean_val = black_women_mean_val,
                                             baseline_survival = black_women_baseline_survival))

white_men_data <- model_data %>%
  filter(race == "White", gender == "Man")  %>%
  mutate(ascvd_risk = predict_10_yr_cvd_risk(coef = white_men_coef,
                                             data = .,
                                             mean_val = white_men_mean_val,
                                             baseline_survival = white_men_baseline_survival))

black_men_data <- model_data %>%
  filter(race == "Black", gender == "Man")  %>%
  mutate(ascvd_risk = predict_10_yr_cvd_risk(coef = black_men_coef,
                                             data = .,
                                             mean_val = black_men_mean_val,
                                             baseline_survival = black_men_baseline_survival))

all_data_with_ascvd <- bind_rows(
  white_women_data,
  black_women_data,
  white_men_data,
  black_men_data
)

saveRDS(all_data_with_ascvd, file = paste(data_object_write_path, "all_data_with_ascvd.rds", sep = ""))

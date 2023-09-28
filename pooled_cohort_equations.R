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

data <- readRDS(here::here(directory_path, 'data/processed', 'cvd_data.rds'))

# The 2013 Pooled Cohort Equations for ASCVD risk use the following features:
#   ln(Age)
#   ln(Age^2)
#   ln(Total cholesterol)
#   ln(Age) * ln(Total cholesterol)
#   ln(HDL-C)
#   ln(Age) * ln(HDL-C)
#   ln(Treated sys. BP)
#   ln(Age) * ln(Treated sys. BP)
#   ln(Untreated sys. BP)
#   ln(Age) * ln(Untreated sys. BP)
#   Current Smoker
#   ln(Age) * Current Smoker
#   Diabetes

# ln(55) * -29.799 = -119.41

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


white_women_test <- data.frame(ridageyr = 55,
                      lbxtc = 213,
                      lbdhdd = 50,
                      sys_bp = 120,
                      hypertension_treatment = FALSE,
                      smokes= FALSE,
                      diabetes = FALSE) %>%
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



white_women_data <- model_data %>%
  filter(gender == "Woman", race == "White") 

# Known coefficients
white_women_coef <- c(ln_age = -29.799, 
                       ln_age_sq = 4.884, 
                       ln_tchol = 13.540,
                       ln_age_ln_tchol = -3.114,
                       ln_hdl = -13.578,
                       ln_age_ln_hdl = 3.149,
                       ln_treated_sys_bp = 2.019,
                       ln_untreated_sys_bp = 1.957,
                       smokes = 7.574,
                       ln_age_smokes = -1.665,
                       diabetes = 0.661
                       )

predict_10_yr_cvd_risk <- function(coef, data, mean_val, base_rate) {
  # data <- data %>%
  #   mutate(dummy = rbernoulli(n()))
  # formula <- reformulate(
  #   paste(c("-1 + ", paste(names(coef), collapse = " + ")), collapse = " "), 
  #   "dummy")
  # dummy_model <- glm(formula, data = data, family = "binomial")
  # dummy_model$coefficients <- coef
  # pred <- predict(dummy_model, newdata = data)
  # print(pred)
  covariates <- names(coef)
  data <- data %>% 
    select(any_of(covariates))
  formula <- reformulate(paste(c("-1 + ", paste(names(coef), collapse = " + ")), collapse = " "))
  model_data <- model.matrix(formula, data)[,]
  dot_product <- c(model_data %*% coef)
  return(dot_product)
  # print(length(coef))
  # print(dim(model_data))
  # # print(model_data)[1]
  # # print(model_data[1,])
  # print(dim(coef))
  # # print(coef %*% t(model_data))
  # predictions <- c(coef %*% t(model_data))
  # return(predictions)
}

# reformulate(paste(c("-1 + ", paste(names(white_women_coef), collapse = " + ")), collapse = " "), "dummy")

# dummy_model <- glm(rbernoulli(nrow(model_data)) ~ -1 + ridageyr, data = model_data, family = "binomial")
# dummy_model$coef <- c(ridageyr = 1)
# dummy_model$coef

m <- predict_10_yr_cvd_risk(coef = white_women_coef,
                       data = white_women_test)


m
t(m)
dot_prod <- c(m %*% white_women_coef)[1]
white_women_mean_val <- -29.18
white_women_baseline_survival <- 0.9665

1 - white_women_baseline_survival^exp(dot_prod - white_women_mean_val)

test <- ~ -1 + ln_age + ln_age_sq + ln_tchol
mini_mod <- model.matrix(test, white_women_data)[,]
a <- white_women_mini %*% t(mini_mod) 
data.frame(a) %>% View()



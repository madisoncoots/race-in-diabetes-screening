# Author: Madison Coots
# Date: October 25, 2023
# ----------------------

library(tidyverse)
library(readr)
library(janitor)
library(stats)
library(PooledCohort)
# https://github.com/bcjaeger/PooledCohort

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
      sex = if_else(gender == "Woman", "female", "male"),
      race = tolower(race),
      age_years = ridageyr,
      chol_total_mgdl = lbxtc,
      chol_hdl_mgdl = lbdhdd,
      bp_sys_mmhg = sys_bp,
      bp_meds = if_else(hypertension_treatment, "yes", "no"),
      smoke_current = if_else(smokes, "yes", "no"),
      diabetes = if_else(diabetes, "yes", "no"),
      stringsAsFactors = FALSE
      ) %>% 
    select(seqn, wtmec8yr, sex, race,
           age_years, chol_total_mgdl, 
           chol_hdl_mgdl, bp_sys_mmhg,
           bp_meds, smoke_current,
           diabetes, stringsAsFactors)
  return(model_data)
}

model_data <- make_model_data(data)

# ======================================================================
# Making race-aware ASCVD predictions (standard)
# ======================================================================

all_data_with_race_aware_ascvd <- model_data %>% 
  mutate(
    race_aware_ascvd_risk = predict_10yr_ascvd_risk(
      sex = sex,
      race = race,
      age_years = age_years,
      chol_total_mgdl = chol_total_mgdl,
      chol_hdl_mgdl = chol_hdl_mgdl,
      bp_sys_mmhg = bp_sys_mmhg,
      bp_meds = bp_meds,
      smoke_current = smoke_current,
      diabetes = diabetes
    )
  )

# ======================================================================
# Making race-blind ASCVD predictions (non-standard)
# ======================================================================

race_breakdown <- model_data %>%
  group_by(race) %>%
  summarize(n = sum(wtmec8yr)) %>%
  ungroup() %>%
  mutate(prop = n / sum(n))

prop_black <- race_breakdown %>% filter(race == "black") %>% pull(prop)
prop_white <- race_breakdown %>% filter(race == "white") %>% pull(prop)

all_data_with_race_blind_ascvd <- model_data %>% 
  mutate(race = "black",
         black_ascvd_risk = 
           predict_10yr_ascvd_risk(
             sex = sex,
             race = race,
             age_years = age_years,
             chol_total_mgdl = chol_total_mgdl,
             chol_hdl_mgdl = chol_hdl_mgdl,
             bp_sys_mmhg = bp_sys_mmhg,
             bp_meds = bp_meds,
             smoke_current = smoke_current,
             diabetes = diabetes
             )
         ) %>%
  mutate(
    race = "white",
    white_ascvd_risk = 
      predict_10yr_ascvd_risk(
        sex = sex,
        race = race,
        age_years = age_years,
        chol_total_mgdl = chol_total_mgdl,
        chol_hdl_mgdl = chol_hdl_mgdl,
        bp_sys_mmhg = bp_sys_mmhg,
        bp_meds = bp_meds,
        smoke_current = smoke_current,
        diabetes = diabetes
        ),
    race_blind_ascvd_risk = (prop_white * white_ascvd_risk +  prop_black * black_ascvd_risk)
  )

# ======================================================================
# Saving data and both predictions
# ======================================================================

all_data_with_both_ascvd <-
  all_data_with_race_aware_ascvd %>%
  left_join(all_data_with_race_blind_ascvd %>%
              select(seqn, race_blind_ascvd_risk),
            by = c("seqn"))

saveRDS(all_data_with_both_ascvd, file = paste(data_object_write_path, "all_data_with_ascvd_bcjaeger_PCE.rds", sep = ""))  




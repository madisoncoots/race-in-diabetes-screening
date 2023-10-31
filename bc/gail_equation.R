# Author: Madison Coots
# Date: October 31, 2023
# ----------------------

library(readr)
library(janitor)
library(BCRA)
library(tidyverse)

directory_path <- dirname(rstudioapi::getActiveDocumentContext()$path)

data <- readRDS(here::here(directory_path, 'data/processed', 'bc_data.rds'))

readRDS("Downloads/all_data_with_ascvd.rds") %>% View()

# ======================================================================
# Predicting race-aware 5-year breast cancer risk (standard)
# ======================================================================

all_data_with_race_aware_risk <- data %>%
  mutate(race_aware_risk = absolute.risk(data))

# ======================================================================
# Predicting race-blind 5-year breast cancer risk (non-standard)
# ======================================================================

race_breakdown <- data %>%
  group_by(Race) %>%
  summarize(n = sum(wtmec8yr)) %>%
  ungroup() %>%
  mutate(prop = n / sum(n))

prop_white <- race_breakdown %>% filter(Race == 1) %>% pull(prop)
prop_black <- race_breakdown %>% filter(Race == 2) %>% pull(prop)
prop_us_hispanic <- race_breakdown %>% filter(Race == 3) %>% pull(prop)
prop_non_us_hispanic <- race_breakdown %>% filter(Race == 5) %>% pull(prop)
prop_asian <- race_breakdown %>% filter(Race == 11) %>% pull(prop)

all_data_with_race_blind_risk <- data %>%
  mutate(white_risk = absolute.risk(data %>% mutate(Race = 1)),
         black_risk = absolute.risk(data %>% mutate(Race = 2)),
         us_hispanic_risk = absolute.risk(data %>% mutate(Race = 3)),
         non_us_hispanic_risk = absolute.risk(data %>% mutate(Race = 5)),
         asian_risk = absolute.risk(data %>% mutate(Race = 11)),
         race_blind_risk = (
           prop_white * white_risk +
           prop_black * black_risk +
           prop_us_hispanic * us_hispanic_risk +
           prop_non_us_hispanic * non_us_hispanic_risk +
           prop_asian * asian_risk
         )) %>%
  select(-white_risk, -black_risk, -us_hispanic_risk, -non_us_hispanic_risk, -asian_risk)

# ======================================================================
# Saving data and both predictions
# ======================================================================

all_data_with_both_risk <-
  all_data_with_race_aware_risk %>%
  left_join(all_data_with_race_blind_risk %>%
              select(ID, race_blind_risk), by = c("ID")) %>%
  mutate(race_aware_risk = race_aware_risk / 100,
         race_blind_risk = race_blind_risk / 100)

saveRDS(all_data_with_both_risk, file = paste(data_object_write_path, "all_data_with_bc_risk.rds", sep = ""))

saveRDS(all_data_with_both_ascvd, file = paste(data_object_write_path, "all_data_with_ascvd_2013_PCE.rds", sep = ""))  
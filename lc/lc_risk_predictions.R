library(tidyverse)
library(readr)
library(lcmodels)

read_path <- "Documents/harvard/research/lung_cancer/data/processed/lc_data.rds"
data_object_write_path <- "Documents/harvard/research/lung_cancer/data/processed/"

data <- readRDS(read_path)

lcrisks_data <- data %>%
  select(age,
         female,
         years_smoked,
         years_quit,
         n_cig_per_day,
         race_code,
         lung_disease,
         num_relatives_with_lc,
         bmi,
         highest_educ_level,
         personal_cancer_history,
         hypertension,
         chd,
         angina,
         heart_attack,
         other_heart_disease,
         stroke,
         diab,
         bron,
         kidney,
         liver,
         spec_eq,
         year)

# ======================================================================
# Predicting race-aware lung cancer risk (standard)
# ======================================================================

race_aware_risk <- lcmodels(data %>% select(-pid, -race_str))

race_aware_lcrat <- race_aware_risk$`Risk of lung cancer diagnosis within 5 years (constrained version of LCRAT)`

all_data_with_race_aware_risk <- data %>%
  mutate(race_aware_lcrat = race_aware_lcrat)

# -------------------------------------------

race_aware_risk <- lcrisk(lcrisks_data, nyears=5)
race_aware_lcrat <- race_aware_risk$`Number with lung cancer diagnosed per 1000 (LCRAT)` / 1000 

all_data_with_race_aware_risk <- data %>%
  mutate(race_aware_risk = race_aware_lcrat)

# ======================================================================
# Predicting race-blind 5-year lung cancer risk (non-standard)
# ======================================================================

race_breakdown <- data %>%
  group_by(race_str) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(prop = n / sum(n))

prop_white <- race_breakdown %>% filter(race_str == "White") %>% pull(prop)
prop_black <- race_breakdown %>% filter(race_str == "Black") %>% pull(prop)
prop_hispanic <- race_breakdown %>% filter(race_str == "Hispanic") %>% pull(prop)
prop_asian <- race_breakdown %>% filter(race_str == "Asian/PI") %>% pull(prop)

white_risk <- lcmodels(data %>% select(-pid, -race_str) %>% mutate(race_code = 0))
white_risk <- white_risk$`Risk of lung cancer diagnosis within 5 years (constrained version of LCRAT)`
black_risk <- lcmodels(data %>% select(-pid, -race_str) %>% mutate(race_code = 1))
black_risk <- black_risk$`Risk of lung cancer diagnosis within 5 years (constrained version of LCRAT)`
hispanic_risk <- lcmodels(data %>% select(-pid, -race_str) %>% mutate(race_code = 2))
hispanic_risk <- hispanic_risk$`Risk of lung cancer diagnosis within 5 years (constrained version of LCRAT)`
asian_risk <- lcmodels(data %>% select(-pid, -race_str) %>% mutate(race_code = 3))
asian_risk <- asian_risk$`Risk of lung cancer diagnosis within 5 years (constrained version of LCRAT)`

all_data_with_race_blind_risk <- data %>%
  mutate(white_risk = white_risk,
         black_risk = black_risk,
         hispanic_risk = hispanic_risk,
         asian_risk = asian_risk,
         race_blind_risk = (
           prop_white * white_risk +
             prop_black * black_risk +
             prop_hispanic * hispanic_risk +
             prop_asian * asian_risk
         )) %>%
  select(-white_risk, -black_risk, -hispanic_risk, -asian_risk)

# -------------------------------------------

race_breakdown <- data %>%
  group_by(race_str) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(prop = n / sum(n))

prop_white <- race_breakdown %>% filter(race_str == "White") %>% pull(prop)
prop_black <- race_breakdown %>% filter(race_str == "Black") %>% pull(prop)
prop_hispanic <- race_breakdown %>% filter(race_str == "Hispanic") %>% pull(prop)
prop_asian <- race_breakdown %>% filter(race_str == "Asian/PI") %>% pull(prop)

white_risk <- lcrisk(lcrisks_data %>% mutate(race_code = 0), nyears = 5)
white_risk <- white_risk$`Number with lung cancer diagnosed per 1000 (LCRAT)` / 1000 
black_risk <- lcrisk(lcrisks_data %>% mutate(race_code = 1), nyears = 5)
black_risk <- black_risk$`Number with lung cancer diagnosed per 1000 (LCRAT)` / 1000 
hispanic_risk <- lcrisk(lcrisks_data %>% mutate(race_code = 2), nyears = 5)
hispanic_risk <- hispanic_risk$`Number with lung cancer diagnosed per 1000 (LCRAT)` / 1000 
asian_risk <- lcrisk(lcrisks_data %>% mutate(race_code = 3), nyears = 5)
asian_risk <- asian_risk$`Number with lung cancer diagnosed per 1000 (LCRAT)` / 1000 

all_data_with_race_blind_risk <- data %>%
  mutate(white_risk = white_risk,
         black_risk = black_risk,
         hispanic_risk = hispanic_risk,
         asian_risk = asian_risk,
         race_blind_risk = (
           prop_white * white_risk +
             prop_black * black_risk +
             prop_hispanic * hispanic_risk +
             prop_asian * asian_risk
         )) %>%
  select(-white_risk, -black_risk, -hispanic_risk, -asian_risk)

# ======================================================================
# Saving data and both predictions
# ======================================================================

all_data_with_both_risk <-
  all_data_with_race_aware_risk %>%
  left_join(all_data_with_race_blind_risk %>%
              select(pid, race_blind_risk), by = c("pid"))

saveRDS(all_data_with_both_risk, file = paste(data_object_write_path, "all_data_with_lc_risk.rds", sep = ""))





























age <- c(66,58,75,72,56)
bmi <- c(23,28,26,27,24)
cpd <- c(36,36,40,24,40)
emp <- c(0,1,1,0,1)
fam.lung.trend <- c(0,2,0,2,0)
female <- c(0,1,0,1,0)
smkyears <- c(43,37,45,42,29)
qtyears <- c(0,0,9,6,6)
race <- c(0,1,1,1,1)
edu6 <- c(3,5,4,5,5)
asb <- c(0,0,0,0,0)
pneu <- c(0,0,0,0,0)
prior.cancer <- c(0,0,0,0,0)
fam.cancer.onset <- c(0,1,0,2,0)
dust <- c(0,0,0,0,0)
fam.cancer <- c(0,1,0,1,0)
fam.smoke.cancer <- c(0,1,0,1,0)
no.hayfever <- c(1,1,1,1,1)
asian <- c(1,1,1,1,1)
islander <- c(0,0,0,0,0)
indian <- c(0,0,0,0,0)
hypertension <- c(0,0,1,0,1)
chd <- c(0,0,0,0,0)
angina <- c(0,0,0,0,0)
heartattack <- c(0,0,0,0,1)
heartdisease <- c(0,0,0,0,0)
stroke <- c(0,0,0,0,0)
diab <- c(1,0,0,0,0)
bron <- c(0,1,0,0,1)
kidney <- c(0,0,0,0,0)
liver <- c(0,0,0,0,0)
speceq <- c(0,1,0,0,0)
year <- rep(2019,5)
persons <- data.frame(age,
                      female,
                      smkyears,
                      qtyears,
                      cpd,
                      race,
                      emp,
                      fam.lung.trend,
                      bmi,
                      edu6,
                      asb,
                      pneu,
                      prior.cancer,
                      fam.cancer.onset,
                      dust,
                      fam.cancer,
                      fam.smoke.cancer,
                      no.hayfever,
                      asian,
                      islander,
                      indian,
                      hypertension,
                      chd,
                      angina,
                      heartattack,
                      heartdisease,
                      stroke,
                      diab,
                      bron,
                      kidney,
                      liver,
                      speceq,
                      year)
persons_predictions <- lcmodels(persons)
persons_predictions$`Risk of lung cancer diagnosis within 5 years (constrained version of LCRAT)`


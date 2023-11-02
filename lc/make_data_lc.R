library(tidyverse)
library(janitor)
library(lcmodels)

read_path <- "Documents/harvard/research/lung_cancer/data/raw/participant.data.d040722.csv.zip"
data_object_write_path <- "Documents/harvard/research/lung_cancer/data/processed/"

raw_data <- read_csv(read_path) %>%
  clean_names()

data <- raw_data %>%
  mutate(weight_kg = weight * 0.453592,
         height_m = height * 0.0254,
         bmi = weight_kg / (height_m)^2,
         female = if_else(gender == 1, 0, 1),
         years_quit = if_else(cigsmok == 1, 0, age - age_quit),
         asian_ethnicity = as.integer(race == 3),
         islander_ethnicity = as.integer(race == 5),
         american_indian_ethnicity = as.integer(race == 4),
         hispanic = (ethnic == 1),
         race_str = case_when(
           (!hispanic & race == 1) ~ "White", # Non-Hispanic-White
           (!hispanic & race == 2) ~ "Black", # Non-Hispanic-Black
           (hispanic) ~ "Hispanic", # Hispanic
           (!hispanic & (race == 3 | race == 5)) ~ "Asian/PI", # Asian or PI
           (!hispanic & !(race %in% c(1, 2, 3, 5))) ~ "Other"), # Other
         race_code = case_when(
           (race_str == "White") ~ 0, # Non-Hispanic-White
           (race_str == "Black") ~ 1, # Non-Hispanic-Black
           (race_str == "Hispanic") ~ 2, # Hispanic
           (race_str == "Asian/PI") | (race_str == "Other") ~ 3), # Other
         lung_disease = if_else((diagcopd == 1 | diagemph  == 1), 1, 0),
         famfather = ifelse(is.na(famfather), 0, famfather),
         fammother = ifelse(is.na(fammother), 0, fammother),
         num_relatives_with_lc = famfather + fammother,
         highest_educ_level = case_when(
           (educat == 1 | educat == 2) ~ 1, # less than 12th grade
           (educat == 3) ~ 2, # HS grad
           (educat == 4) ~ 3, # post HS, no college
           (educat == 5) ~ 4, # Associates, some college
           (educat == 6) ~ 5, # Bachelors
           (educat == 7) ~ 6, # Grad school
           (educat == 8 | educat == 95 | educat == 98 | educat == 99 ) ~ NA # Unknown / missing
         ),
         asbestos_exp = NA, # only have work exposure for asbestos
         dust_exp = NA, # don't have -- unclear what qualifies as dust
         personal_cancer_history = as.integer(
           (cancblad + cancbrea + canccerv + canccolo +
              cancesop + canckidn + canclary + canclung +
              cancnasa + cancoral + cancpanc + cancphar +
              cancstom + cancthyr + canctran) > 0
           ),
         fam_history_lc = NA, # onset information we do not have
         more_than_1_relative_with_any_cancer = NA, # don't have non-lung cancer info for family
         at_least_1_relative_with_smoking_cancer = NA, # don't have non-lung cancer info for family
         hay_fever = NA, # don't have
         other_heart_disease = NA, # don't have
         kidney = NA, # don't have
         liver = NA, # don't have
         spec_eq = NA, # don't have
         angina = NA, # don't have
         year = 2002 # taken from https://pubmed.ncbi.nlm.nih.gov/21714641/
         ) %>% 
  select(
    pid,
    age,
    female,
    years_smoked = smokeyr,
    years_quit,
    n_cig_per_day = smokeday,
    race_code,
    race_str,
    lung_disease,
    num_relatives_with_lc,
    bmi,
    highest_educ_level,
    asbestos_exp,
    diagpneu,
    personal_cancer_history,
    fam_history_lc,
    dust_exp,
    more_than_1_relative_with_any_cancer,
    at_least_1_relative_with_smoking_cancer,
    hay_fever,
    asian_ethnicity,
    islander_ethnicity,
    american_indian_ethnicity,
    hypertension = diaghype,
    chd = diaghear,
    angina,
    heart_attack = diaghear,
    other_heart_disease,
    stroke =diagstro,
    diab = diagdiab,
    bron = diagbron,
    kidney,
    liver,
    spec_eq,
    year) %>%
  filter(years_quit > 0, # bad data
         race_str %in% c("White", "Black", "Hispanic", "Asian/PI"))

saveRDS(data, file = paste(data_object_write_path, "lc_data.rds", sep = ""))


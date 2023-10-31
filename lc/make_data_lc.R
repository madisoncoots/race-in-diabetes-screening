library(tidyverse)
library(janitor)

DATA_PATH <- "Documents/harvard/research/lung_cancer/data/raw/participant.data.d040722.csv.zip"
WRITE_PATH <- "Documents/harvard/research/lung_cancer/data/processed/"

raw_data <- read_csv(DATA_PATH) %>%
  clean_names()

raw_data %>%
  select(
    pid,
    age,
    educat,
    ethnic,
    gender,
    height,
    race,
    weight,
    age_quit,
    smokeday,
    smokeyr,
    diagchro,
    diagcopd,
    diagdiab,
    diagemph,
    diaghear,
    diaghype,
    diagstro,
    canclung,
    famfather,
    fammother
  ) %>%
  mutate(gender = if_else(gender == 1, 0, 1),
         race = case_when(
           (ethnic == 2 & race == 1) ~ 0, # Non-Hispanic-White
           (ethnic == 2 & race == 2) ~ 1, # Non-Hispanic-Black
           (ethnic == 1) ~ 2, # Hispanic
           (ethnic == 2) ~ 3), # Other ethnicity
         lung_disease = if_else((diagcopd == 1 | diagemph  == 1), 1, 0),
         famfather = ifelse(is.na(famfather), 0, famfather),
         fammother = ifelse(is.na(fammother), 0, fammother)
         ) %>%
  View()


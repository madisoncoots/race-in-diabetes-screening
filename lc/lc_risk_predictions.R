library(tidyverse)
library(janitor)

DATA_PATH <- "Documents/harvard/research/lung_cancer/data/participant.data.d040722.csv.zip"
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
  )

# Author: Madison Coots
# Date: September 13, 2023
# ----------------------
# This file pulls all of the requisite NHANES data tables (from 2011-2018)
# and performs a join for the analysis. The following tables are downloaded:
# 1. Demographics (DEMO)
# 2. Diabetes questionnaire responses (DIQ)
# 3. Blood pressure and cholesterol (BPQ)
# 4. Body measurements (BMX)
# 5. Glycohemoglobin measurements (GHB)
# 6. Health insurance status (HIQ)
# 7. Food insecurity data (FSQ)
# 8. Physical activity data (PAQ)
# 9. Medical conditions data (MCQ)
# 10. Mental health data (DPQ)
# 11. Smoking history (SMQ)
# 12. Total cholesterol (TCHOL)
# 13. High-density lipoprotein (HDL)
# 14. Low-density lipoprotein (TRIGLY)
# 15. Blood pressure (BPX)
# 16. Aspirin therapy (RXQASA)

#
# After joining the tables across years, this script implements the same
# filtering on age, BMI, and pregnancy status described in Aggarwal et al. (2022).
# We also replicate this paper's method of constructing the diabetes labels.
#
# The full data object is written to the path specified in data_object_write_path

# The second part of this script constructs a synthetic datasset of individuals
# not receiving treatment for cardviovascular conditions.
# 
# This data object is written to the path specifid in data_object_write_path

library(haven)
library(tidyverse)
library(janitor)

data_object_write_path <- here::here(dirname(rstudioapi::getActiveDocumentContext()$path), "data/processed/")
table_write_path <- here::here(dirname(rstudioapi::getActiveDocumentContext()$path), "data/raw/")

# 2011-2012
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/DEMO_G.XPT", demo <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/DIQ_G.XPT", diq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/BPQ_G.XPT", bpq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/BMX_G.XPT", bmx <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/GHB_G.XPT", ghb <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/HIQ_G.XPT", hiq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/FSQ_G.XPT", fsq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/PAQ_G.XPT", paq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/MCQ_G.XPT", mcq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/DPQ_G.XPT", dpq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/SMQ_G.XPT", smq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/TCHOL_G.XPT", tchol <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/HDL_G.XPT", hdl <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/TRIGLY_G.XPT", trigly <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/BPX_G.XPT", bpx <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/RXQASA_G.XPT", rxqasa <- tempfile(), mode="wb")

raw_demographics_11_12 <- foreign::read.xport(demo) %>% 
  clean_names()
raw_survey_responses_11_12 <- foreign::read.xport(diq) %>% 
  clean_names()
raw_bp_and_chol_11_12 <- foreign::read.xport(bpq) %>% 
  clean_names()
raw_body_measurements_11_12 <- foreign::read.xport(bmx) %>% 
  clean_names()
raw_glycohemoglobin_11_12 <- foreign::read.xport(ghb) %>% 
  clean_names()
raw_health_ins_11_12 <- foreign::read.xport(hiq) %>% 
  clean_names()
raw_food_insec_11_12 <- foreign::read.xport(fsq) %>% 
  clean_names()
raw_physical_act_11_12 <- foreign::read.xport(paq) %>% 
  clean_names()
raw_medical_cond_11_12 <- foreign::read.xport(mcq) %>% 
  clean_names()
raw_mental_health_11_12 <- foreign::read.xport(dpq) %>% 
  clean_names()
raw_smoking_11_12 <- foreign::read.xport(smq) %>% 
  clean_names()
raw_tchol_11_12 <- foreign::read.xport(tchol) %>% 
  clean_names()
raw_hdl_11_12 <- foreign::read.xport(hdl) %>% 
  clean_names()
raw_ldl_11_12 <- foreign::read.xport(trigly) %>% 
  clean_names()
raw_bpx_11_12 <- foreign::read.xport(bpx) %>% 
  clean_names()
raw_aspirin_11_12 <- foreign::read.xport(rxqasa) %>% 
  clean_names()



# 2013-2014
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/DEMO_H.XPT", demo <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/DIQ_H.XPT", diq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/BPQ_H.XPT", bpq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/BMX_H.XPT", bmx <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/GHB_H.XPT", ghb <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/HIQ_H.XPT", hiq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/FSQ_H.XPT", fsq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/PAQ_H.XPT", paq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/MCQ_H.XPT", mcq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/DPQ_H.XPT", dpq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/SMQ_H.XPT", smq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/TCHOL_H.XPT", tchol <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/HDL_H.XPT", hdl <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/TRIGLY_H.XPT", trigly <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/BPX_H.XPT", bpx <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/BPX_H.XPT", bpx <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/RXQASA_H.XPT", rxqasa <- tempfile(), mode="wb")

raw_demographics_13_14 <- foreign::read.xport(demo) %>% 
  clean_names()
raw_survey_responses_13_14 <- foreign::read.xport(diq) %>% 
  clean_names()
raw_bp_and_chol_13_14 <- foreign::read.xport(bpq) %>% 
  clean_names()
raw_body_measurements_13_14 <- foreign::read.xport(bmx) %>% 
  clean_names()
raw_glycohemoglobin_13_14 <- foreign::read.xport(ghb) %>% 
  clean_names()
raw_health_ins_13_14 <- foreign::read.xport(hiq) %>% 
  clean_names()
raw_food_insec_13_14 <- foreign::read.xport(fsq) %>% 
  clean_names()
raw_physical_act_13_14 <- foreign::read.xport(paq) %>% 
  clean_names()
raw_medical_cond_13_14 <- foreign::read.xport(mcq) %>% 
  clean_names()
raw_mental_health_13_14 <- foreign::read.xport(dpq) %>% 
  clean_names()
raw_smoking_13_14 <- foreign::read.xport(smq) %>% 
  clean_names()
raw_tchol_13_14 <- foreign::read.xport(tchol) %>% 
  clean_names()
raw_hdl_13_14 <- foreign::read.xport(hdl) %>% 
  clean_names()
raw_ldl_13_14 <- foreign::read.xport(trigly) %>% 
  clean_names()
raw_bpx_13_14 <- foreign::read.xport(bpx) %>% 
  clean_names()
raw_aspirin_13_14 <- foreign::read.xport(rxqasa) %>% 
  clean_names()


# 2015-2016
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/DEMO_I.XPT", demo <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/DIQ_I.XPT", diq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/BPQ_I.XPT", bpq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/BMX_I.XPT", bmx <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/GHB_I.XPT", ghb <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/HIQ_I.XPT", hiq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/FSQ_I.XPT", fsq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/PAQ_I.XPT", paq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/MCQ_I.XPT", mcq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/DPQ_I.XPT", dpq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/SMQ_I.XPT", smq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/TCHOL_I.XPT", tchol <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/HDL_I.XPT", hdl <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/TRIGLY_I.XPT", trigly <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/BPX_I.XPT", bpx <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/RXQASA_I.XPT", rxqasa <- tempfile(), mode="wb")

raw_demographics_15_16 <- foreign::read.xport(demo) %>% 
  clean_names()
raw_survey_responses_15_16 <- foreign::read.xport(diq) %>% 
  clean_names()
raw_bp_and_chol_15_16 <- foreign::read.xport(bpq) %>% 
  clean_names()
raw_body_measurements_15_16 <- foreign::read.xport(bmx) %>% 
  clean_names()
raw_glycohemoglobin_15_16 <- foreign::read.xport(ghb) %>% 
  clean_names()
raw_health_ins_15_16 <- foreign::read.xport(hiq) %>% 
  clean_names()
raw_food_insec_15_16 <- foreign::read.xport(fsq) %>% 
  clean_names()
raw_physical_act_15_16 <- foreign::read.xport(paq) %>% 
  clean_names()
raw_medical_cond_15_16 <- foreign::read.xport(mcq) %>% 
  clean_names()
raw_mental_health_15_16 <- foreign::read.xport(dpq) %>% 
  clean_names()
raw_smoking_15_16 <- foreign::read.xport(smq) %>% 
  clean_names()
raw_tchol_15_16 <- foreign::read.xport(tchol) %>% 
  clean_names()
raw_hdl_15_16 <- foreign::read.xport(hdl) %>% 
  clean_names()
raw_ldl_15_16 <- foreign::read.xport(trigly) %>% 
  clean_names()
raw_bpx_15_16 <- foreign::read.xport(bpx) %>% 
  clean_names()
raw_aspirin_15_16 <- foreign::read.xport(rxqasa) %>% 
  clean_names()


# 2017-2018
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/DEMO_J.XPT", demo <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/DIQ_J.XPT", diq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/BPQ_J.XPT", bpq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/BMX_J.XPT", bmx <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/GHB_J.XPT", ghb <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/HIQ_J.XPT", hiq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/FSQ_J.XPT", fsq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/PAQ_J.XPT", paq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/MCQ_J.XPT", mcq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/DPQ_J.XPT", dpq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/SMQ_J.XPT", smq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/TCHOL_J.XPT", tchol <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/HDL_J.XPT", hdl <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/TRIGLY_J.XPT", trigly <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/BPX_J.XPT", bpx <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/RXQASA_J.XPT", rxqasa <- tempfile(), mode="wb")

raw_demographics_17_18 <- foreign::read.xport(demo) %>% 
  clean_names()
raw_survey_responses_17_18 <- foreign::read.xport(diq) %>% 
  clean_names()
raw_bp_and_chol_17_18 <- foreign::read.xport(bpq) %>% 
  clean_names()
raw_body_measurements_17_18 <- foreign::read.xport(bmx) %>% 
  clean_names()
raw_glycohemoglobin_17_18 <- foreign::read.xport(ghb) %>% 
  clean_names()
raw_health_ins_17_18 <- foreign::read.xport(hiq) %>% 
  clean_names()
raw_food_insec_17_18 <- foreign::read.xport(fsq) %>% 
  clean_names()
raw_physical_act_17_18 <- foreign::read.xport(paq) %>% 
  clean_names()
raw_medical_cond_17_18 <- foreign::read.xport(mcq) %>% 
  clean_names()
raw_mental_health_17_18 <- foreign::read.xport(dpq) %>% 
  clean_names()
raw_smoking_17_18 <- foreign::read.xport(smq) %>% 
  clean_names()
raw_tchol_17_18 <- foreign::read.xport(tchol) %>% 
  clean_names()
raw_hdl_17_18 <- foreign::read.xport(hdl) %>% 
  clean_names()
raw_ldl_17_18 <- foreign::read.xport(trigly) %>% 
  clean_names()
raw_bpx_17_18 <- foreign::read.xport(bpx) %>% 
  clean_names()
raw_aspirin_17_18 <- foreign::read.xport(rxqasa) %>% 
  clean_names()


# ------------------- Combine -------------------

# Demographics data
raw_demographics_all <- bind_rows(
  raw_demographics_11_12,
  raw_demographics_13_14,
  raw_demographics_15_16,
  raw_demographics_17_18
)

# Diabetes survey data
raw_survey_responses_all <- bind_rows(
  raw_survey_responses_11_12,
  raw_survey_responses_13_14,
  raw_survey_responses_15_16,
  raw_survey_responses_17_18
)

# Blood pressure and cholesterol data
raw_bp_and_chol_all <- bind_rows(
  raw_bp_and_chol_11_12,
  raw_bp_and_chol_13_14,
  raw_bp_and_chol_15_16,
  raw_bp_and_chol_17_18
)

# Body measurements data
raw_body_measurements_all <- bind_rows(
  raw_body_measurements_11_12,
  raw_body_measurements_13_14,
  raw_body_measurements_15_16,
  raw_body_measurements_17_18
)

# Glycohemoglobin data
raw_glycohemoglobin_all <- bind_rows(
  raw_glycohemoglobin_11_12,
  raw_glycohemoglobin_13_14,
  raw_glycohemoglobin_15_16,
  raw_glycohemoglobin_17_18
)

# Health insurance data
raw_health_ins_all <- bind_rows(
  raw_health_ins_11_12,
  raw_health_ins_13_14,
  raw_health_ins_15_16,
  raw_health_ins_17_18
)

# Food insecurity data
raw_food_insec_all <- bind_rows(
  raw_food_insec_11_12,
  raw_food_insec_13_14,
  raw_food_insec_15_16,
  raw_food_insec_17_18
)

# Physical activity data
raw_physical_act_all <- bind_rows(
  raw_physical_act_11_12,
  raw_physical_act_13_14,
  raw_physical_act_15_16,
  raw_physical_act_17_18
)

# Medical conditions data
raw_medical_cond_all <- bind_rows(
  raw_medical_cond_11_12,
  raw_medical_cond_13_14,
  raw_medical_cond_15_16,
  raw_medical_cond_17_18
)

# Mental health data
raw_mental_health_all <- bind_rows(
  raw_mental_health_11_12,
  raw_mental_health_13_14,
  raw_mental_health_15_16,
  raw_mental_health_17_18
)

# Smoking history data
raw_smoking_all <- bind_rows(
  raw_smoking_11_12,
  raw_smoking_13_14,
  raw_smoking_15_16,
  raw_smoking_17_18
)

# Total cholesterol data
raw_tchol_all <- bind_rows(
  raw_tchol_11_12,
  raw_tchol_13_14,
  raw_tchol_15_16,
  raw_tchol_17_18
)

# HDL data
raw_hdl_all <- bind_rows(
  raw_hdl_11_12,
  raw_hdl_13_14,
  raw_hdl_15_16,
  raw_hdl_17_18
)

# LDL data
raw_ldl_all <- bind_rows(
  raw_ldl_11_12,
  raw_ldl_13_14,
  raw_ldl_15_16,
  raw_ldl_17_18
)

# Blood pressure data
raw_bpx_all <- bind_rows(
  raw_bpx_11_12,
  raw_bpx_13_14,
  raw_bpx_15_16,
  raw_bpx_17_18
)

# Aspirin data
raw_aspirin_all <- bind_rows(
  raw_aspirin_11_12,
  raw_aspirin_13_14,
  raw_aspirin_15_16,
  raw_aspirin_17_18
)

write_csv(raw_demographics_all, file = paste(table_write_path, "raw_demographics_all.csv", sep = ""))
write_csv(raw_survey_responses_all, file = paste(table_write_path, "raw_survey_responses_all.csv", sep = ""))
write_csv(raw_bp_and_chol_all, file = paste(table_write_path, "raw_bp_and_chol_all.csv", sep = ""))
write_csv(raw_body_measurements_all, file = paste(table_write_path, "raw_body_measurements_all.csv", sep = ""))
write_csv(raw_glycohemoglobin_all, file = paste(table_write_path, "raw_glycohemoglobin_all.csv", sep = ""))
write_csv(raw_health_ins_all, file = paste(table_write_path, "raw_health_ins_all.csv", sep = ""))
write_csv(raw_food_insec_all, file = paste(table_write_path, "raw_food_insec_all.csv", sep = ""))
write_csv(raw_physical_act_all, file = paste(table_write_path, "raw_physical_act_all.csv", sep = ""))
write_csv(raw_medical_cond_all, file = paste(table_write_path, "raw_medical_cond_all.csv", sep = ""))
write_csv(raw_mental_health_all, file = paste(table_write_path, "raw_mental_health_all.csv", sep = ""))
write_csv(raw_smoking_all, file = paste(table_write_path, "raw_smoking_all.csv", sep = ""))
write_csv(raw_tchol_all, file = paste(table_write_path, "raw_tchol_all.csv", sep = ""))
write_csv(raw_hdl_all, file = paste(table_write_path, "raw_hdl_all.csv", sep = ""))
write_csv(raw_ldl_all, file = paste(table_write_path, "raw_ldl_all.csv", sep = ""))
write_csv(raw_bpx_all, file = paste(table_write_path, "raw_bpx_all.csv", sep = ""))
write_csv(raw_aspirin_all, file = paste(table_write_path, "raw_aspirin_all.csv", sep = ""))





data <- raw_demographics_all %>%
  inner_join(raw_survey_responses_all, by = c("seqn")) %>%
  inner_join(raw_bp_and_chol_all, by = c("seqn")) %>%
  inner_join(raw_body_measurements_all, by = c("seqn")) %>%
  inner_join(raw_glycohemoglobin_all, by = c("seqn")) %>%
  inner_join(raw_health_ins_all, by = c("seqn")) %>%
  inner_join(raw_food_insec_all, by = c("seqn")) %>%
  inner_join(raw_physical_act_all, by = c("seqn")) %>%
  inner_join(raw_medical_cond_all, by = c("seqn")) %>%
  inner_join(raw_mental_health_all, by = c("seqn")) %>%
  inner_join(raw_smoking_all, by = c("seqn")) %>%
  inner_join(raw_tchol_all, by = c("seqn")) %>%
  inner_join(raw_hdl_all, by = c("seqn")) %>%
  inner_join(raw_ldl_all, by = c("seqn")) %>%
  inner_join(raw_bpx_all, by = c("seqn")) %>%
  inner_join(raw_aspirin_all, by = c("seqn")) %>%
  filter(ridageyr >= 18) %>% # Taken from Supplement
  filter(ridageyr <= 70) %>%
  filter(ridexprg != 1 | is.na(ridexprg)) %>%
  filter(bmxbmi >= 18.5, # Taken from Supplement
         bmxbmi <= 50) %>%
  # Making the race variable more readable
  mutate(
    race = case_when(ridreth3 == 1 | ridreth3 == 2 ~ "Hispanic",
                     ridreth3 == 3 ~ "White",
                     ridreth3 == 4 ~ "Black",
                     ridreth3 == 6 ~ "Asian"),
    race = factor(race),
    # Re-leveling the race factor, so that White is base level (as in paper)
    race = relevel(race, ref = "White")) %>%
  # Recoding the gender covariate
  mutate(gender = case_when(riagendr == 2 ~ "Woman",
                            riagendr == 1 ~ "Man",
                            riagendr == "." ~ as.character(NA))) %>%
  # Recoding the income codes to interpretable values
  mutate(income = case_when(indhhin2 == 1 ~ "$0 to $4,999",
                            indhhin2 == 2 ~ "$5,000 to $9,999",
                            indhhin2 == 3 ~ "$10,000 to $14,999",
                            indhhin2 == 4 ~ "$15,000 to $19,999",
                            indhhin2 == 5 ~ "$20,000 to $24,999",
                            indhhin2 == 6 ~ "$25,000 to $34,999",
                            indhhin2 == 7 ~ "$35,000 to $44,999",
                            indhhin2 == 8 ~ "$45,000 to $54,999",
                            indhhin2 == 9 ~ "$55,000 to $64,999",
                            indhhin2 == 10 ~ "$65,000 to $74,999",
                            indhhin2 == 12 ~ "$20,000 and Over",
                            indhhin2 == 13 ~ "Under $20,000",
                            indhhin2 == 14 ~ "$75,000 to $99,999",
                            indhhin2 == 15 ~ "$100,000 and Over",
                            indhhin2 %in% c(77,99,".") ~ as.character(NA))
  ) %>%
  # Recoding the health insurance codes to interpretable values
  mutate(health_insurance = case_when(hiq011 == 1 ~ "Yes",
                                      hiq011 == 2 ~ "No",
                                      hiq011 %in% c(7, 9, ".") ~ as.character(NA))
  ) %>%
  # Recoding the food security codes to interpretable values
  mutate(food_security = case_when(fsd032a == 1 ~ "Often worried",
                                   fsd032a == 2 ~ "Sometimes worried",
                                   fsd032a == 3 ~ "Never worried",
                                   fsd032a %in% c(7, 9, ".") ~ as.character(NA))
  ) %>%
  # Recoding the blood relatives had diabetes covariate
  mutate(relatives_had_diabetes = case_when(mcq300c == 1 ~ "Yes",
                                            mcq300c == 2 ~ "No",
                                            mcq300c %in% c(7, 9, ".") ~ as.character(NA))) %>%
  # Recoding the blood relatives had heart attack covariate
  mutate(relatives_had_heart_attack = case_when(mcq300a == 1 ~ "Yes",
                                            mcq300a == 2 ~ "No",
                                            mcq300a %in% c(7, 9, ".") ~ as.character(NA))) %>%
  # Recoding the felt depressed covariate
  mutate(felt_depressed = case_when(dpq020 == 0 ~ "Not at all",
                                    dpq020 == 1 ~ "Several days",
                                    dpq020 == 2 ~ "More than half the days",
                                    dpq020 == 3 ~ "Nearly every day",
                                    dpq020 %in% c(7, 9, ".") ~ as.character(NA))) %>%
  # Adjusting weights for 8 years of data
  mutate(wtmec8yr = wtmec2yr/4) %>%
  # Making diabetes labels as described in the paper and replication code
  mutate(lbxgh = as.numeric(as.character((lbxgh))),
         diq010 = as.numeric(as.character((diq010))),
         a1c = cut(lbxgh,breaks=c(0,5.7,6.5,1000),right=FALSE),
         diabetes_diagnosis = case_when(diq010 %in% 1 ~ 1,
                                        diq010 %in% c(2,3,9) ~ 0,
                                        diq010 %in% 7 ~ as.numeric(NA)),
         diabetes = diabetes_diagnosis,
         diabetes = if_else(a1c=="[6.5,1e+03)" &!is.na(a1c), 1, diabetes),
         diabetes = as.integer(diabetes),
         diabetes = if_else(diabetes == 1, TRUE, FALSE)
  ) %>%
  # Constructing the CVD label
  mutate(congestive_heart_failure = (mcq160b == 1),
         coronary_heart_disease = (mcq160c == 1),
         angina = (mcq160d == 1),
         heart_attack = (mcq160e == 1),
         stroke = (mcq160f == 1),
         cvd = (congestive_heart_failure | coronary_heart_disease | angina | heart_attack | stroke)) %>%
  drop_na(cvd) %>%
  # Extracting smoking label (smokes everyday or sometimes)
  mutate(smokes = (smq040 == 1) | (smq040 == 2)) %>%
  # Normalizing weights for numerical stability in regressions
  mutate(normalized_weights = wtmec8yr / sum(wtmec8yr)) %>%
  # Constructing feature of whether or not respondent is taking statins
  # A response of NA mans that a respondent was never recommended
  # statins, therefore they are not taking them
  mutate(statins = (bpq100d == 1) & (!is.na(bpq100d))) %>%
  # Constructing feature of whether or not respondent is taking hypertension meds
  # A response of NA mans that a respondent was never recommended
  # medication, therefore they are not taking any
  mutate(hypertension_treatment = (bpq050a == 1) & (!is.na(bpq050a))) %>%
  # Constructing feature of whether or not respondent is taking aspirin
  mutate(aspirin_dr = (rxq515 == 1) & (!is.na(rxq515)), # taking dr. recommended aspirin
         aspirin_self = (rxq520 == 1) & (!is.na(rxq520)), # self-directed aspirin
         aspirin = (aspirin_dr | aspirin_self)) %>%
  mutate(hypertension_treatment = (bpq050a == 1) & (!is.na(bpq050a))) %>%
  # Computing the average of three blood pressure readings
  rowwise() %>%
  mutate(sys_bp = mean(c(bpxsy1, bpxsy2, bpxsy3, bpxsy4), na.rm = TRUE)) %>%
  ungroup()

saveRDS(data, file = paste(data_object_write_path, "cvd_data.rds", sep = ""))

# ==============================================================================
# ============================ Synthetic data set ==============================

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

complex_model_formula <- cvd ~ gender + ridageyr + race + lbxtc + lbdldl + 
  lbdhdd + statins + sys_bp + hypertension_treatment + diabetes + smokes + 
  aspirin + relatives_had_diabetes + relatives_had_heart_attack + 
  felt_depressed + income + health_insurance  + food_security 

complex_model <- glm(complex_model_formula,
                     data = data,
                     family = "binomial",
                     weights = round(wtmec8yr/1000)) # glm complains when weights aren't ints

data_no_treatment <- data %>%
  mutate(statins = TRUE,
         hypertension_treatment = TRUE,
         aspirin = TRUE) %>%
  select(gender, ridageyr, race,
         lbxtc, lbdldl, lbdhdd,
         statins, sys_bp, hypertension_treatment,
         diabetes, smokes, aspirin,
         relatives_had_diabetes, relatives_had_heart_attack, felt_depressed,
         income, health_insurance, food_security,
         cvd, wtmec8yr)

cvd_risk_no_treatment <- predict(complex_model, newdata = data_no_treatment, type = "response")
plot(cvd_risk_no_treatment)
summary(cvd_risk_no_treatment)

# ======== This is test code -- delete later ========
# cvd_risk <- predict(complex_model, newdata = data, type = "response")
# plot(cvd_risk)
# summary(cvd_risk)
# 
# test_model_data <- data %>% filter(!statins, !hypertension_treatment, !aspirin)
# test_model_formula <- cvd ~ gender + ridageyr + race + lbxtc + lbdldl + 
#   lbdhdd + sys_bp + diabetes + smokes
# test_model <- glm(test_model_formula,
#                  data = test_model_data,
#                  family = "binomial",
#                  weights = round(wtmec8yr/1000)) # glm complains when weights aren't ints
# test_model_pred <- predict(test_model, newdata = test_model_data, type = "response") 
# plot(test_model_pred)
# summary(test_model_pred)

set.seed(1)
synthetic_data <- data_no_treatment %>%
  mutate(pred_no_treatment = cvd_risk_no_treatment,
         cvd = runif(n()) <= pred_no_treatment) %>%
  select(-pred_no_treatment)

saveRDS(synthetic_data, file = paste(data_object_write_path, "synthetic_cvd_data.rds", sep = ""))

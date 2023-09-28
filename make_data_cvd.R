# Author: Madison Coots
# Date: September 13, 2023
# ----------------------
# This file pulls all of the requisite NHANES data tables (from 2011-2018)
# and performs a join for the analysis. The following tables are downloaded:
# 1. Demographics (DEMO)
# 2. Diabetes questionnaire responses (DIQ)
# 3. Blood pressure and cholesterol (BPQ) 
# 5. Glycohemoglobin measurements (GHB) 
# 9. Medical conditions data (MCQ)
# 11. Smoking history (SMQ)
# 12. Total cholesterol (TCHOL)
# 13. High-density lipoprotein (HDL)
# 15. Blood pressure (BPX)

# After joining the tables across years, this script creates the data 
# necessary to estimate 10-year CVD risk with the Pooled Cohort Equations for
# 10-Year CVD Risk
# 2013 ACC/AHA Guideline on the Assessment of Cardiovascular Risk
# Taken from the supplement of:
# A Report of the American College of Cardiology/American Heart Association 
# Task Force on Practice Guidelines. Goff et al. 2014

# The full data object is written to the path specified in data_object_write_path


library(haven)
library(tidyverse)
library(janitor)

data_object_write_path <- here::here(dirname(rstudioapi::getActiveDocumentContext()$path), "data/processed/")
table_write_path <- here::here(dirname(rstudioapi::getActiveDocumentContext()$path), "data/raw/")

# 2011-2012
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/DEMO_G.XPT", demo <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/DIQ_G.XPT", diq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/BPQ_G.XPT", bpq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/GHB_G.XPT", ghb <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/MCQ_G.XPT", mcq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/SMQ_G.XPT", smq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/TCHOL_G.XPT", tchol <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/HDL_G.XPT", hdl <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2011-2012/BPX_G.XPT", bpx <- tempfile(), mode="wb")

raw_demographics_11_12 <- foreign::read.xport(demo) %>% 
  clean_names()
raw_survey_responses_11_12 <- foreign::read.xport(diq) %>% 
  clean_names()
raw_bp_and_chol_11_12 <- foreign::read.xport(bpq) %>%
  clean_names()
raw_glycohemoglobin_11_12 <- foreign::read.xport(ghb) %>% 
  clean_names()
raw_medical_cond_11_12 <- foreign::read.xport(mcq) %>% 
  clean_names()
raw_smoking_11_12 <- foreign::read.xport(smq) %>% 
  clean_names()
raw_tchol_11_12 <- foreign::read.xport(tchol) %>% 
  clean_names()
raw_hdl_11_12 <- foreign::read.xport(hdl) %>% 
  clean_names()
raw_bpx_11_12 <- foreign::read.xport(bpx) %>% 
  clean_names()



# 2013-2014
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/DEMO_H.XPT", demo <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/DIQ_H.XPT", diq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/BPQ_H.XPT", bpq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/GHB_H.XPT", ghb <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/MCQ_H.XPT", mcq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/SMQ_H.XPT", smq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/TCHOL_H.XPT", tchol <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/HDL_H.XPT", hdl <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2013-2014/BPX_H.XPT", bpx <- tempfile(), mode="wb")

raw_demographics_13_14 <- foreign::read.xport(demo) %>% 
  clean_names()
raw_survey_responses_13_14 <- foreign::read.xport(diq) %>% 
  clean_names()
raw_bp_and_chol_13_14 <- foreign::read.xport(bpq) %>% 
  clean_names()
raw_glycohemoglobin_13_14 <- foreign::read.xport(ghb) %>% 
  clean_names()
raw_medical_cond_13_14 <- foreign::read.xport(mcq) %>% 
  clean_names()
raw_smoking_13_14 <- foreign::read.xport(smq) %>% 
  clean_names()
raw_tchol_13_14 <- foreign::read.xport(tchol) %>% 
  clean_names()
raw_hdl_13_14 <- foreign::read.xport(hdl) %>% 
  clean_names()
raw_bpx_13_14 <- foreign::read.xport(bpx) %>% 
  clean_names()


# 2015-2016
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/DEMO_I.XPT", demo <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/DIQ_I.XPT", diq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/BPQ_I.XPT", bpq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/GHB_I.XPT", ghb <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/MCQ_I.XPT", mcq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/SMQ_I.XPT", smq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/TCHOL_I.XPT", tchol <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/HDL_I.XPT", hdl <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2015-2016/BPX_I.XPT", bpx <- tempfile(), mode="wb")

raw_demographics_15_16 <- foreign::read.xport(demo) %>% 
  clean_names()
raw_survey_responses_15_16 <- foreign::read.xport(diq) %>% 
  clean_names()
raw_bp_and_chol_15_16 <- foreign::read.xport(bpq) %>% 
  clean_names()
raw_glycohemoglobin_15_16 <- foreign::read.xport(ghb) %>% 
  clean_names()
raw_medical_cond_15_16 <- foreign::read.xport(mcq) %>% 
  clean_names()
raw_smoking_15_16 <- foreign::read.xport(smq) %>% 
  clean_names()
raw_tchol_15_16 <- foreign::read.xport(tchol) %>% 
  clean_names()
raw_hdl_15_16 <- foreign::read.xport(hdl) %>% 
  clean_names()
raw_bpx_15_16 <- foreign::read.xport(bpx) %>% 
  clean_names()


# 2017-2018
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/DEMO_J.XPT", demo <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/DIQ_J.XPT", diq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/BPQ_J.XPT", bpq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/GHB_J.XPT", ghb <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/MCQ_J.XPT", mcq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/SMQ_J.XPT", smq <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/TCHOL_J.XPT", tchol <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/HDL_J.XPT", hdl <- tempfile(), mode="wb")
download.file("https://wwwn.cdc.gov/nchs/nhanes/2017-2018/BPX_J.XPT", bpx <- tempfile(), mode="wb")

raw_demographics_17_18 <- foreign::read.xport(demo) %>% 
  clean_names()
raw_survey_responses_17_18 <- foreign::read.xport(diq) %>% 
  clean_names()
raw_bp_and_chol_17_18 <- foreign::read.xport(bpq) %>% 
  clean_names()
raw_glycohemoglobin_17_18 <- foreign::read.xport(ghb) %>% 
  clean_names()
raw_medical_cond_17_18 <- foreign::read.xport(mcq) %>% 
  clean_names()
raw_smoking_17_18 <- foreign::read.xport(smq) %>% 
  clean_names()
raw_tchol_17_18 <- foreign::read.xport(tchol) %>% 
  clean_names()
raw_hdl_17_18 <- foreign::read.xport(hdl) %>% 
  clean_names()
raw_bpx_17_18 <- foreign::read.xport(bpx) %>% 
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


# Glycohemoglobin data
raw_glycohemoglobin_all <- bind_rows(
  raw_glycohemoglobin_11_12,
  raw_glycohemoglobin_13_14,
  raw_glycohemoglobin_15_16,
  raw_glycohemoglobin_17_18
)

# Medical conditions data
raw_medical_cond_all <- bind_rows(
  raw_medical_cond_11_12,
  raw_medical_cond_13_14,
  raw_medical_cond_15_16,
  raw_medical_cond_17_18
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

# Blood pressure data
raw_bpx_all <- bind_rows(
  raw_bpx_11_12,
  raw_bpx_13_14,
  raw_bpx_15_16,
  raw_bpx_17_18
)

write_csv(raw_demographics_all, file = paste(table_write_path, "raw_demographics_all.csv", sep = ""))
write_csv(raw_survey_responses_all, file = paste(table_write_path, "raw_survey_responses_all.csv", sep = ""))
write_csv(raw_bp_and_chol_all, file = paste(table_write_path, "raw_bp_and_chol_all.csv", sep = ""))
write_csv(raw_glycohemoglobin_all, file = paste(table_write_path, "raw_glycohemoglobin_all.csv", sep = ""))
write_csv(raw_medical_cond_all, file = paste(table_write_path, "raw_medical_cond_all.csv", sep = ""))
write_csv(raw_smoking_all, file = paste(table_write_path, "raw_smoking_all.csv", sep = ""))
write_csv(raw_tchol_all, file = paste(table_write_path, "raw_tchol_all.csv", sep = ""))
write_csv(raw_hdl_all, file = paste(table_write_path, "raw_hdl_all.csv", sep = ""))
write_csv(raw_bpx_all, file = paste(table_write_path, "raw_bpx_all.csv", sep = ""))





data <- raw_demographics_all %>%
  inner_join(raw_survey_responses_all, by = c("seqn")) %>%
  inner_join(raw_bp_and_chol_all, by = c("seqn")) %>%
  inner_join(raw_glycohemoglobin_all, by = c("seqn")) %>%
  inner_join(raw_medical_cond_all, by = c("seqn")) %>%
  inner_join(raw_smoking_all, by = c("seqn")) %>%
  inner_join(raw_tchol_all, by = c("seqn")) %>%
  inner_join(raw_hdl_all, by = c("seqn")) %>%
  inner_join(raw_bpx_all, by = c("seqn")) %>%
  filter(ridageyr >= 18) %>%
  filter(ridexprg != 1 | is.na(ridexprg)) %>%
  # Making the race variable more readable
  mutate(
    race = case_when(ridreth3 == 1 | ridreth3 == 2 ~ "Hispanic",
                     ridreth3 == 3 ~ "White",
                     ridreth3 == 4 ~ "Black",
                     ridreth3 == 6 ~ "Asian"),
    race = factor(race),
    # Re-leveling the race factor, so that White is base level (as in paper)
    race = relevel(race, ref = "White")) %>%
  filter(race == "White" | race == "Black") %>% # ASCVD eq. exist only for White and Black individuals
  # Recoding the gender covariate
  mutate(gender = case_when(riagendr == 2 ~ "Woman",
                            riagendr == 1 ~ "Man",
                            riagendr == "." ~ as.character(NA))) %>%
  # Adjusting weights for 8 years of data
  mutate(wtmec8yr = wtmec2yr/4) %>%
  # Making diabetes labels as described in the Aggarwal et al. paper and replication code
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
  # mutate(congestive_heart_failure = (mcq160b == 1),
  #        coronary_heart_disease = (mcq160c == 1),
  #        angina = (mcq160d == 1),
  #        heart_attack = (mcq160e == 1),
  #        stroke = (mcq160f == 1),
  #        cvd = (congestive_heart_failure | coronary_heart_disease | angina | heart_attack | stroke)) %>%
  # drop_na(cvd) %>%
  # Extracting smoking label (smokes everyday or sometimes)
  mutate(smokes = (smq040 == 1) | (smq040 == 2)) %>%
  # Constructing feature of whether or not respondent is taking statins
  # A response of NA mans that a respondent was never recommended
  # statins, therefore they are not taking them
  # mutate(statins = (bpq100d == 1) & (!is.na(bpq100d))) %>%
  # Constructing feature of whether or not respondent is taking hypertension meds
  # A response of NA mans that a respondent was never recommended
  # medication, therefore they are not taking any
  mutate(hypertension_treatment = (bpq050a == 1) & (!is.na(bpq050a))) %>%
  # Computing the average of three blood pressure readings
  rowwise() %>%
  mutate(sys_bp = mean(c(bpxsy1, bpxsy2, bpxsy3, bpxsy4), na.rm = TRUE),
         dias_bp = mean(c(bpxdi1, bpxdi2, bpxdi3, bpxdi4), na.rm = TRUE)) %>%
  ungroup() %>%
  select(seqn, wtmec8yr, gender, ridageyr, race,
         lbxtc, lbdhdd,
         sys_bp, hypertension_treatment,
         diabetes, smokes) %>%
  drop_na()

saveRDS(data, file = paste(data_object_write_path, "cvd_data.rds", sep = ""))

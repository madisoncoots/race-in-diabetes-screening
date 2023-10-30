library(BCRA)
library(tidyverse)

View(absolute.risk)

data("exampledata")
absolute.risk(exampledata %>% select(-T2))

data("BrCa_1_AR")
BrCa_1_AR

data("BrCa_beta")
BrCa_beta

error.table(data, Raw_Ind=1)

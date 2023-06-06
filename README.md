# Reevaluating the Role of Race and Ethnicity in Diabetes Screening

This is the code base for our [Reevaluating the Role of Race and Ethnicity in Diabetes Screening](https://madisoncoots.com/) paper.

To reproduce the results in our paper, run the `all_tables_and_figures.R` script. R data objects and raw NHANES data tables for the analyses are located in the `data/` repository, so it is not necessary to run any data preprocessing scripts to generate the figures.

For completeness, we do also include the data preprocessing script that produces the R data object, *but this script does not need to be run to reproduce the results*. This script is `make_diabetes_data.R`. `make_diabetes_data.R` pulls the NHANES tables directly from the CDC website, but we also provide these tables locally so that no additional download is required.

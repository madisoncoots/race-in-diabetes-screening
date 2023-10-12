# =======================================
# Coef. x Value Group Means (by Race/Sex) 
# =======================================

white_women_mean_val <- -29.18
black_women_mean_val <- 86.61
white_men_mean_val <- 61.18
black_men_mean_val <- 19.54


# =======================================
# Baseline Survival Rates (by Race/Sex) 
# =======================================

white_women_baseline_survival <- 0.9665
black_women_baseline_survival <- 0.9533
white_men_baseline_survival <- 0.9144
black_men_baseline_survival <- 0.8954

# =======================================
# ASCVD Risk Model Coefficients 
# =======================================

white_women_coef <- c(ln_age = -29.799, 
                      ln_age_sq = 4.884, 
                      ln_tchol = 13.540,
                      ln_age_ln_tchol = -3.114,
                      ln_hdl = -13.578,
                      ln_age_ln_hdl = 3.149,
                      ln_treated_sys_bp = 2.019,
                      ln_untreated_sys_bp = 1.957,
                      smokes = 7.574,
                      ln_age_smokes = -1.665,
                      diabetes = 0.661
)

black_women_coef <- c(ln_age = 17.114, 
                      ln_tchol = 0.940,
                      ln_hdl = -18.920,
                      ln_age_ln_hdl = 4.475,
                      ln_treated_sys_bp = 29.291,
                      ln_age_ln_treated_sys_bp = -6.432,
                      ln_untreated_sys_bp = 27.820,
                      ln_age_ln_untreated_sys_bp = -6.087,
                      smokes = 0.691,
                      diabetes = 0.874
)

white_men_coef <- c(ln_age = 12.344, 
                      ln_tchol = 11.853,
                      ln_age_ln_tchol = -2.664,
                      ln_hdl = -7.990,
                      ln_age_ln_hdl = 1.769,
                      ln_treated_sys_bp = 1.797,
                      ln_untreated_sys_bp = 1.764,
                      smokes = 7.837,
                      ln_age_smokes = -1.795,
                      diabetes = 0.658
)

black_men_coef <- c(ln_age = 2.469, 
                    ln_tchol = 0.302,
                    ln_hdl = -0.307,
                    ln_treated_sys_bp = 1.916,
                    ln_untreated_sys_bp = 1.809,
                    smokes = 0.549,
                    diabetes = 0.645
)

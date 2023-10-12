# =======================================
# ASCVD Risk Model Coefficients 
# =======================================

women_coef <- c(intercept = -12.823110,
                ridageyr = 0.106501, 
                is_black = 0.432440,
                sq_sys_bp = 0.000056,
                sys_bp = 0.017666,
                hypertension_treatment = 0.731678,
                diabetes = 0.943970,
                smokes = 1.009790,
                tc_hdl_ratio = 0.151318,
                age_if_black = -0.008580,
                sys_bp_if_ht_treat = -0.003647,
                sys_bp_if_black = 0.006208,
                black_and_ht_treatment = 0.152968,
                age_times_sys_bp = -0.000153,
                black_and_diabetes = 0.115232,
                black_and_smokes = -0.092231,
                tc_hdl_ratio_if_black = 0.070498,
                sys_bp_if_black_and_hp_treatment = -0.000173,
                age_times_sys_bp_if_black = -0.000094
)

men_coef <- c(intercept = -11.679980,
              ridageyr = 0.064200, 
              is_black = 0.482835,
              sq_sys_bp = -0.000061,
              sys_bp = 0.038950,
              hypertension_treatment = 2.055533,
              diabetes = 0.842209,
              smokes = 0.895589,
              tc_hdl_ratio = 0.193307,
              sys_bp_if_ht_treat = -0.014207,
              sys_bp_if_black = 0.011609,
              black_and_ht_treatment = -0.119460,
              age_times_sys_bp = 0.000025,
              black_and_diabetes = -0.077214,
              black_and_smokes = -0.226771,
              tc_hdl_ratio_if_black = -0.117749,
              sys_bp_if_black_and_hp_treatment = 0.004190 ,
              age_times_sys_bp_if_black = -0.000199
)

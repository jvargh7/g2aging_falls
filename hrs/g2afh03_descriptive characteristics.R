# gc();rm(list=ls());source(".Rprofile") --

source("hrs/g2afhrs_diagnosed diabetes.R")
source("C:/code/external/functions/survey/svysummary.R")

continuous_vars = c("age","hh_size","hh_children","lengthmar",
                    "bmi","height","weight","waistcircumference",
                    "lengthmar","adl_some6", "iadl_some4",
                    "a1c_adj","sbp","dbp","tc_adj","hdl_adj","nfalls","fu_nfalls","agediagnosed_dm",
                    "moderate_pa","vigorous_pa",
                    "polypharmacy")

proportion_vars = c("medication_bp","medication_dm","rxdiabi","rxdiabo",
                    "rxstrok","rxangina","rxchf","rxarthr",
                    "rxlung","rxpsych","rxcancr","rxhrtat","rxheart",
                    
                    
                    "diagnosed_bp","htn",
                    "alzh_demen",
                    "bmi_underweight","bmi_overweight","bmi_obese",
                    "fall_any","fall_injury","fracture_hip",
                    "fu_fall_any","fu_fall_injury","fu_fracture_hip",
                    "heavydrinker",
                    
                    "vision_impairment","balance_impairment","married"
                    )

grouped_vars = c(
                 "age_category",
                 "bmi_category",
                 "education_h","hh_wealthquintile",
                 "smoke","laborforce","raceeth","race","ethnicity"
                 )

hrs_sy <- svysummary(hrs_unique_svy,
                    c_vars = continuous_vars,
                    p_vars = proportion_vars,
                    g_vars = grouped_vars,
                    id_vars = "gender"
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"));

hrs_sy_total <- svysummary(hrs_unique_svy,
                           c_vars = continuous_vars,
                           p_vars = proportion_vars,
                           g_vars = grouped_vars
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"));

bind_rows(hrs_sy_total %>% mutate(gender = "Total"),
          hrs_sy) %>% 
  write_csv(.,path = "hrs/g2afh03_descriptive characteristics.csv")

with(hrs_all_65plus,table(gender))
with(hrs_unique_df,table(gender))
with(hrs_all_65plus,table(fu_fall_any))
with(hrs_all_65plus,table(gender,fu_fall_any))
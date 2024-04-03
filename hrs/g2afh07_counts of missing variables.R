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


missing_counts_by_sex <- hrs_all_65plus %>% 
  group_by(gender) %>% 
  summarize(across(one_of(c(continuous_vars,proportion_vars,grouped_vars)), function(x) sum(is.na(x)))) %>% 
  pivot_longer(cols=-one_of("gender"),names_to="variable",values_to="n") %>% 
  pivot_wider(names_from=gender,values_from=n)

missing_counts_total <- hrs_all_65plus %>% 
  summarize(across(one_of(c(continuous_vars,proportion_vars,grouped_vars)), function(x) sum(is.na(x)))) %>% 
  pivot_longer(cols=everything(),names_to="variable",values_to="Total")

left_join(missing_counts_total,
          missing_counts_by_sex,
          by=c("variable")) %>% 
  write_csv(.,"hrs/g2afh07_counts of missing variables.csv")

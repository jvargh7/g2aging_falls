# gc();rm(list=ls());source(".Rprofile") --

source("elsa/g2afelsa_diagnosed diabetes.R")

continuous_vars = c("age","hh_size","children","lengthmar",
                    "bmi","height","weight","waistcircumference",
                    "lengthmar","adl_some6", "iadl_some4",
                    "hba1c","sbp","dbp","hdl","trig","ldl","chol",
                    "nfalls","fu_nfalls","agediagnosed_dm",
                    "moderate_pa","vigorous_pa","polypharmacy"
)

proportion_vars = c("medication_bp","medication_dm","rxdiabi","rxdiabo",
                    "rxlung","rxasthma","rxcancr","rxbldthn","rxhchol",
                    
                    "married",
                    "diagnosed_bp","htn",
                    "alzh_demen",
                    "bmi_underweight","bmi_overweight","bmi_obese",
                    "fall_any","fall_injury","fracture_hip",
                    "fu_fall_any","fu_fall_injury","fu_fracture_hip",
                    
                    "heavydrinker",
                    
                    "vision_impairment","balance_impairment"
                    
)

grouped_vars = c(
  "age_category",
  "bmi_category",
  "education_h","hh_wealthquintile",
  "smoke","laborforce","race"
)

missing_counts_by_sex <- elsa_all_65plus %>% 
  group_by(gender) %>% 
  summarize(across(one_of(c(continuous_vars,proportion_vars,grouped_vars)), function(x) sum(is.na(x)))) %>% 
  pivot_longer(cols=-one_of("gender"),names_to="variable",values_to="n") %>% 
  pivot_wider(names_from=gender,values_from=n)

missing_counts_total <- elsa_all_65plus %>% 
  summarize(across(one_of(c(continuous_vars,proportion_vars,grouped_vars)), function(x) sum(is.na(x)))) %>% 
  pivot_longer(cols=everything(),names_to="variable",values_to="Total")

left_join(missing_counts_total,
          missing_counts_by_sex,
          by=c("variable")) %>% 
  write_csv(.,"elsa/g2afe07_counts of missing variables.csv")

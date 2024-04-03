# gc();rm(list=ls());source(".Rprofile") --

source("elsa/g2afelsa_diagnosed diabetes.R")
source("C:/code/external/functions/survey/svysummary.R")


excluded_df = elsa_all_65plus_control_available %>% 
  dplyr::filter(is.na(fu_fall_any))


elsa_excluded_svy <- excluded_df %>% 
  as_survey_design(ids = psu,
                   strata  = strata,
                   weight = normalizedweight,
                   nest = TRUE,
                   variance = "YG")

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
                    # "fu_fall_any","fu_fall_injury","fu_fracture_hip",
                    
                    "heavydrinker",
                    
                    "vision_impairment","balance_impairment"
                    
)

grouped_vars = c(
  "age_category",
  "bmi_category",
  "education_h","hh_wealthquintile",
  "smoke","laborforce","race"
)

elsa_sy <- svysummary(elsa_excluded_svy,
                      c_vars = continuous_vars,
                      p_vars = proportion_vars,
                      g_vars = grouped_vars,
                      id_vars = "gender"
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"));

elsa_sy_total <- svysummary(elsa_excluded_svy,
                            c_vars = continuous_vars,
                            p_vars = proportion_vars,
                            g_vars = grouped_vars
                            # id_vars = "gender"
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"));


bind_rows(elsa_sy_total %>% mutate(gender = "Total"),
          elsa_sy) %>% 
  write_csv(.,path = "elsa/g2afe08_excluded characteristics.csv")

with(elsa_all_65plus,table(gender))
with(elsa_unique_df,table(gender))
with(elsa_all_65plus,table(fu_fall_any))
with(elsa_all_65plus,table(gender,fu_fall_any))
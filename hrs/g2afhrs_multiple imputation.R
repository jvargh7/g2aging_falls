source("hrs/g2afhrs_diagnosed diabetes.R")
rm(hrs_unique_df,hrs_unique_svy)

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
                    
                    "vision_impairment","balance_impairment",
                    
                    "control_joint","control_a1c","control_bp","control_chol"
)

grouped_vars = c(
  "age_category",
  "bmi_category",
  "education_h","hh_wealthquintile",
  "smoke","laborforce","raceeth",
  # "race","ethnicity" # We remove them because we are using race
  "gender"
)

id_vars = c("hhid","pn","hhidpn","spousepn","spouseidpn","strata","psu","coupleid","wave",
            "hhanalysisweight","hhsampleweight","indsampleweight","normalizedweight")


library(survey)
library(mice)

before_imputation <- hrs_all_65plus %>% 
  dplyr::select(one_of(id_vars),
                one_of(continuous_vars),one_of(proportion_vars),one_of(grouped_vars)) %>% 
  mutate_at(vars(fall_any,fall_injury,fracture_hip),~as.numeric(.)) %>%
  # Step 1: One hot encoding
  mutate(
    ge75 = case_when(age >= 75 ~ 1,
                       TRUE ~ 0),
    female = case_when(gender == "Female" ~ 1,
                       TRUE ~ 0)
    
  ) %>% 
  # Step 2: Modeling interactions
  mutate(control_joint_fall_any = fall_any*control_joint,
         
         
         # Since exposure is w_htn and effect modifier is husband's education
         control_joint_balance_impairment = balance_impairment*control_joint,
         
         control_joint_ge75 = control_joint*ge75,
         
         control_joint_rxdiabi = control_joint*rxdiabi,
         
         control_joint_female = control_joint*female
         
  ) 

before_imputation %>% 
  dplyr::select(-one_of(id_vars),-gender) %>% 
  mutate(any_missing = rowSums(is.na(.))) %>% 
  summarize(prop = mean(any_missing>0))

interaction_terms <- c("control_joint_fall_any",
                       "control_joint_balance_impairment",
                       "control_joint_ge75",
                       "control_joint_rxdiabi",
                       "control_joint_female"
)

before_imputation %>% 
  summarize(across(one_of(interaction_terms),.fns=function(x) paste0(round(mean(x,na.rm=TRUE),1)," (",round(mean(!is.na(x)),1),")")))


mi_null <- mice(before_imputation,
                maxit = 0)

method = mi_null$method
pred = mi_null$predictorMatrix

pred[id_vars,] <- 0
pred[,id_vars] <- 0
method[id_vars] <- ""


# Impute via equation and do not use for imputation , --------
method["ge75"] <- "~I((age>=75)*1)"
pred[c("ge75"),] <- 0


# https://stackoverflow.com/questions/33865161/model-multiple-imputation-with-interaction-terms
# https://thestatsgeek.com/2014/05/10/multiple-imputation-with-interactions-and-non-linear-terms/

for(i_t in interaction_terms){
  print(i_t)
  exposure_term = str_extract(i_t,"^control_joint")
  em_term = str_replace(i_t,pattern=paste0(exposure_term,"_"),replacement = "")
  method[i_t] = paste0("~I(",exposure_term,"*",em_term,")")
  
  # Do not use interaction terms for imputation of the source variables
  pred[c(exposure_term,em_term),i_t] <- 0
}


# Takes ~4h
mi_dfs <- mice(before_imputation,
               method = method,
               pred = pred,
               m=10,maxit=50,seed=500)

saveRDS(mi_dfs, paste0(path_g2a_falls_folder,"/working/g2afhrs_multiple imputation.RDS"))

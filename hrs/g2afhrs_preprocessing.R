require(lubridate)
g2afhrs_preprocessing <- function(df){
  df %>% 
    # Blood pressure cleaning -------
  mutate(
    diagnosed_dm = case_when(diagnosed_dm == 1 ~ 1,
                             TRUE ~ 0),
    medication_dm = case_when(medication_dm == 1 ~ 1,
                              TRUE ~ 0),
    
    
    
    diagnosed_bp = case_when(diagnosed_bp == 1 ~ 1,
                             TRUE ~ 0),
    medication_bp = case_when(medication_bp == 1 ~ 1,
                              TRUE ~ 0))  %>% 
    
    mutate(bmi_underweight = case_when(bmi > bmi_max ~ NA_real_,
                                       bmi < bmi_cutoff[1] ~ 1,
                                       bmi >= bmi_cutoff[1] ~ 0,
                                       TRUE ~ NA_real_),
           
           
           bmi_overweight = case_when(bmi > bmi_max ~ NA_real_,
                                      bmi >= bmi_cutoff[2] & bmi < bmi_cutoff[3] ~ 1,
                                      bmi < bmi_cutoff[2] | bmi >= bmi_cutoff[3] ~ 0,
                                      TRUE ~ NA_real_),
           
           
           bmi_obese = case_when(bmi > bmi_max ~ NA_real_,
                                 bmi >= bmi_cutoff[3] ~ 1,
                                 bmi < bmi_cutoff[3] ~ 0,
                                 TRUE ~ NA_real_)) %>% 
    
    mutate(
      htn = case_when(diagnosed_bp == 1 ~ 1,
                      is.na(sbp) | is.na(dbp) ~ NA_real_,
                      sbp >= sbp_cutoff ~ 1,
                      dbp >= dbp_cutoff ~ 1,
                      sbp < sbp_cutoff ~ 0,
                      dbp < dbp_cutoff ~ 0,
                      TRUE ~ NA_real_),
      highbp = case_when(
        is.na(sbp) | is.na(dbp) ~ NA_real_,
        sbp >= sbp_cutoff ~ 1,
        dbp >= dbp_cutoff ~ 1,
        sbp < sbp_cutoff ~ 0,
        dbp < dbp_cutoff ~ 0,
        TRUE ~ NA_real_),
      # Among those diagnosed, indicator of hypertension control status
      diaghtn = case_when(
        diagnosed_bp == 0 ~ NA_real_,
        is.na(sbp) | is.na(dbp) ~ NA_real_,
        diagnosed_bp == 1 & age <= agebp_cutoff & sbp >= sbp_target[1] ~ 1,
        diagnosed_bp == 1 & age <= agebp_cutoff & dbp >= dbp_target[1] ~ 1,
        diagnosed_bp == 1 & age <= agebp_cutoff & sbp < sbp_target[1] ~ 0,
        diagnosed_bp == 1 & age <= agebp_cutoff & dbp < dbp_target[1] ~ 0,
        
        diagnosed_bp == 1 & age > agebp_cutoff & sbp >= sbp_target[2] ~ 1,
        diagnosed_bp == 1 & age > agebp_cutoff & dbp >= dbp_target[2] ~ 1,
        diagnosed_bp == 1 & age > agebp_cutoff & sbp < sbp_target[2] ~ 0,
        diagnosed_bp == 1 & age > agebp_cutoff & dbp < dbp_target[2] ~ 0,
        
        TRUE ~ NA_real_)
    ) %>% 
    
    # Hypertension cascade -----
  mutate(htn_sample = case_when(!is.na(sbp)|!is.na(dbp) ~ 1,
                                is.na(sbp) & is.na(dbp) ~ 0,
                                TRUE ~ 1),
         # Diagnosis: No/DK, Blood pressure: in range
         htn_free = case_when(
           is.na(htn) ~ NA_real_,
           htn == 1 ~ 0,
           htn == 0 ~ 1,
           TRUE ~ NA_real_),
         
         htn_undiag_htn = case_when(diagnosed_bp == 1 | is.na(diagnosed_bp) ~ NA_real_,
                                    htn == 1 ~ 1,
                                    htn == 0 ~ 0,
                                    TRUE ~ NA_real_),
         
         # Diagnosis: Yes + Treated: No, Blood pressure: <NA>
         htn_diag_untreat = case_when(diagnosed_bp == 1 & medication_bp == 1 ~ 0,
                                      diagnosed_bp == 1 & medication_bp == 0 ~ 1,
                                      TRUE ~ NA_real_),
         
         # Dignosis: Yes, Treated: Yes, Blood pressure: out of control range
         htn_treat_uncontr = case_when(medication_bp == 0 | is.na(medication_bp)  ~ NA_real_,
                                       medication_bp == 1 & diaghtn == 1 ~ 1,
                                       medication_bp == 1 & diaghtn == 0 ~ 0,
                                       TRUE ~ NA_real_),
         # Dignosis: Yes, Treated: Yes, Blood pressure: in range
         htn_treat_contr = 1 - htn_treat_uncontr,
         
         # Dignosis: Yes, Treated: Yes or No, Blood pressure: out of control range
         htn_diag_uncontr = case_when(diagnosed_bp == 0 | is.na(diagnosed_bp)  ~ NA_real_,
                                      diaghtn == 1 ~ 1,
                                      diaghtn == 0 ~ 0,
                                      TRUE ~ NA_real_),
         # Dignosis: Yes, Treated: Yes, Blood pressure: in range
         htn_diag_contr = 1 - htn_diag_uncontr
         
  ) %>%
    
    # Prediabetes and Prehypertension ------
  mutate(
    prehypertension = case_when(diagnosed_bp == 1 ~ NA_real_,
                                is.na(sbp) | is.na(dbp) ~ NA_real_,
                                htn == 1 ~ 0,
                                sbp >= sbppre_cutoff & sbp < sbp_cutoff ~ 1,
                                dbp >= dbppre_cutoff & dbp < dbp_cutoff~ 1,
                                sbp < sbppre_cutoff ~ 0,
                                dbp < dbppre_cutoff ~ 0,
                                TRUE ~ NA_real_)
  ) %>% 
    mutate(gender = case_when(gender == 1 ~ "Male",
                              gender == 2 ~ "Female"),
           spousegender = case_when(spousegender == 1 ~ "Male",
                                    spousegender == 2 ~ "Female"),
           education_h = case_when(education_h %in% c(1:3) ~ education_h,
                                   TRUE ~ NA_real_)
           
           
    ) %>% 
    mutate(laborforce = case_when(laborforce %in% c(1,2) ~ "employed",
                                  laborforce %in% c(4,5) ~ "retired",
                                  TRUE ~ "other")) %>% 
    
    mutate_at(vars(diagnosed_bp,medication_bp,
                   diagnosed_dm,medication_dm), function(x) case_when(x== 2 ~ 0,
                                                                      x == 1 ~ 1,
                                                                      TRUE ~ NA_real_)) %>% 
    mutate(raceeth = case_when(ethnicity == 1 ~ "Hispanic",
                               race == 1 ~ "NH White",
                               race == 2 ~ "NH Black",
                               race == 3 ~ "NH Other",
                               TRUE ~ NA_character_)) %>% 
    # BMI
    mutate_at(vars(bmi),function(x) case_when(x > bmi_max ~ NA_real_,
                                              TRUE ~ as.numeric(x))) %>%
    # Circumference
    mutate_at(vars(waistcircumference),function(x) case_when(x > 240 ~ NA_real_,
                                                             TRUE ~ as.numeric(x))) %>% 
    
    # Education - harmonized
    mutate_at(vars(education_h),function(x) case_when(x == 1 ~ "Less than lower secondary",
                                                      x == 2 ~ "Upper secondary and vocational training",
                                                      x == 3 ~ "Tertiary",
                                                      TRUE ~ NA_character_)) %>% 
    
    # # Religion
    # mutate_at(vars(religion),function(x) case_when(x == 12 ~ "Hindu",
    #                                                   x == 13 ~ "Muslim",
    #                                                   TRUE ~ "Other")) %>% 
    # insurance, alcohol
    mutate_at(vars(alcohol), function(x) case_when(x == 0 ~ 0,
                                                   x == 1 ~ 1,
                                                   TRUE ~ NA_real_)) %>% 
    # Smoking
    mutate_at(vars(smokeever,smokecurr),function(x) case_when(x == 0 ~ 0,
                                                              x == 1 ~ 1,
                                                              TRUE ~ NA_real_)) %>% 
    
    mutate(smoke = case_when(smokecurr == 1 ~ 2,
                             smokeever == 1 ~ 1,
                             smokeever == 0 ~ 0, 
                             smokecurr == 0 ~ 0,
                             TRUE ~ 0)) %>% 
    mutate(smoke = factor(smoke,levels=c(0:2),labels=c("Never","Former","Current"))) %>% 
    mutate(htn_disease = case_when(is.na(htn_free) ~ NA_real_,
                                   htn_free == 1 ~ 0,
                                   htn_undiag_htn == 1 ~ 1,
                                   htn_diag_untreat == 1 ~ 1,
                                   htn_treat_uncontr == 1 ~ 1,
                                   htn_treat_contr == 1 ~ 1,
                                   TRUE ~ 0),
           
           htn_diagnosed = case_when(is.na(htn_free) ~ NA_real_,
                                     htn_free == 1 ~ 0,
                                     htn_undiag_htn == 1 ~ 0,
                                     htn_diag_untreat == 1 ~ 1,
                                     htn_treat_uncontr == 1 ~ 1,
                                     htn_treat_contr == 1 ~ 1,
                                     TRUE ~ 0
           ),
           htn_treated = case_when(is.na(htn_free) ~ NA_real_,
                                   htn_free == 1 ~ 0,
                                   htn_undiag_htn == 1 ~ 0,
                                   htn_diag_untreat == 1 ~ 0,
                                   htn_treat_uncontr == 1 ~ 1,
                                   htn_treat_contr == 1 ~ 1,
                                   TRUE ~ 0
           ),
           htn_controlled = case_when(is.na(htn_free) ~ NA_real_,
                                      htn_free == 1 ~ 0,
                                      htn_undiag_htn == 1 ~ 0,
                                      htn_diag_contr == 1 ~ 1,
                                      htn_diag_untreat == 1 ~ 0,
                                      htn_diag_uncontr == 1 ~ 0,
                                      TRUE ~ 0
           ),
           
           htn_diagnosed_in_dis = case_when(is.na(htn_free) ~ NA_real_,
                                            htn_free == 1 ~ NA_real_,
                                            htn_undiag_htn == 1 ~ 0,
                                            htn_diag_untreat == 1 ~ 1,
                                            htn_treat_uncontr == 1 ~ 1,
                                            htn_treat_contr == 1 ~ 1,
                                            TRUE ~ 0
           ),
           htn_treated_in_dis = case_when(is.na(htn_free) ~ NA_real_,
                                          htn_free == 1 ~ NA_real_,
                                          htn_undiag_htn == 1 ~ 0,
                                          htn_diag_untreat == 1 ~ 0,
                                          htn_treat_uncontr == 1 ~ 1,
                                          htn_treat_contr == 1 ~ 1,
                                          TRUE ~ 0
           ),
           htn_controlled_in_dis = case_when(is.na(htn_free) ~ NA_real_,
                                             htn_free == 1 ~ NA_real_,
                                             htn_undiag_htn == 1 ~ 0,
                                             htn_diag_contr == 1 ~ 1,
                                             htn_diag_untreat == 1 ~ 0,
                                             htn_diag_uncontr == 1 ~ 0,
                                             TRUE ~ 0
           )) %>% 
    
    mutate(bmi_category = case_when(bmi > bmi_max ~ NA_real_,
                                    bmi >= bmi_cutoff[3] ~ 4,
                                    bmi >= bmi_cutoff[2] ~ 3,
                                    bmi >= bmi_cutoff[1] ~ 2,
                                    bmi < bmi_cutoff[1] ~ 1,
                                    TRUE ~ NA_real_),
           
           highwc = case_when(gender == "Female" & waistcircumference >= female_wc_cutoff ~ 1,
                              gender == "Female" & waistcircumference < female_wc_cutoff ~ 0,
                              gender == "Male" & waistcircumference >= male_wc_cutoff ~ 1,
                              gender == "Male" & waistcircumference < male_wc_cutoff ~ 0,
                              TRUE ~ NA_real_
           ),
           
           lengthmar_ge10 = case_when(lengthmar >= 10 ~ 1,
                                      lengthmar < 10 ~ 0,
                                      TRUE ~ NA_real_),
           firstmar = case_when(nmarriages == 1 ~ 1,
                                nmarriages > 1 ~ 0,
                                TRUE ~ NA_real_),
           married = case_when(marstat %in% c(1,2) ~ 1,
                               TRUE ~ 0)
    ) %>% 
    
    mutate(bmi_category = factor(bmi_category,levels=c(1:4),labels=c("Underweight","Normal","Overweight","Obese"))) %>% 
    
    mutate_at(vars(diagnosed_dm,medication_dm,
                   diagnosed_bp,medication_bp),~case_when(is.na(.) ~ 0,
                                                          TRUE ~ .)) %>% 
    
    mutate_at(vars(moderate_pa,vigorous_pa),function(x) case_when(x == 1 ~ 7,
                                                                  x == 2 ~ 3.5,
                                                                  x == 3 ~ 1,
                                                                  x == 4 ~ 0.5,
                                                                  x == 5 ~ 0,
                                                                  TRUE ~ NA_real_)) %>% 
    
    mutate(across(contains("rx"),.fns=function(x) case_when(x  == 1 ~ 1,
                                                            TRUE ~ 0))) %>% 
    mutate(across(one_of(c("sight_selfrated","sight_near", 
                           "cataract_surgery_ever",
                           "fall_any","fall_injury",
                           "fall_equip", "fracture_hip", 
                           "cataract","alzh","dementia","alzh_ever","dementia_ever",
                           "sbs_complete","semitandem_complete","fulltandem_complete",
                           "balance","legraise_eyesopen","legraise_eyesshut",
                           "chr1comp","chrcomp")),.fns = function(x) case_when(x < 0 ~ NA_real_,
                                                                               TRUE ~ x))) %>% 
    
    return(.)
}


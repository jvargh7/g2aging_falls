gc();rm(list=ls());source(".Rprofile")

analytic_falls_elsa <- function(baseline_wave = 2,followup_wave = 3){
  biomarkers <- readRDS(paste0(path_g2a_longitudinal_folder,"/working/elsa biomarkers.RDS")) %>% 
    # Do not include wave 9 because we need to use it as the follow-up for wave8
    dplyr::filter(is.na(wavevisit)|wavevisit == 8)
  
  followup_vars <- c("fall_any","fall_injury","nfalls","fall_equip","fracture_hip")
  
  baseline <- bind_rows(
    readRDS(paste0(path_g2a_falls_folder,"/working/elsa/G2A ELSA Falls Wave ",baseline_wave," male.RDS"))  %>% 
      dplyr::filter(sampleweight > 0),
    
    readRDS(paste0(path_g2a_falls_folder,"/working/elsa/G2A ELSA Falls Wave ",baseline_wave," female.RDS"))  %>% 
      dplyr::filter(sampleweight > 0)
    
  ) 
  
  followup <- bind_rows(
    readRDS(paste0(path_g2a_falls_folder,"/working/elsa/G2A ELSA Falls Wave ",followup_wave," male.RDS"))  %>% 
      dplyr::filter(sampleweight > 0),
    
    readRDS(paste0(path_g2a_falls_folder,"/working/elsa/G2A ELSA Falls Wave ",followup_wave," female.RDS"))  %>% 
      dplyr::filter(sampleweight > 0)
    
  ) %>% 
    dplyr::select(personid,sampleweight,one_of(followup_vars)) %>% 
    rename(fu_sampleweight = sampleweight) %>% 
    rename_at(vars(matches(paste0("(",paste0(followup_vars,collapse="|"),")"))),~paste0("fu_",.)) %>% 
    mutate_at(vars(starts_with("fu_")),~as.numeric(.))
  
  analytic_df = left_join(baseline,
                          biomarkers %>% 
                            dplyr::select(-year) %>% 
                            dplyr::filter(wave == baseline_wave),
                          by = c("personid")) %>% 
    left_join(followup,
              by=c("personid")) 
  
  return(analytic_df)
  
  
  
}

wave8 <- analytic_falls_elsa(8,9)
wave6 <- analytic_falls_elsa(6,7)
wave4 <- analytic_falls_elsa(4,5)
wave2 <- analytic_falls_elsa(2,3)


all_elsa <- bind_rows(wave2,
          wave4,
          wave6,
          wave8) 

elsa_with_bldwt <- all_elsa %>% 
  dplyr::filter(!is.na(bldwt),age>=65,bldwt > 0) %>% 
  mutate(normalizedweight = bldwt/sum(bldwt)) %>% 
  mutate(normalizedweight = normalizedweight/n()) %>% 
  ungroup() %>% 
  mutate(normalizedweight = normalizedweight*n())  


elsa_with_bldwt_diagnoseddm <- elsa_with_bldwt %>% 
  mutate(
    # https://www.omnicalculator.com/health/cholesterol-units
    hdl = hdl*38.67,
    chol = chol*38.67,
    ldl = ldl*38.67,
    # https://heartcare.sydney/cholesterol-unit-conversion/
    trig = trig*88.57) %>% 
  dplyr::filter(diagnosed_dm == 1) %>% 
  group_by(wave)   %>% 
  mutate(iadl_any4 = case_when(iadl_any4 >= 0 ~ iadl_any4,
                               TRUE ~ NA_real_),
         fall_any  = case_when(fall_any >= 0 ~ as.numeric(fall_any),
                               TRUE ~ NA_real_),
         correct_countbackwards = case_when(countbackwards %in% c(1,2) ~ 1,
                                            countbackwards == 0 ~ 0,
                                            TRUE ~ NA_real_),
         correct_serial7subtract = case_when(serial7subtract %in% c(3:5) ~ 1,
                                             serial7subtract %in% c(0:2) ~ 0,
                                             TRUE ~ NA_real_),
         correct_immediatewordrecall = case_when(immediatewordrecall %in% c(5:10) ~ 1,
                                                 immediatewordrecall %in% c(0:4) ~ 0,
                                                 TRUE ~ NA_real_),
         correct_delayedwordrecall = case_when(delayedwordrecall %in% c(4:10) ~ 1,
                                               delayedwordrecall %in% c(0:3) ~ 0,
                                               TRUE ~ NA_real_)
  ) %>% 
  mutate(
    composite_cognition = rowSums(.[,c("correct_immediatewordrecall","correct_delayedwordrecall",
                                       "correct_countbackwards","correct_serial7subtract")],na.rm=TRUE),
    valid_cognition = rowSums(!is.na(.[,c("correct_immediatewordrecall","correct_delayedwordrecall",
                                          "correct_countbackwards","correct_serial7subtract")]),na.rm=TRUE)) %>% 
  mutate(composite_cognition_impairment = case_when(wave %in% c(8,9) & valid_cognition < 4 ~ NA_real_,
                                                    wave %in% c(2,4,6) & valid_cognition < 2 ~ NA_real_,
                                                    composite_cognition %in% c(3,4) &  wave %in% c(8,9) ~ 0,
                                                    composite_cognition %in% c(2) &  wave %in% c(2,4,6) ~ 1,
                                                    composite_cognition %in% c(0:2) &  wave %in% c(8,9) ~ 1,
                                                    composite_cognition %in% c(0:1) &  wave %in% c(2,4,6) ~ 0,
                                                    TRUE ~ NA_real_)) %>% 
  mutate(control_a1c = case_when(hba1c < 2.0 | hba1c >= 20.0 ~ NA_real_,
                                 hba1c < hba1c_target ~ 1,
                                 hba1c >= hba1c_target ~ 0,
                                 TRUE ~ NA_real_),
         
         control_bp = case_when(sbp < sbp_target[1] & dbp < dbp_target[1] ~ 1,
                                sbp >= 140 | dbp >= 90 ~ 0,
                                TRUE ~ NA_real_),
         
         control_chol = case_when(chol <= totalchol_target ~ 1,
                                  chol > totalchol_target ~ 0,
                                  TRUE ~ NA_real_),
         
         control_ldl = case_when(ldl <= ldl_target ~ 1,
                                  ldl > ldl_target ~ 0,
                                  TRUE ~ NA_real_),
         
         control_joint = case_when(is.na(control_a1c) | is.na(control_bp) | is.na(control_chol) ~ NA_real_,
                                   control_a1c == 1 & control_bp == 1 & control_chol == 1 ~ 1,
                                   TRUE ~ 0),
         control_joint2 = case_when(is.na(control_a1c) | is.na(control_bp) | is.na(control_ldl) ~ NA_real_,
                                    control_a1c == 1 & control_bp == 1 & control_ldl == 1 ~ 1,
                                    TRUE ~ 0)
  ) %>% 
  mutate(polypharmacy = rowSums(.[(c("medication_bp","rxdiabi","rxdiabo",
                                     "rxlung",
                                     "rxasthma",
                                     "rxcancr",
                                     "rxbldthn",
                                     "rxhchol"))],na.rm=TRUE),
         vision_impairment = case_when(sight_selfrated %in% c(5:6) ~ 1,
                                       sight_near %in% c(5:6) ~ 1,
                                       sight_selfrated %in% c(1:4) &
                                         sight_near %in% c(1:4) ~ 0,
                                       TRUE ~ NA_real_),
         balance_impairment = case_when(age <70 & fulltandem_seconds == 30 ~ 1,
                                        age >= 70 & fulltandem_seconds >= 10 ~ 1,
                                        !is.na(fulltandem_seconds) ~ 0,
                                        TRUE ~ NA_real_),
         married = case_when(marital %in% c(1) ~ 1,
                             marital %in% c(3,4,5,7,8) ~ 0, # Partnered, Separated, Divorced, Widowed, Never Married
                             TRUE ~ NA_real_))
  
elsa_with_bldwt_diagnoseddm %>% 
  saveRDS(.,paste0(path_g2a_falls_folder,"/working/g2afe02_analytic dataset.RDS"))


# Checking counts -------


all_elsa %>% 
  # group_by(gender) %>%
  tally()

all_elsa %>% 
  distinct(personid,gender) %>% 
  # group_by(gender) %>%
  tally()

elsa_with_bldwt %>% 
  # group_by(gender) %>%
  tally()

elsa_with_bldwt %>% 
  distinct(personid,gender) %>% 
  # group_by(gender) %>%
  tally()

elsa_with_bldwt_diagnoseddm %>% 
  # group_by(gender) %>%
  tally()

elsa_with_bldwt_diagnoseddm %>% 
  distinct(personid,gender) %>% 
  # group_by(gender) %>%
  tally()

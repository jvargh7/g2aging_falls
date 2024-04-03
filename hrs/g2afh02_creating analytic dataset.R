gc();rm(list=ls());source(".Rprofile")
# 
# male_14 <- readRDS(paste0(path_g2a_falls_folder,"/working/hrs/G2A HRS Falls Wave 14 male.RDS"))  %>% 
#   dplyr::filter(!is.na(indsampleweight))
# female_14 <- readRDS(paste0(path_g2a_falls_folder,"/working/hrs/G2A HRS Falls Wave 14 female.RDS"))    %>% 
#   dplyr::filter(!is.na(indsampleweight))
# 
# 
# male_13 <- readRDS(paste0(path_g2a_falls_folder,"/working/hrs/G2A HRS Falls Wave 13 male.RDS"))  %>% 
#   dplyr::filter(!is.na(indsampleweight))
# female_13 <- readRDS(paste0(path_g2a_falls_folder,"/working/hrs/G2A HRS Falls Wave 13 female.RDS"))    %>% 
#   dplyr::filter(!is.na(indsampleweight))


analytic_falls <- function(baseline_wave = 13,followup_wave = 14){
  biomarkers <- readRDS(paste0(path_g2a_longitudinal_folder,"/working/hrs biomarkers.RDS")) 
  
  followup_vars <- c("fall_any","fall_injury","nfalls","fall_equip","fracture_hip")
  
  baseline <- bind_rows(
    readRDS(paste0(path_g2a_falls_folder,"/working/hrs/G2A HRS Falls Wave ",baseline_wave," male.RDS"))  %>% 
      dplyr::filter(!is.na(indsampleweight)),
    
    readRDS(paste0(path_g2a_falls_folder,"/working/hrs/G2A HRS Falls Wave ",baseline_wave," female.RDS"))  %>% 
      dplyr::filter(!is.na(indsampleweight))
    
  ) 
  
  followup <- bind_rows(
    readRDS(paste0(path_g2a_falls_folder,"/working/hrs/G2A HRS Falls Wave ",followup_wave," male.RDS"))  %>% 
      dplyr::filter(!is.na(indsampleweight)),
    
    readRDS(paste0(path_g2a_falls_folder,"/working/hrs/G2A HRS Falls Wave ",followup_wave," female.RDS"))  %>% 
      dplyr::filter(!is.na(indsampleweight))
    
  ) %>% 
    dplyr::select(hhid,pn,indsampleweight,one_of(followup_vars)) %>% 
    rename(fu_indsampleweight = indsampleweight) %>% 
    rename_at(vars(matches(paste0("(",paste0(followup_vars,collapse="|"),")"))),~paste0("fu_",.)) %>% 
    mutate_at(vars(starts_with("fu_")),~as.numeric(.))
  
  analytic_df = left_join(baseline,
                          biomarkers %>% 
                            dplyr::filter(wave == baseline_wave),
                          by = c("hhid","pn")) %>% 
    left_join(followup,
              by=c("hhid","pn")) 
  
  return(analytic_df)
  
  
  
}

wave13 <- analytic_falls(13,14)
wave12 <- analytic_falls(12,13)
wave11 <- analytic_falls(11,12)
wave10 <- analytic_falls(10,11)
wave9 <- analytic_falls(9,10)
wave8 <- analytic_falls(8,9)

all_hrs <- bind_rows(wave8,
          wave9,
          wave10,
          wave11,
          wave12,
          wave13) %>% 
  # blversion == 1 -->
  mutate(biowgtr = case_when(wave == 13 & blversion == 1 ~ indsampleweight*2,
                             is.na(biowgtr) ~ indsampleweight*2,
                             TRUE ~ biowgtr)) 


hrs_with_biowgtr <- all_hrs %>% 
  dplyr::filter(!is.na(blversion),age>=65,biowgtr > 0) %>% 
  group_by(wave) %>% 
  mutate(normalizedweight = biowgtr/sum(biowgtr)) %>% 
  mutate(normalizedweight = normalizedweight/n()) %>% 
  ungroup() %>% 
  mutate(normalizedweight = normalizedweight*n()) %>% 
  mutate(alzh_demen = case_when(wave == 8 & memoryproblem == 1 ~ 1,
                                alzh == 1 | dementia == 1 ~ 1,
                                TRUE ~ 0),
         totalwordrecall = immediatewordrecall + delayedwordrecall) 

hrs_with_biowgtr_diagnoseddm = hrs_with_biowgtr %>% 
  dplyr::filter(diagnosed_dm == 1) %>%  
  mutate(correct_countbackwards = case_when(countbackwards %in% c(1,2) ~ 1,
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
  mutate(composite_cognition_impairment = case_when(valid_cognition < 4 ~ NA_real_,
                                                    composite_cognition %in% c(3,4) ~ 0,
                                                    composite_cognition %in% c(0:2) ~ 1,
                                                    TRUE ~ NA_real_)) %>% 
  mutate(control_a1c = case_when(a1c_adj < 2.0 | a1c_adj >= 20.0 ~ NA_real_,
                                 a1c_adj < 7.5 ~ 1,
                                 a1c_adj >= 7.5 ~ 0,
                                 TRUE ~ NA_real_),
         
         control_bp = case_when(sbp < 140 & dbp < 90 ~ 1,
                                sbp >= 140 | dbp >= 90 ~ 0,
                                TRUE ~ NA_real_),
         
         control_chol = case_when(tc_adj <= 200 ~ 1,
                                  tc_adj > 200 ~ 0,
                                  TRUE ~ NA_real_),
         
         control_joint = case_when(is.na(control_a1c) | is.na(control_bp) | is.na(control_chol) ~ NA_real_,
                                   control_a1c == 1 & control_bp == 1 & control_chol == 1 ~ 1,
                                   TRUE ~ 0)
  )  %>% 
  
  mutate(polypharmacy = rowSums(.[(c("medication_bp","rxdiabi","rxdiabo",
                                     "rxstrok",
                                     "rxangina",
                                     "rxchf",
                                     "rxlung",
                                     "rxcancr",
                                     "rxhrtat",
                                     "rxheart",
                                     "rxhchol",
                                     "rxbreath",
                                     "rxstomach",
                                     "rxdepres"))],na.rm=TRUE),
         vision_impairment = case_when(sight_selfrated %in% c(5:6) ~ 1,
                                       sight_near %in% c(5:6) ~ 1,
                                       sight_selfrated %in% c(1:4) &
                                         sight_near %in% c(1:4) ~ 0,
                                       TRUE ~ NA_real_),
         balance_impairment = case_when(age <70 & fulltandem_seconds == 30 ~ 1,
                                        age >= 70 & fulltandem_seconds >= 10 ~ 1,
                                        !is.na(fulltandem_seconds) ~ 0,
                                        TRUE ~ NA_real_))  


hrs_with_biowgtr_diagnoseddm %>% 
  saveRDS(.,paste0(path_g2a_falls_folder,"/working/g2afh02_analytic dataset.RDS"))

# Checking counts -------


all_hrs %>% 
  # group_by(gender) %>% 
  tally()

all_hrs %>% 
  distinct(hhidpn,gender) %>% 
  # group_by(gender) %>% 
  tally()


hrs_with_biowgtr %>% 
  group_by(gender) %>%
  tally()

hrs_with_biowgtr %>% 
  distinct(hhidpn,gender) %>% 
  # group_by(gender) %>%
  tally()


hrs_with_biowgtr_diagnoseddm %>% 
  # group_by(gender) %>%
  tally()

hrs_with_biowgtr_diagnoseddm %>% 
  distinct(hhidpn,gender) %>% 
  # group_by(gender) %>%
  tally()

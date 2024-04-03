gc();rm(list=ls());source(".Rprofile")

library(srvyr)

source("C:/code/external/functions/survey/svysummary.R")

# DATA AGGREGATION ------------

prelim_hrs <- function(baseline_wave = 13){
  biomarkers <- readRDS(paste0(path_g2a_longitudinal_folder,"/working/hrs biomarkers.RDS")) 
  
  followup_vars <- c("fall_any","fall_injury","nfalls","fall_equip","fracture_hip",
                     "age",
                     "diagnosed_dm","iadl_some4","iadl_any4",
                     "immediatewordrecall","delayedwordrecall",
                     "countbackwards","serial7subtract",
                     "sbp","dbp")
  
  baseline <- bind_rows(
    readRDS(paste0(path_g2a_falls_folder,"/working/hrs/G2A HRS Falls Wave ",baseline_wave," male.RDS"))  %>% 
      dplyr::filter(!is.na(indsampleweight)),
    
    readRDS(paste0(path_g2a_falls_folder,"/working/hrs/G2A HRS Falls Wave ",baseline_wave," female.RDS"))  %>% 
      dplyr::filter(!is.na(indsampleweight))
    
  ) %>% 
    dplyr::select(hhid,pn,indsampleweight,psu,strata,one_of(followup_vars)) 
  
  analytic_df = left_join(baseline,
                          biomarkers %>% 
                            dplyr::filter(wave == baseline_wave),
                          by = c("hhid","pn")) %>% 
    mutate(wave = baseline_wave)
  
  return(analytic_df)
  
}

wave14 <- prelim_hrs(14)
wave13 <- prelim_hrs(13)
wave12 <- prelim_hrs(12)
wave11 <- prelim_hrs(11)
wave10 <- prelim_hrs(10)
wave9 <- prelim_hrs(9)
wave8 <- prelim_hrs(8)


all_65plus <- bind_rows(wave8,
          wave9,
          wave10,
          wave11,
          wave12,
          wave13) %>% 
  mutate(biowgtr = case_when(wave == 13 & blversion == 1 ~ indsampleweight*2,
                             is.na(biowgtr) ~ indsampleweight*2,
                             TRUE ~ biowgtr),
         totalwordrecall = immediatewordrecall + delayedwordrecall) %>% 
  dplyr::filter(age>=65) %>% 
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
  dplyr::filter(!is.na(indsampleweight), indsampleweight > 0,diagnosed_dm == 1)




# ALL T2DM -------------
all_65plus_svy <- all_65plus  %>% 
  group_by(wave) %>% 
  mutate(normalizedweight = indsampleweight/sum(indsampleweight)) %>% 
  mutate(normalizedweight = normalizedweight/n()) %>% 
  ungroup() %>% 
  mutate(normalizedweight = normalizedweight*n()) %>% 
  dplyr::select(wave,psu,strata,normalizedweight,age,
                fall_any,iadl_any4,contains("correct"), valid_cognition,composite_cognition,composite_cognition_impairment) %>% 
  as_survey_design(ids = psu,
                   strata  = strata,
                   weight = normalizedweight,
                   nest = TRUE,
                   variance = "YG")

id_vars = "wave"
continuous_vars = c("age")
proportion_vars =c("fall_any","iadl_any4","composite_cognition_impairment") 

hrs_sy <- svysummary(all_65plus_svy,
                     c_vars = continuous_vars,
                     p_vars = proportion_vars,
                     # g_vars = grouped_vars,
                     id_vars = "wave"
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))


hrs_ct <- all_65plus_svy$variables %>% 
  group_by_at(vars(one_of(id_vars))) %>% 
  summarize_at(vars(one_of(c(
    continuous_vars,
    proportion_vars
    # grouped_vars
  ))),
  list(n = ~sum(!is.na(.)))) %>% 
  pivot_longer(names_to="variable",values_to="n",cols=-one_of(id_vars)) %>% 
  mutate(variable = str_replace(variable,"_n$",""))



hrs_sy %>% 
  left_join(hrs_ct,
            by=c("variable","wave")) %>% 
  write_csv(.,"gadrc proposal/g2afgp01_hrs estimates for all diabetes.csv")

all_65plus %>% 
  distinct(hhid,pn) %>% 
  nrow()


# BIOMARKER T2DM ----------

bio_65plus_svy <- all_65plus  %>% 
  mutate(biowgtr = case_when(
                             wave %in% c(13,14) & blversion %in% c(1,2) ~ indsampleweight*2,
                             is.na(biowgtr) ~ indsampleweight*2,
                             TRUE ~ biowgtr)) %>% 
  dplyr::filter(!is.na(blversion),biowgtr > 0) %>% 
  group_by(wave) %>% 
  mutate(normalizedweight = biowgtr/sum(biowgtr)) %>% 
  mutate(normalizedweight = normalizedweight/n()) %>% 
  ungroup() %>% 
  mutate(normalizedweight = normalizedweight*n()) %>% 
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
         ) %>% 
  
  dplyr::select(wave,psu,strata,normalizedweight,age,
                contains("control")) %>%
  as_survey_design(ids = psu,
                   strata  = strata,
                   weight = normalizedweight,
                   nest = TRUE,
                   variance = "YG")

proportion_vars_control = c("control_joint","control_a1c","control_bp","control_chol")


hrs_control_sy <- svysummary(bio_65plus_svy,
                     c_vars = continuous_vars,
                     p_vars = proportion_vars_control,
                     # g_vars = grouped_vars,
                     id_vars = "wave"
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))

hrs_control_ct <- bio_65plus_svy$variables %>% 
  group_by_at(vars(one_of(id_vars))) %>% 
  summarize_at(vars(one_of(c(
    continuous_vars,
    proportion_vars_control
    # grouped_vars
  ))),
  list(n = ~sum(!is.na(.)))) %>% 
  pivot_longer(names_to="variable",values_to="n",cols=-one_of(id_vars)) %>% 
  mutate(variable = str_replace(variable,"_n$",""))


hrs_control_sy %>% 
  left_join(hrs_control_ct,
            by=c("variable","wave")) %>% 
  write_csv(.,"gadrc proposal/g2afgp01_hrs estimates for control.csv")

all_65plus  %>% 
  mutate(biowgtr = case_when(
    wave %in% c(13,14) & blversion %in% c(1,2) ~ indsampleweight*2,
    is.na(biowgtr) ~ indsampleweight*2,
    TRUE ~ biowgtr)) %>% 
  dplyr::filter(!is.na(blversion),biowgtr > 0)  %>% 
  distinct(hhid,pn) %>% 
  nrow()

gc();rm(list=ls());source(".Rprofile")

library(srvyr)

source("C:/code/external/functions/survey/svysummary.R")

# DATA AGGREGATION ------------

prelim_elsa <- function(baseline_wave = 8){
  biomarkers <- readRDS(paste0(path_g2a_longitudinal_folder,"/working/elsa biomarkers.RDS")) %>% 
    mutate(wave = case_when(wavevisit == 9 ~ 9,
                            TRUE ~ wave))
  
  followup_vars <- c("fall_any","fall_injury","nfalls","fall_equip","fracture_hip",
                     "age",
                     "diagnosed_dm","iadl_some4","iadl_any4",
                     "immediatewordrecall","delayedwordrecall",
                     # Available in ELSA
                     "totalwordrecall",
                     "countbackwards","serial7subtract")
  
  baseline <- bind_rows(
    readRDS(paste0(path_g2a_falls_folder,"/working/elsa/G2A ELSA Falls Wave ",baseline_wave," male.RDS"))  %>% 
      dplyr::filter(sampleweight > 0),
    
    readRDS(paste0(path_g2a_falls_folder,"/working/elsa/G2A ELSA Falls Wave ",baseline_wave," female.RDS"))  %>% 
      dplyr::filter(sampleweight > 0)
    
  ) %>% 
    dplyr::select(personid,sampleweight,psu,strata,one_of(followup_vars)) 
  
  
  nurse_wave = case_when(baseline_wave == 9 ~ 8,TRUE ~ baseline_wave)
  
  nurse_vars <- bind_rows(
    readRDS(paste0(path_g2a_falls_folder,"/working/elsa/G2A ELSA Falls Wave ",nurse_wave," male.RDS"))  %>% 
      dplyr::filter(sampleweight > 0),
    
    readRDS(paste0(path_g2a_falls_folder,"/working/elsa/G2A ELSA Falls Wave ",nurse_wave," female.RDS"))  %>% 
      dplyr::filter(sampleweight > 0)
  ) %>% 
    dplyr::select(personid,one_of("sbp","dbp","bmi","weight","height"))
  
  analytic_df = left_join(baseline,
                          biomarkers %>% 
                            dplyr::filter(wave == baseline_wave),
                          by = c("personid")) %>% 
    left_join(nurse_vars,
              by = c("personid")) %>% 
    mutate(wave = baseline_wave)
  
  return(analytic_df)
  
}

wave9 <- prelim_elsa(9)
wave8 <- prelim_elsa(8)
wave6 <- prelim_elsa(6)
wave4 <- prelim_elsa(4)
wave2 <- prelim_elsa(2)


all_65plus <- bind_rows(wave2,
                        wave4,
                        wave6,
                        wave8,
                        wave9) %>% 
  dplyr::filter(age>=65) %>% 
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
  dplyr::filter(!is.na(sampleweight), sampleweight > 0,diagnosed_dm == 1)




# ALL T2DM -------------
continuous_vars = c("age")
proportion_vars =c("fall_any","iadl_any4","composite_cognition_impairment") 

all_65plus_svy <- all_65plus  %>% 
  group_by(wave) %>% 
  mutate(normalizedweight = sampleweight/sum(sampleweight)) %>% 
  mutate(normalizedweight = normalizedweight/n()) %>% 
  ungroup() %>% 
  mutate(normalizedweight = normalizedweight*n()) %>% 
  dplyr::select(wave,psu,strata,normalizedweight,age,
                fall_any,iadl_any4,contains("correct"), valid_cognition,composite_cognition,composite_cognition_impairment) %>% 
  mutate(across(one_of(c(continuous_vars,proportion_vars)),function(x) as.numeric(x))) %>% 
  as_survey_design(
                   ids = psu,
                   # strata  = strata,
                   weight = normalizedweight,
                   nest = FALSE,
                   variance = "YG")

id_vars = "wave"


elsa_sy <- svysummary(all_65plus_svy,
                     c_vars = continuous_vars,
                     p_vars = proportion_vars,
                     # g_vars = grouped_vars,
                     id_vars = "wave"
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))


elsa_ct <- all_65plus_svy$variables %>% 
  group_by_at(vars(one_of(id_vars))) %>% 
  summarize_at(vars(one_of(c(
    continuous_vars,
    proportion_vars
    # grouped_vars
  ))),
  list(n = ~sum(!is.na(.)))) %>% 
  pivot_longer(names_to="variable",values_to="n",cols=-one_of(id_vars)) %>% 
  mutate(variable = str_replace(variable,"_n$",""))



elsa_sy %>% 
  left_join(elsa_ct,
            by=c("variable","wave")) %>% 
  write_csv(.,"gadrc proposal/g2afgp02_elsa estimates for all diabetes.csv")

all_65plus %>% 
  distinct(personid) %>% 
  nrow()


# BIOMARKER T2DM ----------

bio_65plus_svy <- all_65plus  %>% 
  dplyr::filter(!is.na(bldwt),bldwt > 0) %>% 
  group_by(wave) %>% 
  mutate(normalizedweight = bldwt/sum(bldwt)) %>% 
  mutate(normalizedweight = normalizedweight/n()) %>% 
  ungroup() %>% 
  mutate(normalizedweight = normalizedweight*n()) %>% 
  mutate(
    # https://www.omnicalculator.com/health/cholesterol-units
    hdl = hdl*38.67,
    chol = chol*38.67,
    ldl = ldl*38.67,
    # https://heartcare.sydney/cholesterol-unit-conversion/
    trig = trig*88.57) %>% 
  mutate(control_a1c = case_when(hba1c < 2.0 | hba1c >= 20.0 ~ NA_real_,
                                 hba1c < 7.5 ~ 1,
                                 hba1c >= 7.5 ~ 0,
                                 TRUE ~ NA_real_),
         
         control_bp = case_when(sbp < 140 & dbp < 90 ~ 1,
                                sbp >= 140 | dbp >= 90 ~ 0,
                                TRUE ~ NA_real_),
         
         control_chol = case_when(ldl <= 100 ~ 1,
                                  ldl > 100 ~ 0,
                                  TRUE ~ NA_real_),
         
         control_joint = case_when(is.na(control_a1c) | is.na(control_bp) | is.na(control_chol) ~ NA_real_,
                                   control_a1c == 1 & control_bp == 1 & control_chol == 1 ~ 1,
                                   TRUE ~ 0)
  ) %>% 
  
  dplyr::select(wave,psu,strata,normalizedweight,age,
                contains("control")) %>%
  as_survey_design(
                   # ids = psu,
                   strata  = strata,
                   weight = normalizedweight,
                   nest = TRUE,
                   variance = "YG")

proportion_vars_control = c("control_joint","control_a1c","control_bp","control_chol")


elsa_control_sy <- svysummary(bio_65plus_svy,
                             c_vars = continuous_vars,
                             p_vars = proportion_vars_control,
                             # g_vars = grouped_vars,
                             id_vars = "wave"
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))

elsa_control_ct <- bio_65plus_svy$variables %>% 
  group_by_at(vars(one_of(id_vars))) %>% 
  summarize_at(vars(one_of(c(
    continuous_vars,
    proportion_vars_control
    # grouped_vars
  ))),
  list(n = ~sum(!is.na(.)))) %>% 
  pivot_longer(names_to="variable",values_to="n",cols=-one_of(id_vars)) %>% 
  mutate(variable = str_replace(variable,"_n$",""))


elsa_control_sy %>% 
  left_join(elsa_control_ct,
            by=c("variable","wave")) %>% 
  write_csv(.,"gadrc proposal/g2afgp02_elsa estimates for control.csv")

all_65plus  %>% 
  dplyr::filter(!is.na(bldwt),bldwt > 0)  %>% 
  distinct(personid) %>% 
  nrow()

gc();rm(list=ls());source(".Rprofile")

library(srvyr)

hrs_all_65plus_control_available <- readRDS(paste0(path_g2a_falls_folder,"/working/g2afh02_analytic dataset.RDS")) %>% 
  dplyr::filter(!is.na(control_joint)) %>% 
  mutate(followup = case_when(is.na(fu_fall_any) ~ 0,
                              TRUE ~ 1))

hrs_all_65plus <- hrs_all_65plus_control_available %>% 
    dplyr::filter(!is.na(fu_fall_any))

# Assessing counts ---------
hrs_all_65plus_control_available %>% 
  group_by(gender) %>%
  tally()

hrs_all_65plus_control_available %>% 
  distinct(hhidpn,gender) %>% 
  # group_by(gender) %>%
  tally()

hrs_all_65plus %>% 
  # group_by(gender) %>%
  tally()

hrs_all_65plus %>% 
  distinct(hhidpn,gender) %>% 
  # group_by(gender) %>%
  tally()

# Survey design ------------
hrs_all_65plus_diagnosed_svy <- hrs_all_65plus %>% 
  as_survey_design(ids = psu,
                   strata  = strata,
                   weight = normalizedweight,
                   nest = TRUE,
                   variance = "YG")

hrs_unique_df <- hrs_all_65plus %>% 
  group_by(hhidpn) %>% 
  dplyr::filter(wave == min(wave)) %>% 
  ungroup() 

hrs_unique_df %>% 
  group_by(gender) %>% 
  tally()

hrs_unique_svy <- hrs_unique_df %>% 
  as_survey_design(ids = psu,
                   strata  = strata,
                   weight = normalizedweight,
                   nest = TRUE,
                   variance = "YG")


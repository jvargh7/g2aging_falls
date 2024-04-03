gc();rm(list=ls());source(".Rprofile")

library(srvyr)

elsa_all_65plus_control_available <- readRDS(paste0(path_g2a_falls_folder,"/working/g2afe02_analytic dataset.RDS"))  %>% 
  dplyr::filter(!is.na(control_joint)) %>% 
  mutate(followup = case_when(is.na(fu_fall_any) ~ 0,
                              TRUE ~ 1))

elsa_all_65plus <- elsa_all_65plus_control_available %>% 
  dplyr::filter(!is.na(fu_fall_any))


# Assessing counts ---------
elsa_all_65plus_control_available %>% 
  # group_by(gender) %>%
  tally()

elsa_all_65plus_control_available %>% 
  distinct(personid,gender) %>% 
  # group_by(gender) %>%
  tally()

elsa_all_65plus %>% 
  # group_by(gender) %>%
  tally()

elsa_all_65plus %>% 
  distinct(personid,gender) %>% 
  # group_by(gender) %>%
  tally()

# Survey design ----------

elsa_all_65plus_diagnosed_svy <- elsa_all_65plus %>% 
  as_survey_design(
                   # ids = psu,
                   strata  = strata,
                   weight = normalizedweight,
                   nest = FALSE,
                   variance = "YG")


# elsa_biomarkers_diagnosed %>% 
#   dplyr::filter(!is.na(diagnosed_dm)) %>% 
#   group_by(wave,age_category) %>% 
#   dplyr::summarize(m = mean(fall_any,na.rm=TRUE),
#             n = n(),
#             na_count = sum(is.na(fall_any))) %>% 
#   View()

elsa_unique_df <- elsa_all_65plus %>% 
  group_by(personid) %>% 
  dplyr::filter(wave == min(wave)) %>% 
  ungroup() 

# elsa_unique_df %>% 
#   group_by(gender) %>% 
#   tally()

elsa_unique_svy <- elsa_unique_df %>% 
  as_survey_design(ids = psu,
                   strata  = strata,
                   weight = normalizedweight,
                   nest = TRUE,
                   variance = "YG")


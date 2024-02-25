# Based on g2aging_longitudinal/else/g2alelsa_individual data.R -

r_male <- haven::read_dta(paste0(path_g2a_data,"/elsa/h_elsa_g2.dta"),
                          col_select = na.omit(g2aelsa_r_variables$selected)) %>% 
  rename_with(.cols = g2aelsa_r_variables$selected,
              ~g2aelsa_r_variables$new_var[which(g2aelsa_r_variables$selected == .x)]) %>% 
  dplyr::filter(gender == 1)

r_female <- haven::read_dta(paste0(path_g2a_data,"/elsa/h_elsa_g2.dta"),
                            col_select = na.omit(g2aelsa_r_variables$selected)) %>% 
  rename_with(~ g2aelsa_r_variables$new_var[which(g2aelsa_r_variables$selected == .x)], 
              .cols = g2aelsa_r_variables$selected) %>% 
  dplyr::filter(gender == 2)

survey_vars <- c("strata","psu","hhid")
hh_vars <- c("hh_wealth","hh_income","hh_consumption","hh_size")

if("drinksperday" %in% g2aelsa_r_variables$new_var){
  r_male <- r_male %>% 
    mutate(drinksperweek = drinksperday*7)

  
  r_female <- r_female %>% 
    mutate(drinksperweek = drinksperday*7)

  
  
}


source("elsa/g2afelsa_preprocessing.R")


male <- r_male %>% mutate(type = "Respondent")  %>% 
  g2afelsa_preprocessing(.)  

female <- r_female %>% mutate(type = "Respondent")  %>% 
  g2afelsa_preprocessing(.) 

library(Hmisc)
hh_quintiles <- bind_rows(male,
                          female)  %>% 
  dplyr::filter(!is.na(sampleweight)) %>% 
  distinct(hhid,.keep_all=TRUE) %>% 
  dplyr::select(starts_with("hh"),sampleweight) %>% 
  mutate_at(vars(hh_income),
            function(x) cut(x,breaks=c(wtd.quantile(x,weights=.$sampleweight,probs = c(0,0.33,0.67,1.0))),right=TRUE,include.lowest=TRUE,
                            labels=c("Low","Medium","High"))) %>% 
  
  mutate_at(vars(hh_wealth,hh_consumption),
            function(x) cut(x,breaks=c(wtd.quantile(x,weights=.$sampleweight,probs = seq(0,1,by=0.2))),right=TRUE,include.lowest=TRUE,
                            labels=c("Lowest","Low","Medium","High","Highest"))) %>% 
  rename_at(vars(hh_wealth,hh_consumption),~paste0(.,"quintile")) %>% 
  rename(hh_incometertile = hh_income) 


male <- male %>% 
  dplyr::filter(!is.na(sampleweight)) %>% 
  left_join(hh_quintiles %>% 
              dplyr::select(hhid,contains("ile")),
            by="hhid")  %>% 
  mutate(psu = zoo::na.locf(psu)) %>% 
  # dplyr::select(hhid,h_personid,w_personid,psu,hhweight) %>% View() %>% 
  mutate(hh_lengthmar = case_when(!is.na(lengthmar) ~ lengthmar,
                                  TRUE ~ NA_real_),
         hh_lengthmar_ge10 = case_when(!is.na(lengthmar_ge10) ~ lengthmar_ge10,
                                       TRUE ~ NA_real_)) %>%
  mutate_at(vars(height),function(x) x*100) 

female <- female %>% 
  dplyr::filter(!is.na(sampleweight)) %>% 
  left_join(hh_quintiles %>% 
              dplyr::select(hhid,contains("ile")),
            by="hhid")  %>% 
  # MANUAL -----------
  mutate(psu = zoo::na.locf(psu,fromLast = TRUE)) %>% 
  # dplyr::select(hhid,h_personid,w_personid,psu,hhweight) %>% View() %>% 
  mutate(hh_lengthmar = case_when(!is.na(lengthmar) ~ lengthmar,
                                  TRUE ~ NA_real_),
         hh_lengthmar_ge10 = case_when(!is.na(lengthmar_ge10) ~ lengthmar_ge10,
                                       TRUE ~ NA_real_)) %>%
  mutate_at(vars(height),function(x) x*100) 


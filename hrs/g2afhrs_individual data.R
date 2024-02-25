g2a_respondents <- haven::read_dta(paste0(path_g2a_data,"/hrs/H_HRS_c.dta"),
                                   col_select = na.omit(g2ahrs_harmonized_r_variables$selected)) %>% 
  rename_with(~ g2ahrs_harmonized_r_variables$new_var[which(g2ahrs_harmonized_r_variables$selected == .x)], 
              .cols = g2ahrs_harmonized_r_variables$selected) %>% 
  mutate(pn = as.numeric(pn),
         hhid = paste0(hhid,"0"))


rand_respondents <- haven::read_dta(paste0(path_g2a_data,"/hrs/randhrs1992_2018v2.dta"),
                                    col_select = na.omit(g2ahrs_rand_r_variables$selected)) %>% 
  rename_with(~ g2ahrs_rand_r_variables$new_var[which(g2ahrs_rand_r_variables$selected == .x)], 
              .cols = g2ahrs_rand_r_variables$selected) %>% 
  mutate(hhid = str_sub(hhidpn,1,7),
         pn = as.numeric(pn),
         spouseidpn = case_when(spouseidpn == 0 | is.na(spouseidpn) ~ NA_character_,
                                TRUE ~ sprintf("%09d",spouseidpn))) %>% 
  mutate(spousepn = as.numeric(str_sub(spouseidpn,-2,-1))) %>% 
  dplyr::select(hhid,pn,hhidpn,spousepn,spouseidpn,everything())

respondents = left_join(rand_respondents,
                        g2a_respondents,
                        by = c("hhid","pn"))

survey_vars <- c("strata","psu","hhsampleweight","hhanalysisweight","pn","hhid","spousepn")
hh_vars <- c("hh_wealth","hh_income","hh_children","hh_size")

source("hrs/g2afhrs_preprocessing.R")

male <- respondents %>% dplyr::filter(gender == 1) %>% mutate(type = "Respondent") %>% 
  mutate(coupleid = paste0(hhid,pmin(pn,spousepn,na.rm = TRUE),"_",pmax(pn,spousepn,na.rm=TRUE))) %>% 
  g2afhrs_preprocessing(.)

female <- respondents %>% dplyr::filter(gender == 2) %>% mutate(type = "Respondent") %>% 
  mutate(coupleid = paste0(hhid,pmin(pn,spousepn,na.rm = TRUE),"_",pmax(pn,spousepn,na.rm=TRUE))) %>% 
  g2afhrs_preprocessing(.)

require(Hmisc)
hh_quintiles <- bind_rows(male,
                          female) %>% 
  distinct(hhid,.keep_all=TRUE) %>% 
  dplyr::select(starts_with("hh")) %>% 
  mutate_at(vars(hh_income),
            function(x) cut(x,breaks=c(wtd.quantile(x,weights=.$hhsampleweight,probs = c(0,0.33,0.67,1.0))),right=TRUE,include.lowest=TRUE,
                            labels=c("Low","Medium","High"))) %>% 
  
  mutate_at(vars(hh_wealth),
            function(x) cut(x,breaks=c(wtd.quantile(x,weights=.$hhsampleweight,probs = seq(0,1,by=0.2))),right=TRUE,include.lowest=TRUE,
                            labels=c("Lowest","Low","Medium","High","Highest"))) %>% 
  rename_at(vars(hh_wealth),~paste0(.,"quintile")) %>% 
  rename(hh_incometertile = hh_income)


male <- male %>% 
  left_join(hh_quintiles %>% 
              dplyr::select(hhid,contains("ile")),
            by="hhid") 

female <- female %>% 
  left_join(hh_quintiles %>% 
              dplyr::select(hhid,contains("ile")),
            by="hhid") 
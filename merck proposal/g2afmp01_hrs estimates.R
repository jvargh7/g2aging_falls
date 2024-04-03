gc();rm(list=ls());source(".Rprofile")

library(srvyr)

source("C:/code/external/functions/survey/svysummary.R")

# DATA AGGREGATION ------------

prelim_hrs_merck <- function(baseline_wave = 13){
  biomarkers <- readRDS(paste0(path_g2a_longitudinal_folder,"/working/hrs biomarkers.RDS")) 
  
  followup_vars <- c("fall_any","fall_injury","nfalls","fall_equip","fracture_hip",
                     "age","race",
                     "diagnosed_dm",
                     "medication_bp","medication_dm","rxhchol",
                     "rxstrok","rxangina","rxchf","rxhrtat","rxheart",
                     
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

wave14 <- prelim_hrs_merck(14)
wave13 <- prelim_hrs_merck(13)
wave12 <- prelim_hrs_merck(12)
wave11 <- prelim_hrs_merck(11)
wave10 <- prelim_hrs_merck(10)
wave9 <- prelim_hrs_merck(9)
wave8 <- prelim_hrs_merck(8)



all_50plus <- bind_rows(wave8,
                        wave9,
                        wave10,
                        wave11,
                        wave12,
                        wave13) %>% 
  mutate(race_black = case_when(race == 2 ~ 1,
                                TRUE ~ 0),
         race_hispanic = case_when(race == 3 ~ 1,
                                   TRUE ~ 0)) %>% 
  mutate(biowgtr = case_when(wave == 13 & blversion == 1 ~ indsampleweight*2,
                             is.na(biowgtr) ~ indsampleweight*2,
                             TRUE ~ biowgtr)) %>% 
  dplyr::filter(age>=50)  %>% 
  dplyr::filter(!is.na(indsampleweight), indsampleweight > 0,diagnosed_dm == 1) 


all_50plus_svy <- all_50plus  %>% 
  group_by(wave) %>% 
  mutate(normalizedweight = indsampleweight/sum(indsampleweight)) %>% 
  mutate(normalizedweight = normalizedweight/n()) %>% 
  ungroup() %>% 
  mutate(normalizedweight = normalizedweight*n()) %>% 
  as_survey_design(ids = psu,
                   strata  = strata,
                   weight = normalizedweight,
                   nest = TRUE,
                   variance = "YG")

id_vars = "wave"
continuous_vars = c("age")
proportion_vars =c("medication_bp","medication_dm","rxhchol",
                   "rxstrok","rxangina","rxchf","rxhrtat","rxheart",
                   "race_black","race_hispanic") 

hrs_sy <- svysummary(all_50plus_svy,
                     c_vars = continuous_vars,
                     p_vars = proportion_vars,
                     # g_vars = grouped_vars,
                     id_vars = "wave"
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))


hrs_ct <- all_50plus_svy$variables %>% 
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
  write_csv(.,"merck proposal/g2afmp01_hrs estimates of medication use for all diabetes.csv")

all_50plus %>% 
  distinct(hhid,pn) %>% 
  nrow()

# BIOMARKER T2DM ----------

bio_50plus_svy <- all_50plus  %>% 
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

continuous_vars = c("age")
id_vars = "wave"
proportion_vars_control = c("control_joint","control_a1c","control_bp","control_chol")


hrs_control_sy <- svysummary(bio_50plus_svy,
                             c_vars = continuous_vars,
                             p_vars = proportion_vars_control,
                             # g_vars = grouped_vars,
                             id_vars = id_vars
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))

hrs_control_ct <- bio_50plus_svy$variables %>% 
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
  write_csv(.,"merck proposal/g2afmp01_hrs estimates for control among adults 50 and older.csv")








# FIGURES -------------
hrs_meds_summary <-read_csv("merck proposal/g2afmp01_hrs estimates of medication use for all diabetes.csv") %>% 
  mutate(year = 1990 + wave*2)

hrs_control_summary <-read_csv("merck proposal/g2afmp01_hrs estimates for control among adults 50 and older.csv") %>% 
  mutate(year = 1990 + wave*2)

fig_meds = hrs_meds_summary %>% 
  dplyr::filter(variable %in% c("medication_bp","medication_dm","rxhchol")) %>% 
  mutate(variable_labelled = factor(variable,levels=c("medication_dm","medication_bp",
                                                      "rxhchol"),
                                    labels=c("Glucose \nLowering","Blood Pressure \nLowering",
                                             "Cholesterol \nLowering")))   %>% 
  ggplot(data = .,aes(x=year,color=variable_labelled,
                      group=interaction(variable_labelled),
                      y = estimate,ymin = lci,ymax=uci)) +
  geom_point() +
  geom_path() +
  # geom_errorbar() +
  theme_bw() +
  scale_color_manual("",values=c(
    rgb(114/255,148/255,212/255),
    rgb(242/255,173/255,0/255),
    rgb(90/255,188/255,214/255))) +
  scale_linetype_manual("",values=c(1,2)) +
  xlab("") +
  ylab("Percentage of adults \n50 and older taking meds (%)") +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 16),
        strip.text = element_text(size = 16))
  

fig_control = hrs_control_summary %>% 
  dplyr::filter(variable != "age") %>% 
  mutate(variable_labelled = factor(variable,levels=c("control_a1c","control_bp","control_chol","control_joint"),
                                    labels=c("HbA1c","Blood Pressure","Cholesterol","Joint ABC"))) %>% 
  ggplot(data = .,aes(x=year,color=variable_labelled,
                      group=interaction(variable_labelled),
                      y = estimate,ymin = lci,ymax=uci)) +
  geom_point() +
  geom_path() +
  # geom_errorbar() +
  theme_bw() +
  scale_color_manual("",values=c(
    rgb(114/255,148/255,212/255),
    rgb(242/255,173/255,0/255),
    rgb(90/255,188/255,214/255),
    "#FF6961")) +
  scale_linetype_manual("",values=c(1,2)) +
  xlab("") +
  ylab("Percentage of adults \n50 and older  in control (%)") +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 16),
        strip.text = element_text(size = 16))

# fig_control  %>% 
#   ggsave(.,filename=paste0(path_merck_ascvd_proposal,"/figures/rates of abc in adults.png"),width = 8,height =4)

library(ggpubr)
ggarrange(fig_meds,
          fig_control,
          labels=c("A","B"),
          nrow=2) %>% 
  ggsave(.,filename=paste0(path_merck_ascvd_proposal,"/figures/rates of treatment and control in adults.png"),width = 8,height =8)


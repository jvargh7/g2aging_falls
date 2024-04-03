gc();rm(list=ls());source(".Rprofile")

source("hrs/g2afhrs_diagnosed diabetes.R")
source("C:/code/external/functions/survey/svysummary.R")


# ALL T2DM -------------

id_vars_1 = c("wave")

continuous_vars = c("age")
proportion_vars =c("fu_fall_any","fall_any","iadl_any4","composite_cognition_impairment") 
proportion_vars_control = c("control_joint","control_a1c","control_bp","control_chol")

hrs_sy <- svysummary(hrs_all_65plus_diagnosed_svy,
                     c_vars = continuous_vars,
                     p_vars = c(proportion_vars,proportion_vars_control),
                     # g_vars = grouped_vars,
                     id_vars = id_vars_1
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))


hrs_ct <- hrs_all_65plus_diagnosed_svy$variables %>% 
  group_by_at(vars(one_of(id_vars_1))) %>% 
  summarize_at(vars(one_of(c(
    continuous_vars,
    proportion_vars
    # grouped_vars
  ))),
  list(n = ~sum(!is.na(.)))) %>% 
  pivot_longer(names_to="variable",values_to="n",cols=-one_of(id_vars_1)) %>% 
  mutate(variable = str_replace(variable,"_n$",""))



hrs_sy %>% 
  left_join(hrs_ct,
            by=c("variable","wave")) %>% 
  write_csv(.,"hrs/g2afh04_hrs estimates for all diabetes.csv")

hrs_all_65plus %>% 
  distinct(hhid,pn) %>% 
  nrow()

# BY SEX -----------
id_vars_2 = c("wave","gender")

hrs_sy_sex <- svysummary(hrs_all_65plus_diagnosed_svy,
                     c_vars = continuous_vars,
                     p_vars = c(proportion_vars,proportion_vars_control),
                     # g_vars = grouped_vars,
                     id_vars = id_vars_2
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))


hrs_ct_sex <- hrs_all_65plus_diagnosed_svy$variables %>% 
  group_by_at(vars(one_of(id_vars_2))) %>% 
  summarize_at(vars(one_of(c(
    continuous_vars,
    proportion_vars
    # grouped_vars
  ))),
  list(n = ~sum(!is.na(.)))) %>% 
  pivot_longer(names_to="variable",values_to="n",cols=-one_of(id_vars_2)) %>% 
  mutate(variable = str_replace(variable,"_n$",""))



hrs_sy_sex %>% 
  left_join(hrs_ct_sex,
            by=c("variable","wave","gender")) %>% 
  write_csv(.,"hrs/g2afh04_hrs estimates for all diabetes by sex.csv")

hrs_all_65plus %>% 
  distinct(hhid,pn) %>% 
  nrow()

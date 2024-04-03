gc();rm(list=ls());source(".Rprofile")
source("elsa/g2afelsa_diagnosed diabetes.R")
source("C:/code/external/functions/survey/svysummary.R")


# ALL T2DM -------------
continuous_vars = c("age")
proportion_vars =c("fu_fall_any","fall_any","iadl_any4","composite_cognition_impairment") 



id_vars = "wave"


elsa_sy <- svysummary(elsa_all_65plus_diagnosed_svy,
                      c_vars = continuous_vars,
                      p_vars = proportion_vars,
                      # g_vars = grouped_vars,
                      id_vars = "wave"
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))


elsa_ct <- elsa_all_65plus_diagnosed_svy$variables %>% 
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
  write_csv(.,"elsa/g2afe04_elsa estimates for all diabetes.csv")

elsa_all_65plus %>% 
  distinct(personid) %>% 
  nrow()


# BIOMARKER T2DM ----------
proportion_vars_control = c("control_joint","control_a1c","control_bp","control_chol")


elsa_control_sy <- svysummary(elsa_all_65plus_diagnosed_svy,
                              c_vars = continuous_vars,
                              p_vars = proportion_vars_control,
                              # g_vars = grouped_vars,
                              id_vars = "wave"
) %>%
  mutate_at(vars(estimate,lci,uci),~round(.,1)) %>%
  mutate(est_ci = paste0(estimate," (",
                         lci,", ",uci,")"))

elsa_control_ct <- elsa_all_65plus_diagnosed_svy$variables %>% 
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
  write_csv(.,"elsa/g2afe04_elsa estimates for control.csv")

elsa_all_65plus  %>% 
  dplyr::filter(!is.na(bldwt),bldwt > 0,wave %in% elsa_biomarker_waves)  %>% 
  distinct(personid) %>% 
  nrow()

gc();rm(list=ls());source(".Rprofile")

hrs_excluded <-   read_csv("hrs/g2afh08_excluded characteristics.csv") %>% 
  dplyr::select(variable,gender,group,est_ci) %>% 
  pivot_wider(names_from=gender,values_from=est_ci) %>% 
  rename(HRS_Female = Female,
         HRS_Total = Total,
         HRS_Male = Male) %>% 
  mutate(variable = case_when(variable == "a1c_adj" ~ "hba1c",
                              variable == "hdl_adj" ~ "hdl",
                              variable == "tc_adj" ~ "chol",
                              TRUE ~ variable))

elsa_excluded <-   read_csv("elsa/g2afe08_excluded characteristics.csv")  %>% 
  dplyr::select(variable,gender,group,est_ci) %>% 
  pivot_wider(names_from=gender,values_from=est_ci) %>% 
  rename(ELSA_Female = Female,
         ELSA_Total = Total,
         ELSA_Male = Male) %>% 
  mutate(variable = case_when(
    variable == "children" ~ "hh_children",
    TRUE ~ variable))


full_join(hrs_excluded,
          elsa_excluded,
          by = c("variable","group")) %>% 
  dplyr::select(variable,group,HRS_Total,HRS_Male,HRS_Female,ELSA_Total,ELSA_Male,ELSA_Female) %>% 
  write_csv(.,"paper/table_excluded characteristics.csv")



gc();rm(list=ls());source(".Rprofile")


elsa_coef <- read_csv("elsa/g2afe06_coefficients for sensitivity poisson regression.csv") %>% 
  dplyr::filter(model %in% c("M1","A1","B1","C1","L1","J1")) %>% 
  dplyr::filter(str_detect(iv,"control")) %>% 
  rename(RR = OR) %>% 
  mutate(iv = case_when(iv == "control_joint" ~ "Overall",
                        iv == "control_a1c" ~ "HbA1c < 7%",
                        iv == "control_bp" ~ "BP < 140/90 mmHg",
                        iv == "control_chol" ~ "Cholesterol < 200 mg/dL",
                        iv == "control_ldl" ~ "LDLc < 100 mg/dL",
                        iv == "control_joint2" ~ "Joint ABC 2"))

hrs_coef <- read_csv("hrs/g2afh06_coefficients for sensitivity poisson regression.csv") %>% 
  dplyr::filter(model %in% c("M1","A1","B1","C1","L1","J1")) %>% 
  dplyr::filter(str_detect(iv,"control")) %>% 
  rename(RR = OR) %>% 
  mutate(iv = case_when(iv == "control_joint" ~ "Overall",
                        iv == "control_a1c" ~ "HbA1c < 7%",
                        iv == "control_bp" ~ "BP < 140/90 mmHg",
                        iv == "control_chol" ~ "Cholesterol < 200 mg/dL",
                        iv == "control_ldl" ~ "LDLc < 100 mg/dL",
                        iv == "control_joint2" ~ "Joint ABC 2"))

elsa_emm <- read_csv("elsa/g2afe06_contrasts for sensitivity poisson regression with multiple imputation.csv") %>% 
  dplyr::filter(iv != "Contrast 3") %>% 
  mutate(iv = case_when(iv == "Contrast 1" & model == "M2" ~ "Age < 75",
                        iv == "Contrast 2" & model == "M2" ~ "Age >= 75",
                        iv == "Contrast 1" & model == "M3" ~ "Male",
                        iv == "Contrast 2" & model == "M3" ~ "Female",
                        iv == "Contrast 1" & model == "M4" ~ "No Balance Impairment",
                        iv == "Contrast 2" & model == "M4" ~ "Balance Impairment",
                        iv == "Contrast 1" & model == "M5" ~ "No History of Falls",
                        iv == "Contrast 2" & model == "M5" ~ "History of Falls",
                        TRUE ~ NA_character_
  ))


hrs_emm <- read_csv("hrs/g2afh06_contrasts for sensitivity poisson regression with multiple imputation.csv") %>% 
  dplyr::filter(iv != "Contrast 3") %>% 
  mutate(iv = case_when(iv == "Contrast 1" & model == "M2" ~ "Age < 75",
                        iv == "Contrast 2" & model == "M2" ~ "Age >= 75",
                        iv == "Contrast 1" & model == "M3" ~ "Male",
                        iv == "Contrast 2" & model == "M3" ~ "Female",
                        iv == "Contrast 1" & model == "M4" ~ "No Balance Impairment",
                        iv == "Contrast 2" & model == "M4" ~ "Balance Impairment",
                        iv == "Contrast 1" & model == "M5" ~ "No History of Falls",
                        iv == "Contrast 2" & model == "M5" ~ "History of Falls",
                        TRUE ~ NA_character_
  ))


table_df = bind_rows(hrs_coef %>% dplyr::select(iv,model,RR,lci,uci) %>% mutate(country = "USA"),
                     elsa_coef %>% dplyr::select(iv,model,RR,lci,uci) %>% mutate(country = "England"),
                     hrs_emm %>% dplyr::select(iv,model,RR,lci,uci) %>% mutate(country = "USA"),
                     elsa_emm %>% dplyr::select(iv,model,RR,lci,uci) %>% mutate(country = "England")
) %>% 
  mutate(term = factor(iv,
                       levels=c("Overall",
                                "Age < 75",
                                "Age >= 75",
                                "Female",
                                "Male",
                                "Balance Impairment",
                                "No Balance Impairment",
                                "History of Falls",
                                "No History of Falls",
                                "HbA1c < 7%",
                                "BP < 140/90 mmHg",
                                "Cholesterol < 200 mg/dL",
                                "LDLc < 100 mg/dL",
                                "Joint ABC 2"),
                       ordered = TRUE)) 


table_df %>% 
  dplyr::select(term,country,RR) %>% 
  pivot_wider(names_from = "country",values_from="RR") %>% 
  write_csv(.,"paper/table_sensitivity risk of falls by effect modifier.csv")




gc();rm(list=ls());source(".Rprofile")


elsa_coef <- read_csv("elsa/g2afe05_coefficients for poisson regression.csv") %>% 
  dplyr::filter(model %in% c("M1"),
                iv %in% c("age","balance_impairment",
                          "fall_any","female","iadl_some4",
                          "vision_impairment","rxdiabi","polypharmacy")) 

hrs_coef <- read_csv("hrs/g2afh05_coefficients for poisson regression.csv") %>% 
  dplyr::filter(model %in% c("M1"),
                iv %in% c("age","balance_impairment",
                          "fall_any","female","iadl_some4",
                          "vision_impairment","rxdiabi","polypharmacy"))

bind_rows(elsa_coef %>% mutate(country = "England"),
          hrs_coef %>% mutate(country = "USA")) %>% 
  mutate(term = factor(iv,
                       levels = c("age","balance_impairment",
                                  "fall_any","female","iadl_some4",
                                  "vision_impairment","rxdiabi","polypharmacy"))) %>%
  rename(RR = OR) %>% 
  dplyr::select(term,country,RR) %>% 
  pivot_wider(names_from = "country",values_from="RR") %>% 
  dplyr::select(term,USA,England) %>% 
  write_csv(.,"paper/table_association of known risk factors.csv")

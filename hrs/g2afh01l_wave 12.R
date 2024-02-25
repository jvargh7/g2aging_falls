gc();rm(list=ls());source(".Rprofile")

g2ahrs_harmonized_r_variables <- readxl::read_excel("hrs/G2A HRS Falls Variable List.xlsx",sheet="wave12") %>% 
  rename("selected" = harmonized_r) %>% 
  dplyr::select(level,new_var,selected) %>% 
  dplyr::filter(!is.na(selected)) %>% 
  mutate(selected = str_to_lower(selected))


g2ahrs_rand_r_variables <- readxl::read_excel("hrs/G2A HRS Falls Variable List.xlsx",sheet="wave12") %>% 
  rename("selected" = rand_r) %>% 
  dplyr::select(level,new_var,selected) %>% 
  dplyr::filter(!is.na(selected)) %>% 
  mutate(selected = str_to_lower(selected))



source("hrs/g2afhrs_individual data.R")


saveRDS(male,paste0(path_g2a_falls_folder,"/working/hrs/G2A HRS Falls Wave 12 male.RDS"))  
saveRDS(female,paste0(path_g2a_falls_folder,"/working/hrs/G2A HRS Falls Wave 12 female.RDS"))  

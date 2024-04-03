gc();rm(list=ls());source(".Rprofile")

h_elsa_g2 <- haven::read_dta(paste0(path_g2a_data,"/elsa/h_elsa_g2.dta"),
                             col_select = c("h2coupid","idauniq","s5idauniq","ragender","s5gender","r1strat","r5strat"))


g2aelsa_r_variables <- readxl::read_excel("elsa/G2A ELSA Falls Variable List.xlsx",sheet="wave5") %>% 
  rename("selected" = harmonized_r) %>% 
  dplyr::select(level,new_var,selected) %>% 
  dplyr::filter(!is.na(selected))


source("elsa/g2afelsa_individual data.R")

saveRDS(male,paste0(path_g2a_falls_folder,"/working/elsa/G2A ELSA Falls Wave 5 male.RDS"))  
saveRDS(female,paste0(path_g2a_falls_folder,"/working/elsa/G2A ELSA Falls Wave 5 female.RDS"))  

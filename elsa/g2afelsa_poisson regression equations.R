
ind_covariates <- c("+ age + female + laborforce + iadl_some4 + fall_any + moderate_pa + vigorous_pa + polypharmacy + rxdiabi + 
                    balance_impairment + vision_impairment + heavydrinker + bmi + htn + race + smoke")

hh_covariates <- c("+ hh_size + hh_wealthquintile + wave")

auxiliary_covariates <- c("+ adl_some6 + hba1c + sbp + dbp + trig + hdl + ldl + chol + nfalls + 
                          lengthmar + fall_injury + fracture_hip")

m0 <- paste0("fu_fall_any ~ control_joint") %>% as.formula()

m1 <- paste0("fu_fall_any ~ control_joint",ind_covariates,hh_covariates) %>% as.formula()

m2 <- paste0("fu_fall_any ~ control_joint*ge75",ind_covariates,hh_covariates) %>% str_replace(.,"\\+ age ","") %>% as.formula()

m3 <- paste0("fu_fall_any ~ control_joint*female",ind_covariates,hh_covariates) %>% str_replace(.,"\\+ female ","") %>% as.formula()

m4 <- paste0("fu_fall_any ~ control_joint*balance_impairment",ind_covariates,hh_covariates) %>% str_replace(.,"\\+ balance_impairment ","") %>% as.formula()

m5 <- paste0("fu_fall_any ~ control_joint*fall_any",ind_covariates,hh_covariates) %>% str_replace(.,"\\+ fall_any ","") %>% as.formula()

m6 <- paste0("fu_fall_any ~ control_joint*rxdiabi",ind_covariates,hh_covariates) %>% str_replace(.,"\\+ rxdiabi ","") %>% as.formula()

ltfu1 <- paste0("followup ~ control_joint", ind_covariates,hh_covariates,auxiliary_covariates) %>% as.formula()

a1 <- paste0("fu_fall_any ~ control_a1c",ind_covariates,hh_covariates) %>% as.formula()
b1 <- paste0("fu_fall_any ~ control_bp",ind_covariates,hh_covariates) %>% as.formula()
c1 <- paste0("fu_fall_any ~ control_chol",ind_covariates,hh_covariates) %>% as.formula()
a3 <- paste0("fu_fall_any ~ control_a1c*female",ind_covariates,hh_covariates) %>% str_replace(.,"\\+ female ","") %>% as.formula()
b3 <- paste0("fu_fall_any ~ control_bp*female",ind_covariates,hh_covariates) %>% str_replace(.,"\\+ female ","") %>% as.formula()
c3 <- paste0("fu_fall_any ~ control_chol*female",ind_covariates,hh_covariates) %>% str_replace(.,"\\+ female ","") %>% as.formula()

l1 <- paste0("fu_fall_any ~ control_ldl",ind_covariates,hh_covariates) %>% as.formula()
l3 <- paste0("fu_fall_any ~ control_ldl*female",ind_covariates,hh_covariates) %>% str_replace(.,"\\+ female ","") %>% as.formula()
j1 <- paste0("fu_fall_any ~ control_joint2",ind_covariates,hh_covariates) %>% as.formula()
j3 <- paste0("fu_fall_any ~ control_joint2*female",ind_covariates,hh_covariates) %>% str_replace(.,"\\+ female ","") %>% as.formula()
# Lists for models --------

overall_m0 = list()
overall_m1 = list()
overall_m2 = list()
overall_m3 = list()
overall_m4 = list()
overall_m5 = list()
overall_m6 = list()

overall_a1 = list()
overall_b1 = list()
overall_c1 = list()
overall_a3 = list()
overall_b3 = list()
overall_c3 = list()


overall_l1 = list()
overall_l3 = list()
overall_j1 = list()
overall_j3 = list()
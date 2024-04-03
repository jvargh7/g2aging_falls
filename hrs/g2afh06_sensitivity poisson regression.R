gc();rm(list=ls());source(".Rprofile")

mi_dfs <- readRDS(paste0(path_g2a_falls_folder,"/working/g2afhrs_sensitivity multiple imputation.RDS"))

library(mice)
library(geepack)

source("hrs/g2afhrs_poisson regression equations.R")

for(i in 1:mi_dfs$m){
  df = complete(mi_dfs,action = i) %>% 
    dplyr::select(-hhsampleweight,-spouseidpn,-spousepn) %>% 
    arrange(hhidpn,wave) %>% 
    mutate(raceeth = case_when(is.na(raceeth) ~ "NH White",
                               TRUE ~ raceeth));
  
  ipcw_mod <- geeglm(formula = ltfu1,
                corstr = "exchangeable",
                family=poisson(),
                data = df,
                weights = normalizedweight,
                id = hhidpn) 
  
  df$prob_followup = predict(ipcw_mod,newdata = df,type = "response")
  
  df_restricted <- df %>% 
    dplyr::filter(followup == 1) %>% 
    mutate(ipcw_normalizedweight = normalizedweight/prob_followup)
  
  overall_m0[[i]] = geeglm(formula = m0,
                           corstr = "exchangeable",
                           family=poisson(),
                           data = df_restricted,
                           weights = ipcw_normalizedweight,
                           id = hhidpn);
  
  overall_m1[[i]] = geeglm(formula = m1,
                           corstr = "exchangeable",
                           family=poisson(),
                           data = df_restricted,
                           weights = ipcw_normalizedweight,
                           id = hhidpn);
  
  overall_m2[[i]] = geeglm(formula = m2,
                           corstr = "exchangeable",
                           family=poisson(),
                           data = df_restricted,
                           weights = ipcw_normalizedweight,
                           id = hhidpn);
  overall_m3[[i]] = geeglm(formula = m3,
                           corstr = "exchangeable",
                           family=poisson(),
                           data = df_restricted,
                           weights = ipcw_normalizedweight,
                           id = hhidpn);
  overall_m4[[i]] = geeglm(formula = m4,
                           corstr = "exchangeable",
                           family=poisson(),
                           data = df_restricted,
                           weights = ipcw_normalizedweight,
                           id = hhidpn);
  overall_m5[[i]] = geeglm(formula = m5,
                           corstr = "exchangeable",
                           family=poisson(),
                           data = df_restricted,
                           weights = ipcw_normalizedweight,
                           id = hhidpn);
  
  overall_a1[[i]] = geeglm(formula = a1,
                           corstr = "exchangeable",
                           family=poisson(),
                           data = df_restricted,
                           weights = ipcw_normalizedweight,
                           id = hhidpn);
  overall_b1[[i]] = geeglm(formula = b1,
                           corstr = "exchangeable",
                           family=poisson(),
                           data = df_restricted,
                           weights = ipcw_normalizedweight,
                           id = hhidpn);
  overall_c1[[i]] = geeglm(formula = c1,
                           corstr = "exchangeable",
                           family=poisson(),
                           data = df_restricted,
                           weights = ipcw_normalizedweight,
                           id = hhidpn);
  
  gc();rm(df_restricted);rm(ipcw_mod)
}

# Please download the latest version ----

# https://github.com/jvargh7/functions/blob/main/imputation/adjusted_ci.R
# download.file("https://github.com/jvargh7/functions/blob/main/imputation/adjusted_ci.R",destfile = "")
source("C:/code/external/functions/imputation/adjusted_ci.R")
# https://github.com/jvargh7/functions/blob/main/imputation/clean_mi_conditionalregression.R
source("C:/code/external/functions/imputation/clean_mi_conditionalregression.R")

overall_m0_out = clean_mi_conditionalregression(overall_m0,link="geeglm log")
overall_m1_out = clean_mi_conditionalregression(overall_m1,link="geeglm log")
overall_m2_out = clean_mi_conditionalregression(overall_m2,link="geeglm log")
overall_m3_out = clean_mi_conditionalregression(overall_m3,link="geeglm log")
overall_m4_out = clean_mi_conditionalregression(overall_m4,link="geeglm log")
overall_m5_out = clean_mi_conditionalregression(overall_m5,link="geeglm log")
overall_a1_out = clean_mi_conditionalregression(overall_a1,link="geeglm log")
overall_b1_out = clean_mi_conditionalregression(overall_b1,link="geeglm log")
overall_c1_out = clean_mi_conditionalregression(overall_c1,link="geeglm log")


bind_rows(
  overall_m0_out %>% mutate(model = "M0"),
  overall_m1_out %>% mutate(model = "M1"),
  overall_m2_out %>% mutate(model = "M2"),
  overall_m3_out %>% mutate(model = "M3"),
  overall_m4_out %>% mutate(model = "M4"),
  overall_m5_out %>% mutate(model = "M5"),
  
  overall_a1_out %>% mutate(model = "A1"),
  overall_b1_out %>% mutate(model = "B1"),
  overall_c1_out %>% mutate(model = "C1")) %>% 
  write_csv(.,"hrs/g2afh06_coefficients for sensitivity poisson regression.csv")


# Contrasts ----------

source("C:/code/external/functions/imputation/clean_mi_contrasts.R")

# The coefficients are in column 'RR'. The rest of the columns are for pooling multiple imputated regressions

contrasts_m2_out = clean_mi_contrasts(model_list = overall_m2,link="geeglm log",modifier = "ge75",exposure = "control_joint")
contrasts_m3_out = clean_mi_contrasts(model_list = overall_m3,link="geeglm log",modifier = "female",exposure = "control_joint")
contrasts_m4_out = clean_mi_contrasts(model_list = overall_m4,link="geeglm log",modifier = "balance_impairment",exposure = "control_joint")
contrasts_m5_out = clean_mi_contrasts(model_list = overall_m5,link="geeglm log",modifier = "fall_any",exposure = "control_joint")



bind_rows(
  contrasts_m2_out %>% mutate(model = "M2"),
  contrasts_m3_out %>% mutate(model = "M3"),
  contrasts_m4_out %>% mutate(model = "M4"),
  contrasts_m5_out %>% mutate(model = "M5")
  
) %>% 
  write_csv(.,"hrs/g2afh06_contrasts for sensitivity poisson regression with multiple imputation.csv")

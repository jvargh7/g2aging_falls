gc();rm(list=ls());source(".Rprofile")

proportion_vars =c("fu_fall_any","fall_any","iadl_any4","composite_cognition_impairment") 
proportion_vars_control = c("control_joint","control_a1c","control_bp","control_chol")

hrs_all <- read_csv("hrs/g2afh04_hrs estimates for all diabetes.csv") %>% 
  dplyr::filter(variable == "fall_any" |
                  (wave == 13 & variable == "fu_fall_any")) %>% 
  # Add 1
  mutate(wave = case_when(variable == "fu_fall_any" ~ wave + 1,
                          TRUE ~ wave)) %>% 
  mutate(variable = "fall_any") %>% 
  mutate(year = 1990 + wave*2 + 0.5)

hrs_sex <- read_csv("hrs/g2afh04_hrs estimates for all diabetes by sex.csv") %>% 
  dplyr::filter(variable == "fall_any" |
                  (wave == 13 & variable == "fu_fall_any")) %>% 
  # Add 1
  mutate(wave = case_when(variable == "fu_fall_any" ~ wave + 1,
                          TRUE ~ wave)) %>% 
  mutate(variable = "fall_any") %>% 
  mutate(year = 1990 + wave*2 + 0.5)

hrs_control <-read_csv("hrs/g2afh04_hrs estimates for all diabetes.csv") %>% 
  dplyr::filter(variable %in% proportion_vars_control) %>% 
  mutate(year = 1990 + wave*2 + 0.5)

hrs_control_sex <-read_csv("hrs/g2afh04_hrs estimates for all diabetes by sex.csv") %>% 
  dplyr::filter(variable %in% proportion_vars_control) %>% 
  mutate(year = 1990 + wave*2 + 0.5)

# Falls ------------

df_falls <- bind_rows(hrs_all %>% mutate(gender = "Total"),
                      hrs_sex)

fig_fall = df_falls  %>% 
  mutate(gender = factor(gender,levels=c("Total","Male","Female"))) %>% 
  arrange(gender,year) %>% 
  ggplot(data = .,aes(x=year,color=gender,group=gender,y = estimate,ymin = lci,ymax=uci)) +
  geom_point() +
  geom_path() +
  geom_errorbar(width = 0.1) +
  theme_bw() +
  scale_color_manual("",values=c(rgb(1/255,160/255,138/255),
                                 rgb(114/255,148/255,212/255),
                                 rgb(242/255,173/255,0/255))) +
  xlab("") +
  ylab("Percentage of adults \n65 and older (%)") +
  theme(legend.position = "bottom") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12)) +
  scale_y_continuous(limits=c(0,100),breaks = seq(0,100,by=20)) + 
  scale_x_continuous(limits=c(2006,2019),breaks = seq(2006,2018,by=2)) 


# Control - Total ----------
df_control <- bind_rows(hrs_control %>% mutate(gender = "Total"),
                        hrs_control_sex) %>% 
  mutate(variable_labelled = factor(variable,levels=c("control_a1c","control_bp","control_chol","control_joint"),
                                    labels=c("HbA1c","Blood Pressure","Cholesterol","Joint ABC")),
         gender = factor(gender,levels=c("Total","Male","Female"))) 
  
fig_control_Total = df_control  %>%
  dplyr::filter(gender == "Total") %>% 
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
    rgb(1/255,160/255,138/255),
    "#FF6961")) +
  # scale_linetype_manual("",values=c(1,3)) +
  xlab("") +
  ylab("Percentage of adults \n65 and older  in control (%)") +
  theme(legend.position = "bottom") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12)) +
  scale_y_continuous(limits=c(0,100),breaks = seq(0,100,by=20)) + 
  scale_x_continuous(limits=c(2004,2017),breaks = seq(2004,2016,by=4)) 

fig_control_Male = df_control  %>%
  dplyr::filter(gender == "Male") %>% 
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
    rgb(1/255,160/255,138/255),
    "#FF6961")) +
  # scale_linetype_manual("",values=c(1,3)) +
  xlab("") +
  ylab("Percentage of adults \n65 and older  in control (%)") +
  theme(legend.position = "bottom") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12)) +
  scale_y_continuous(limits=c(0,100),breaks = seq(0,100,by=20)) + 
  scale_x_continuous(limits=c(2004,2017),breaks = seq(2004,2016,by=4)) 


fig_control_Female = df_control  %>%
  dplyr::filter(gender == "Female") %>% 
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
    rgb(1/255,160/255,138/255),
    "#FF6961")) +
  # scale_linetype_manual("",values=c(1,3)) +
  xlab("") +
  ylab("Percentage of adults \n65 and older  in control (%)") +
  theme(legend.position = "bottom") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12)) +
  scale_y_continuous(limits=c(0,100),breaks = seq(0,100,by=20)) + 
  scale_x_continuous(limits=c(2004,2017),breaks = seq(2004,2016,by=4)) 

library(ggpubr)

ggarrange(fig_fall,
          ggarrange(fig_control_Total,
                    fig_control_Male,
                    fig_control_Female,
                    labels=c("B","C","D"),
                    nrow = 1,ncol=3,common.legend = TRUE,legend = "bottom"),
          labels=c("A",""),
          nrow=2) %>% 
  ggsave(.,filename=paste0(path_g2a_falls_folder,"/figures/rates of falls and diabetes control in adults.png"),width = 8,height =6)


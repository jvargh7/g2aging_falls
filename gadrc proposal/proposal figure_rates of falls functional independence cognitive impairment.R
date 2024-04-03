gc();rm(list=ls());source(".Rprofile")

hrs_all <- read_csv("gadrc proposal/g2afgp01_hrs estimates for all diabetes.csv") %>% 
  mutate(year = 1990 + wave*2)


hrs_control <-read_csv("gadrc proposal/g2afgp01_hrs estimates for control.csv") %>% 
  mutate(year = 1990 + wave*2)



fig_all = hrs_all %>% 
  dplyr::filter(!variable %in% c("age")) %>% 
  mutate(variable_labelled = factor(variable,levels=c("fall_any","iadl_any4","composite_cognition_impairment"),
                                    labels=c("Any Fall","Some Functional Limitations","Potential Cognitive \nImpairment"))) %>% 
  ggplot(data = .,aes(x=year,y = estimate, color = variable_labelled, group=variable_labelled,ymin = lci,ymax=uci)) +
  geom_point() +
  geom_path() +
  # geom_errorbar() +
  # facet_grid(~variable_labelled) +
  theme_bw() +
  scale_color_manual(name = "",values = c("#FF6961",
                     rgb(242/255,173/255,0/255),
                     rgb(90/255,188/255,214/255))) +
  xlab("") +
  ylab("Percentage of adults \n65 and older (%)") +
  theme(legend.position = "bottom") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-20,10,10,10))


fig_control = hrs_control %>% 
  dplyr::filter(variable != "age") %>% 
  mutate(variable_labelled = factor(variable,levels=c("control_a1c","control_bp","control_chol","control_joint"),
                                    labels=c("HbA1c","Blood Pressure","Cholesterol","Joint ABC"))) %>% 
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
    rgb(90/255,188/255,214/255),
    "#FF6961")) +
  # scale_linetype_manual("",values=c(1,2)) +
  xlab("") +
  ylab("Percentage of adults \n65 and older  in control (%)") +
  theme(legend.position = "bottom") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-20,10,10,10))

library(ggpubr)

ggarrange(fig_all,
          fig_control,
          labels=c("A","B"),
          nrow=2) %>% 
  ggsave(.,filename=paste0(path_g2a_falls_proposal,"/figures/rates of limitations in adults.png"),width = 8,height =6)

fig_all %>% 
  ggsave(.,filename=paste0(path_g2a_falls_proposal,"/figures/rates of falls in adults.png"),width = 6,height =3)

fig_control %>% 
  ggsave(.,filename=paste0(path_g2a_falls_proposal,"/figures/rates of control in adults.png"),width = 6,height =3)


gc();rm(list=ls());source(".Rprofile")

# Alabama: 67
# Arkansas: 75
# Georgia: 134
# Indiana: 92
# Kentucky: 120
# Louisiana: 64
# Mississippi: 82
# North Carolina: 100
# South Carolina: 46
# Tennessee: 95
# Texas: 254
# Virginia: 134


library(usmap)
merged_cosmos <- read_csv(paste0(path_merck_ascvd_proposal,"/working/merged_cosmos.csv")) %>% 
  distinct(county,counts,state,.keep_all=TRUE) %>% 
  mutate(county = str_extract(county,"^([A-Z]|\\s)+")) %>%
  mutate(county = case_when(state == "Louisiana" ~ paste0(county," PARISH"),
                            TRUE ~ county))  %>%
  mutate(county_fips = map2(.x=state,.y=county,.f = function(x,y) code = tryCatch({fips(state = x,county=y)},error=function(e){""})) %>% 
           as.character()) 

county_boundaries <- tigris::counties(class="sf",cb=TRUE) %>%
  left_join(merged_cosmos %>% 
              dplyr::filter(county_fips != "") %>% 
              dplyr::select(county_fips,counts) %>% 
              mutate(counts2 = case_when(counts == "10 or fewer" ~ 1,
                                         TRUE ~ as.numeric(counts))) %>% 
              mutate(counts_category = case_when(counts == "10 or fewer" ~ 1,
                                                 counts2 %in% c(11:100) ~ 2,
                                                 counts2 %in% c(101:1000) ~ 3,
                                                 counts2 > 1000 ~ 4,
                                        TRUE ~ NA_real_)) %>% 
              mutate(counts_category = factor(counts_category,levels=c(1:4),labels=c("10 or fewer","11 to 100","101 to 1000","1001 or more"))),
            by=c("GEOID"="county_fips")) %>% 
  tigris::shift_geometry()  %>% 
  dplyr::filter(STATEFP < 60) 

state_boundaries <- tigris::states(class = "sf", cb = TRUE) %>% 
  tigris::shift_geometry() %>% 
  dplyr::filter(GEOID < 60) 

options(scipen=999)

figB <- ggplot() +
  geom_sf(data=county_boundaries,aes(fill = counts_category),col=NA)  +
  geom_sf(data=state_boundaries,col="black",fill=NA)  +
  # https://stackoverflow.com/questions/66031935/ggplot2-and-sf-geom-sf-text-within-limits-set-by-coord-sf
  # coord_sf(xlim = c(-140,-60), ylim = c(22,50)) +
  coord_sf(crs = 5070, datum = NA) +
  theme_bw() +
  xlab("") +
  ylab("") +
  scale_fill_manual(name="",values=c("10 or fewer" = "#FF6961",
                                     "11 to 100" = rgb(114/255,148/255,212/255),
                                     "101 to 1000" = rgb(90/255,188/255,214/255),
                                     "1001 or more" = "#027324"),na.value="grey90") +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 14))

figB

ggsave(figB,filename=paste0(path_merck_ascvd_proposal,"/figures/counts in cosmos stroke belt.jpg"),height = 4,width=4*1.685)

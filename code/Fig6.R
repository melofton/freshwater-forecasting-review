#Fig. 5
#Author: Mary Lofton
#Date: last updated 30AUG22

#clear environment
rm(list = ls())

#set-up
pacman::p_load(tidyverse, lubridate, cowplot,ggbeeswarm, viridis)

#read in data 
dat5 <- read_csv("./data/cleaned_matrix.csv")

#Fig. 5
dat6 <- dat5 %>%
  mutate(ecosystem_type = ifelse(ecosystem == "river" | grepl("basin",other_ecosystem),"(b) Lotic","(a) Lentic"),
         horizon_scale = ifelse(max_horizon_days<7,"days (< 7 days)",
                                ifelse(max_horizon_days<30,"weeks (7-30 days)",
                                       ifelse(max_horizon_days<365,"months (30-365 days)",
                                              ifelse(max_horizon_days<3650,"years (365-3650 days)","decadal (>3650 days)")))),
         horizon_scale = factor(horizon_scale, levels = c("days (< 7 days)","weeks (7-30 days)","months (30-365 days)","years (365-3650 days)","decadal (>3650 days)")))

Fig6 <- ggplot(data = dat6, aes(x = unsuitable, y = horizon_scale, group = phys_chem_bio, shape = phys_chem_bio, color = phys_chem_bio))+
  geom_beeswarm(priority='random',cex=18, groupOnX=T,size = 3)+
  facet_grid(cols = vars(ecosystem_type),switch = "x")+
  theme_bw()+
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text.x = element_text(face = "bold"))+
  xlab("Ecosystem Type")+
  ylab("Scale of maximum forecast horizon")+
  scale_shape_manual(values = c(16,17,18,15),name = "Forecast Variable")+
  scale_fill_viridis(discrete = TRUE,name = "Forecast Variable")+
  scale_colour_viridis(discrete = TRUE,name = "Forecast Variable")
Fig6

ggsave(Fig6, filename = "./figures/Fig6.tif",height = 4, width = 6,
       units = "in", dpi = 300, dev = "tiff")

#Fig. 3: Water quality target variables
#Author: Mary Lofton
#Date: last updated 30AUG22

#clear environment
rm(list = ls())

#set-up
pacman::p_load(tidyverse, lubridate, cowplot, viridis)

#read in data
wq_targets <- read_csv("./data/wq_for_target_var.csv")

#munge data
targets <- strsplit(wq_targets$`Target Variables`, split = ",")
target_vector <- data.frame(targets = unlist(targets)) %>%
  mutate(phys_chem_bio = ifelse(targets %in% c("ice","temperature","sediment/turbidity"),"physical",
                                ifelse(targets %in% c("BOD/COD","conductivity/salinity","DO","gas emissions","hazardous chemical","metals","nutrients","pH","toxins/T&O compounds"),"chemical",
                                       ifelse(targets == "index","multiple","biological"))))
theTable <- within(target_vector, 
                   targets <- factor(targets, 
                                     levels=names(sort(table(targets), 
                                                       decreasing=TRUE))))

#build figure
targets <- ggplot(theTable, aes(x = targets, fill = phys_chem_bio))+
  geom_bar(color = "black")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Water Quality Target Variable")+
  ylab("# of papers")+
  theme(legend.position = "top")+
  scale_fill_viridis(discrete = TRUE, name = "Variable Type")

#plot and write figure
targets
ggsave(targets, filename = "./figures/Fig4.tif",height = 4, width = 8,
       units = "in", dpi = 300, dev = "tiff")

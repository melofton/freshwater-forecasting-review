#Matrix analysis
#Author: Mary Lofton
#Date: 06JUL22

#clear environment
rm(list = ls())

#set-up
pacman::p_load(tidyverse, lubridate, cowplot,ggbeeswarm, viridis)

#read in data 
dat5 <- read_csv("./data/cleaned_matrix.csv")

##Table 3 ####
dat10 <- dat5 %>%
  mutate(ecosystem_type = ifelse(ecosystem == "river" | grepl("basin",other_ecosystem),"Lotic","Lentic"))

colnames(dat10)
    
tab3 <- dat10[,c(2,4,3,27,11,12,13,14,18,19,20,21,16,17,23)] %>%
  arrange(Year)
tab3[16,"Year"] <- 2022

write.csv(tab3,"Table3.csv",row.names = FALSE)

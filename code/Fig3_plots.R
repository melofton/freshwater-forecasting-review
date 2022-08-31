#Fig 3 plots
#Author: Mary Lofton
#Date: last updated 30AUG22

#clear environment
rm(list = ls())

#set-up
pacman::p_load(tidyverse, lubridate, cowplot, viridis)

#read in data
final <- read_csv("./data/cleaned_initial_screen.csv")

#data munging
fresh <- final %>%
  filter(freshwater == "yes; surface water" | freshwater == "yes; groundwater") %>%
  mutate(fc = ifelse(is.na(forecast) | forecast == "none of the above" | forecast == "can't tell","not a forecast","forecast"))
forecast <- fresh %>%
  filter(fc == "forecast")
nt <- forecast %>%
  filter(nearterm == "yes")
lt <- forecast %>%
  filter(nearterm == "no")
nt_hydro <- nt %>%
  filter(hydrological == "yes")
nt_eco <- nt %>%
  filter(hydrological == "no")
lt_hydro <- lt %>%
  filter(hydrological == "yes")
lt_eco <- lt %>%
  filter(hydrological == "no")

#Model approach by forecast type (hydro vs. wq, nearterm vs. longterm,
#with or without uncertainty)

#near-term
nt_hydro_det <- nt_hydro %>%
  filter(uncertainty == "no")
nt_hydro_det_model <- nt_hydro_det %>%
  mutate(SIM = ifelse(grepl("numerical",model_approach),1,0),
         PROC = ifelse(grepl("process-based",model_approach),1,0),
         TS = ifelse(grepl("ARIMA",model_approach),1,0),
         EMP = ifelse(grepl("empirical",model_approach),1,0),
         ML = ifelse(grepl("machine",model_approach),1,0),
         other = ifelse(grepl("other",model_approach),1,0)) %>%
  select(SIM:other) %>%
  gather(SIM:other, key = "model_type", value = "value") %>%
  group_by(model_type) %>%
  summarize(mod_number = sum(value, na.rm = TRUE)) %>%
  mutate(model_type = factor(model_type, levels = c("EMP","ML","PROC","SIM","TS","other")))
p1 <- ggplot(nt_hydro_det_model, aes(x = model_type, y = mod_number))+
  geom_bar(stat = "identity",fill = "#D0CECE")+
  xlab("Model Type")+
  ylab("# of papers")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))
p1


nt_hydro_unc <- nt_hydro %>%
  filter(uncertainty == "yes")
nt_hydro_unc_model <- nt_hydro_unc %>%
  mutate(SIM = ifelse(grepl("numerical",model_approach),1,0),
         PROC = ifelse(grepl("process-based",model_approach),1,0),
         TS = ifelse(grepl("ARIMA",model_approach),1,0),
         EMP = ifelse(grepl("empirical",model_approach),1,0),
         ML = ifelse(grepl("machine",model_approach),1,0),
         other = ifelse(grepl("other",model_approach),1,0)) %>%
  select(SIM:other) %>%
  gather(SIM:other, key = "model_type", value = "value") %>%
  group_by(model_type) %>%
  summarize(mod_number = sum(value, na.rm = TRUE)) %>%
  mutate(model_type = factor(model_type, levels = c("EMP","ML","PROC","SIM","TS","other")))
p2 <- ggplot(nt_hydro_unc_model, aes(x = model_type, y = mod_number))+
  geom_bar(stat = "identity",fill = "#D0CECE")+
  xlab("Model Type")+
  ylab("# of papers")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))
p2

nt_eco_det <- nt_eco %>%
  filter(uncertainty == "no")
nt_eco_det_model <- nt_eco_det %>%
  mutate(SIM = ifelse(grepl("numerical",model_approach),1,0),
         PROC = ifelse(grepl("process-based",model_approach),1,0),
         TS = ifelse(grepl("ARIMA",model_approach),1,0),
         EMP = ifelse(grepl("empirical",model_approach),1,0),
         ML = ifelse(grepl("machine",model_approach),1,0),
         other = ifelse(grepl("other",model_approach),1,0)) %>%
  select(SIM:other) %>%
  gather(SIM:other, key = "model_type", value = "value") %>%
  group_by(model_type) %>%
  summarize(mod_number = sum(value, na.rm = TRUE)) %>%
  mutate(model_type = factor(model_type, levels = c("EMP","ML","PROC","SIM","TS","other")))
p3 <- ggplot(nt_eco_det_model, aes(x = model_type, y = mod_number))+
  geom_bar(stat = "identity",fill = "#D0CECE")+
  xlab("Model Type")+
  ylab("# of papers")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))
p3

nt_eco_unc <- nt_eco %>%
  filter(uncertainty == "yes")
nt_eco_unc_model <- nt_eco_unc %>%
  mutate(SIM = ifelse(grepl("numerical",model_approach),1,0),
         PROC = ifelse(grepl("process-based",model_approach),1,0),
         TS = ifelse(grepl("ARIMA",model_approach),1,0),
         EMP = ifelse(grepl("empirical",model_approach),1,0),
         ML = ifelse(grepl("machine",model_approach),1,0),
         other = ifelse(grepl("other",model_approach),1,0)) %>%
  select(SIM:other) %>%
  gather(SIM:other, key = "model_type", value = "value") %>%
  group_by(model_type) %>%
  summarize(mod_number = sum(value, na.rm = TRUE)) %>%
  mutate(model_type = factor(model_type, levels = c("EMP","ML","PROC","SIM","TS","other")))
p4 <- ggplot(nt_eco_unc_model, aes(x = model_type, y = mod_number))+
  geom_bar(stat = "identity",fill = "cadetblue3")+
  xlab("Model Type")+
  ylab("# of papers")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))
p4

#longterm
lt_hydro_det <- lt_hydro %>%
  filter(uncertainty == "no")
lt_hydro_det_model <- lt_hydro_det %>%
  mutate(SIM = ifelse(grepl("numerical",model_approach),1,0),
         PROC = ifelse(grepl("process-based",model_approach),1,0),
         TS = ifelse(grepl("ARIMA",model_approach),1,0),
         EMP = ifelse(grepl("empirical",model_approach),1,0),
         ML = ifelse(grepl("machine",model_approach),1,0),
         other = ifelse(grepl("other",model_approach),1,0)) %>%
  select(SIM:other) %>%
  gather(SIM:other, key = "model_type", value = "value") %>%
  group_by(model_type) %>%
  summarize(mod_number = sum(value, na.rm = TRUE)) %>%
  mutate(model_type = factor(model_type, levels = c("EMP","ML","PROC","SIM","TS","other")))
p5 <- ggplot(lt_hydro_det_model, aes(x = model_type, y = mod_number))+
  geom_bar(stat = "identity",fill = "#D0CECE")+
  xlab("Model Type")+
  ylab("# of papers")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))
p5

lt_hydro_unc <- lt_hydro %>%
  filter(uncertainty == "yes")
lt_hydro_unc_model <- lt_hydro_unc %>%
  mutate(SIM = ifelse(grepl("numerical",model_approach),1,0),
         PROC = ifelse(grepl("process-based",model_approach),1,0),
         TS = ifelse(grepl("ARIMA",model_approach),1,0),
         EMP = ifelse(grepl("empirical",model_approach),1,0),
         ML = ifelse(grepl("machine",model_approach),1,0),
         other = ifelse(grepl("other",model_approach),1,0)) %>%
  select(SIM:other) %>%
  gather(SIM:other, key = "model_type", value = "value") %>%
  group_by(model_type) %>%
  summarize(mod_number = sum(value, na.rm = TRUE)) %>%
  mutate(model_type = factor(model_type, levels = c("EMP","ML","PROC","SIM","TS","other")))
p6 <- ggplot(lt_hydro_unc_model, aes(x = model_type, y = mod_number))+
  geom_bar(stat = "identity",fill = "#D0CECE")+
  xlab("Model Type")+
  ylab("# of papers")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))
p6

lt_eco_det <- lt_eco %>%
  filter(uncertainty == "no")
lt_eco_det_model <- lt_eco_det %>%
  mutate(SIM = ifelse(grepl("numerical",model_approach),1,0),
         PROC = ifelse(grepl("process-based",model_approach),1,0),
         TS = ifelse(grepl("ARIMA",model_approach),1,0),
         EMP = ifelse(grepl("empirical",model_approach),1,0),
         ML = ifelse(grepl("machine",model_approach),1,0),
         other = ifelse(grepl("other",model_approach),1,0)) %>%
  select(SIM:other) %>%
  gather(SIM:other, key = "model_type", value = "value") %>%
  group_by(model_type) %>%
  summarize(mod_number = sum(value, na.rm = TRUE)) %>%
  mutate(model_type = factor(model_type, levels = c("EMP","ML","PROC","SIM","TS","other")))
p7 <- ggplot(lt_eco_det_model, aes(x = model_type, y = mod_number))+
  geom_bar(stat = "identity",fill = "#D0CECE")+
  xlab("Model Type")+
  ylab("# of papers")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))
p7

lt_eco_unc <- lt_eco %>%
  filter(uncertainty == "yes")
lt_eco_unc_model <- lt_eco_unc %>%
  mutate(SIM = ifelse(grepl("numerical",model_approach),1,0),
         PROC = ifelse(grepl("process-based",model_approach),1,0),
         TS = ifelse(grepl("ARIMA",model_approach),1,0),
         EMP = ifelse(grepl("empirical",model_approach),1,0),
         ML = ifelse(grepl("machine",model_approach),1,0),
         other = ifelse(grepl("other",model_approach),1,0)) %>%
  select(SIM:other) %>%
  gather(SIM:other, key = "model_type", value = "value") %>%
  group_by(model_type) %>%
  summarize(mod_number = sum(value, na.rm = TRUE)) %>%
  mutate(model_type = factor(model_type, levels = c("EMP","ML","PROC","SIM","TS","other")))
p8 <- ggplot(lt_eco_unc_model, aes(x = model_type, y = mod_number))+
  geom_bar(stat = "identity",fill = "#D0CECE")+
  xlab("Model Type")+
  ylab("# of papers")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))
p8

Fig3_plots<-plot_grid(p4,p3,p2,p1,p8,p7,p6,p5, align='v', vjust=1, scale = 1,
                nrow = 1, ncol = 8)
ggsave(Fig3_plots, filename = "./figures/Fig3_plots.tif",height = 1.5, width = 13.5,
       units = "in", dpi = 300, dev = "tiff")

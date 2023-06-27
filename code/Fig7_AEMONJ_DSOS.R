#Fig. 6
#Author: Mary Lofton
#Date: last updated 31AUG22

#clear environment
rm(list = ls())

#set-up
pacman::p_load(tidyverse, lubridate, cowplot, ggbeeswarm, viridis)

#read in data 
dat5 <- read_csv("./data/cleaned_matrix.csv")

#Fig. 3

#skill scores in more than one paper:
#' RMSE
#' MAE
#' Brier score
#' CRPS
#' ROC
#' AUC
#' bias
#' reliability/coverage 
#' R2
#' sharpness/spread

#data munging
dat6 <- dat5 %>%
  mutate(ecosystem_type = ifelse(ecosystem == "river" | grepl("basin",other_ecosystem),"Lotic","Lentic"),
         horizon_scale = ifelse(max_horizon_days<7,"days (< 7 days)",
                                ifelse(max_horizon_days<30,"weeks (7-30 days)",
                                       ifelse(max_horizon_days<365,"months (30-365 days)",
                                              ifelse(max_horizon_days<3650,"years (365-3650 days)","decadal (>3650 days)")))),
         horizon_scale = factor(horizon_scale, levels = c("days (< 7 days)","weeks (7-30 days)","months (30-365 days)","years (365-3650 days)","decadal (>3650 days)")))

dat7 <- dat6 %>%
  mutate(RMSE = ifelse(grepl("RMSE",skill_metrics),1,0),
         MAE = ifelse(grepl("MAE",skill_metrics),1,0),
         Brier = ifelse(grepl("Brier",skill_metrics),1,0),
         CRPS = ifelse(grepl("CRPS",skill_metrics),1,0),
         ROC = ifelse(grepl("ROC",skill_metrics),1,0),
         AUC = ifelse(grepl("AUC",skill_metrics),1,0),
         bias = ifelse(grepl("bias",skill_metrics),1,0),
         reliability = ifelse(grepl("reliability",skill_metrics) | grepl("coverage",skill_metrics),1,0),
         R2 = ifelse(grepl("R2",skill_metrics),1,0),
         sharpness = ifelse(grepl("spread",skill_metrics) | grepl("sharpness",skill_metrics),1,0))%>%
  gather(RMSE:sharpness, key = skill_metric, value = frequency) %>%
  group_by(phys_chem_bio,skill_metric) %>%
  summarize(metric_frequency = sum(frequency, na.rm = TRUE))

#panel a
a <- ggplot(data = dat7, aes(x = skill_metric, y = metric_frequency))+
  geom_bar(stat = "identity", position = "stack")+
  theme_classic()+
  #scale_fill_discrete(name = "Forecast Variable Type")+
  scale_x_discrete(labels=c("R2" = expression(R^2)))+
  xlab("Skill metric")+
  ylab("# of papers")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  theme(legend.position = "none")
  #scale_fill_viridis(discrete = TRUE)

a

#panel c
colnames(dat6)
dat9 <- dat6 %>%
  mutate(iterative = ifelse(iterative == "yes",1,0),
         automated = ifelse(automated == "yes",1,0),
         archived = ifelse(archived == "yes",1,0),
         model_comparison = ifelse(model_comparison == "yes",1,0),
         simple_null_model = ifelse(simple_null_model == "yes",1,0)) %>%
  select(phys_chem_bio, iterative, automated, archived, model_comparison, simple_null_model) %>%
  group_by(phys_chem_bio) %>%
  summarize(iterative = sum(iterative, na.rm = TRUE),
            automated = sum(automated, na.rm = TRUE),
            archived = sum(archived, na.rm = TRUE),
            `model comparison` = sum(model_comparison, na.rm = TRUE),
            `null model` = sum(simple_null_model, na.rm = TRUE)) %>%
  gather(iterative:`null model`,key = method, value = frequency) %>%
  mutate(method = factor(method, levels = c("iterative","automated","archived","model comparison","null model")))

c <- ggplot(data = dat9, aes(x = method, y = frequency))+
  geom_bar(stat = "identity")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  xlab("Workflow attribute")+
  ylab("# of papers")+
  theme(legend.position = "bottom")
  #guides(fill = guide_legend(title.position="top"))+
  #scale_fill_viridis(discrete = TRUE,name = "Forecast Variable Type")
c

Fig7<-plot_grid(a,c, align='v', vjust=1, scale = 1,
                nrow = 2, ncol = 1,
                rel_heights = c(1,1.1), hjust = 0)
ggsave(Fig7, filename = "./figures/Fig7_aemonjdsos.tif",height = 5, width = 3.75,
       units = "in", dpi = 300, dev = "tiff")

#alternative plot idea

# a_dwh <- ggplot(data = dat7, aes(x = phys_chem_bio, y = skill_metric, size = metric_frequency ))+
#   geom_point()+
#   # geom_bar(stat = "identity", position = "stack")+
#   theme_classic()+
#   # scale_fill_discrete(name = "Forecast Variable Type")+
#   labs(size = "Metric Frequency", 
#        y = "Skill metric")+
#   # xlab("Forecast Variable Type")+
#   # theme(axis.text.x = element_text(angle = 45,hjust = 1))+
#   scale_x_discrete(name = 'Forecast Variable Type', 
#                    breaks = c('physical', 'chemical', 'biological', 'multiple'), 
#                    labels = c('physical\n(n=5)', 'chemical\n(n=3)', 'biolgoical\n(n=4)', 'multiple\n(n=4)'))
# # theme(legend.position = "none")+
# # scale_fill_brewer(palette = "BrBG",direction = -1)
# 
# a_dwh
# 
# ggsave(a_dwh, filename = "./Fig3_a_DWH.tif",height = 7.5, width = 5,
#        units = "in", dpi = 300, dev = "tiff")
# 

#number of metrics vs. number of papers, colored by target variable type
# metric_list <- data.frame(result_num = rep(NA,16),
#                           num_metrics = rep(NA,16),
#                           phys_chem_bio = rep(NA,16))
# 
# for(i in 1:length(unique(dat6$result_num))){
#   metric_list$result_num[i] <- unique(dat6$result_num)[i]
#   if(is.na(dat6$skill_metrics[i])){
#     metric_list$num_metrics[i] <- 0
#   } else {
#     metrics <- strsplit(dat6$skill_metrics[i],split = ",")
#     metric_list$num_metrics[i] <- length(metrics[[1]])
#   }
#   metric_list$phys_chem_bio[i] <- dat6$phys_chem_bio[i]
# }
# 
# metric_list$num_metrics <- as.double(metric_list$num_metrics)
# 
# a1 <- ggplot(data = metric_list, aes(x = num_metrics))+
#   geom_histogram(binwidth = 1, fill = "lightgray",color = "darkgray")+
#   theme_classic()+
#   scale_x_continuous(breaks = c(0:6))+
#   xlab("# of skill metrics")+
#   ylab("# of papers")
# a1

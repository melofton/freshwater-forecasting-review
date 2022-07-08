#Matrix analysis
#Author: Mary Lofton
#Date: 06JUL22

#clear environment
rm(list = ls())

#set-up
pacman::p_load(tidyverse, lubridate, cowplot,ggbeeswarm)

#read in data 
dat <- read_csv("./GCB-review matrix_06JUL22.csv")
dat1 <- t(dat[c(1:29),]) 
titles <- row.names(dat1)
rownames(dat1) <- NULL
dat2 <- data.frame(dat1)
names(dat2) <- dat2[1,]
dat2 <- dat2[-1,]
dat2$Title <- titles[2:26]
colnames(dat2) <- c("Authors","Journal","Year","DOI","result_num","Reviewer",
                   "t1","ecosystem","other_ecosystem","phys_chem_bio","forecast_vars",
                   "min_horizon_days","max_horizon_days","skill_metrics","null_model",
                   "model_comparison","simple_null_model","uncertainty","t2","iterative",
                   "automated","archived","t3","motivation","end_user","engagement","t4",
                   "notes","unsuitable","Title")
dat3 <- dat2[,c(30,1:29)]
dat4 <- dat3 %>%
  select(-t1,-t2,-t3,-t4) %>%
  filter(unsuitable == "no")

dat5 <- dat4 %>%
  mutate(max_horizon_days = as.double(max_horizon_days),
         min_horizon_days = as.double(min_horizon_days))
dat5$min_horizon_days[1] <- 3650

#Fig. 2
dat6 <- dat5 %>%
  mutate(ecosystem_type = ifelse(ecosystem == "river" | grepl("basin",other_ecosystem),"Lotic","Lentic"),
         horizon_scale = ifelse(max_horizon_days<7,"days (< 7 days)",
                                ifelse(max_horizon_days<30,"weeks (7-30 days)",
                                       ifelse(max_horizon_days<365,"months (30-365 days)",
                                              ifelse(max_horizon_days<3650,"years (365-3650 days)","decadal (>3650 days)")))),
         horizon_scale = factor(horizon_scale, levels = c("days (< 7 days)","weeks (7-30 days)","months (30-365 days)","years (365-3650 days)","decadal (>3650 days)")))

Fig2 <- ggplot(data = dat6, aes(x = unsuitable, y = horizon_scale, group = phys_chem_bio, shape = phys_chem_bio, color = phys_chem_bio))+
  geom_beeswarm(priority='random',cex=18, groupOnX=T,size = 3)+
  facet_grid(cols = vars(ecosystem_type),switch = "x")+
  theme_bw()+
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank())+
  xlab("Ecosystem Type")+
  ylab("Scale of maximum forecast horizon")+
  scale_shape_discrete(name = "Forecast Variable")+
  scale_fill_brewer(palette = "BrBG",direction = -1,name = "Forecast Variable")+
  scale_colour_brewer(palette = "BrBG",direction = -1,name = "Forecast Variable")
Fig2

ggsave(Fig2, filename = "./Fig2.tif",height = 4, width = 6,
       units = "in", dpi = 300, dev = "tiff")

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

dat7 <- dat6 %>%
  mutate(RMSE = ifelse(grepl("RMSE",skill_metrics),1,0),
         MAE = ifelse(grepl("MAE",skill_metrics),1,0),
         Brier = ifelse(grepl("Brier",skill_metrics),1,0),
         CRPS = ifelse(grepl("CRPS",skill_metrics),1,0),
         ROC = ifelse(grepl("ROC",skill_metrics),1,0),
         AUC = ifelse(grepl("AUC",skill_metrics),1,0),
         bias = ifelse(grepl("bias",skill_metrics),1,0),
         reliability = ifelse(grepl("reliability",skill_metrics) | grepl("coverage",skill_metrics),1,0),
         R2 = ifelse(grepl("R2",skill_metrics),1,0))%>%
  gather(RMSE:R2, key = skill_metric, value = frequency) %>%
  group_by(phys_chem_bio,skill_metric) %>%
  summarize(metric_frequency = sum(frequency, na.rm = TRUE))

#panel a
a <- ggplot(data = dat7, aes(x = skill_metric, y = metric_frequency, fill = phys_chem_bio))+
  geom_bar(stat = "identity", position = "stack")+
  theme_classic()+
  scale_fill_discrete(name = "Forecast Variable Type")+
  xlab("Skill metric")+
  ylab("# of papers")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  theme(legend.position = "none")+
  scale_fill_brewer(palette = "BrBG",direction = -1)

a

a_dwh <- ggplot(data = dat7, aes(x = phys_chem_bio, y = skill_metric, size = metric_frequency ))+
  geom_point()+
  # geom_bar(stat = "identity", position = "stack")+
  theme_classic()+
  # scale_fill_discrete(name = "Forecast Variable Type")+
  labs(size = "Metric Frequency", 
       y = "Skill metric")+
  # xlab("Forecast Variable Type")+
  # theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  scale_x_discrete(name = 'Forecast Variable Type', 
                   breaks = c('physical', 'chemical', 'biological', 'multiple'), 
                   labels = c('physical\n(n=5)', 'chemical\n(n=3)', 'biolgoical\n(n=4)', 'multiple\n(n=4)'))
# theme(legend.position = "none")+
# scale_fill_brewer(palette = "BrBG",direction = -1)

a_dwh

ggsave(a_dwh, filename = "./Fig3_a_DWH.tif",height = 7.5, width = 5,
       units = "in", dpi = 300, dev = "tiff")

#panel a1
metric_list <- data.frame(result_num = rep(NA,16),
                          num_metrics = rep(NA,16),
                          phys_chem_bio = rep(NA,16))

for(i in 1:length(unique(dat6$result_num))){
  metric_list$result_num[i] <- unique(dat6$result_num)[i]
  if(is.na(dat6$skill_metrics[i])){
    metric_list$num_metrics[i] <- 0
  } else {
  metrics <- strsplit(dat6$skill_metrics[i],split = ",")
  metric_list$num_metrics[i] <- length(metrics[[1]])
  }
  metric_list$phys_chem_bio[i] <- dat6$phys_chem_bio[i]
}

metric_list$num_metrics <- as.double(metric_list$num_metrics)

a1 <- ggplot(data = metric_list, aes(x = num_metrics))+
  geom_histogram(binwidth = 1, fill = "lightgray",color = "darkgray")+
  theme_classic()+
  scale_x_continuous(breaks = c(0:6))+
  xlab("# of skill metrics")+
  ylab("# of papers")
a1

#panel b
dat8 <- dat6 %>%
  mutate(uncertainty = ifelse(uncertainty == "data_driven","data driven",uncertainty),
         uncertainty = factor(uncertainty, levels = c("present","data driven","propagates","assimilates")))
b <- ggplot(data = dat8, aes(x = uncertainty, fill = phys_chem_bio))+
  geom_bar()+
  theme_classic()+
  xlab("Method of uncertainty specification")+
  ylab("# of papers")+
  scale_fill_discrete(name = "Forecast Variable Type")+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  theme(legend.position = "none")+
  scale_fill_brewer(palette = "BrBG",direction = -1)
b

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

c <- ggplot(data = dat9, aes(x = method, y = frequency, fill = phys_chem_bio))+
  geom_bar(stat = "identity")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  xlab("Workflow attribute")+
  ylab("# of papers")+
  theme(legend.position = "bottom")+
  guides(fill = guide_legend(title.position="top"))+
  scale_fill_brewer(palette = "BrBG",direction = -1,name = "Forecast Variable")
c

Fig3<-plot_grid(a,b,c, align='v', vjust=1, scale = 1,
                nrow = 3, ncol = 1, labels = c("a","b","c"),
                rel_heights = c(1,1,1.55))
ggsave(Fig3, filename = "./Fig3.tif",height = 7.5, width = 3.75,
       units = "in", dpi = 300, dev = "tiff")


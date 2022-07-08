#Initial screen analysis
#Author: Mary Lofton
#Date: 06APR22

##Note 21JUN22

# Should do manual screen of notes to see if any papers need to be
# dropped altogether
# That said, papers not in English could be included in abstract
# screen if we can retrieve the necessary information, but would
# have to be excluded from full screen

#clear environment
rm(list = ls())

#set-up
pacman::p_load(tidyverse, lubridate, cowplot)

#read in Google form abstract screen data 
dat <- read_csv("./final_search_abstract_screen_for_co-authors_06JUL22.csv") %>%
  ##make sure we are only keeping entries that MEL screened
  filter(`Email Address` == "melofton@vt.edu")

#make column names manageable
colnames(dat)[1:10] <- c("date","result_num","freshwater","forecast","uncertainty","nearterm","model_approach","notes","hydrological","email")

#get rid of extra columns
dat <- dat[,c(1:10)] 

#limit to abstracts that passed title screen
#need to re-assign result numbers to link w/ Google Form output
abstracts <- read_csv("./savedrecs.csv") %>% #this is the list of abstracts we originally screened
  select(`Result Number`,`Article Title`) %>%
  filter(complete.cases(.))

titles <- read_csv("./final_search_title_screen.csv") %>% #these are the results of the title screen
  filter(`Title Screen` == "y") %>%
  select(`Title Screen`,`Article Title`)

first_screen <- left_join(titles, abstracts, by = "Article Title")

skipped <- read_csv("./skipped_abstracts.csv") %>%
  rename(`Result Number` = Result) %>%
  select(`Result Number`,`Article Title`)

#this data frame has result numbers of all articles that passed title screen
corrected_screen <- left_join(first_screen, skipped, by = "Article Title") %>%
  mutate(`Result Number` = ifelse(is.na(`Result Number.x`),`Result Number.y`,`Result Number.x`)) %>%
  select(`Result Number`,`Article Title`,`Title Screen`)

#this data frame has only results of abstracts that passed title screen
dat2 <- dat %>%
  filter(result_num %in% corrected_screen$`Result Number` | result_num == 326) %>%
  arrange(result_num)

#check for duplicates
length(unique(dat2$result_num))
#oop we have a duplicated result
n_occur <- data.frame(table(dat2$result_num)) %>%
  filter(Freq >1)
#keep the most recent one
dat3 <- dat2[-489,]

#get result numbers for ILL screens
ill <- dat3 %>%
  filter(grepl("7/5/2022",date))
ill_result_num <- ill$result_num

#we are missing some papers -> which ones?
missing <- corrected_screen %>%
  filter(!`Result Number` %in% dat$result_num)
#ok as of 05JUL22 all four of these have active requests submitted to ILL
#moving on...
#as of 06JUL22 all abstracts screened!!!!

#read in abstracts that were rescreened after addition of 
#questions about model approach and hydrology
rescreened_for_hydro <- read_csv("./rescreened_abstracts.csv") %>%
  filter(!result_num %in% ill_result_num)

hydro_result_num <- unique(rescreened_for_hydro$result_num)

for(i in 1:length(hydro_result_num)){
  my.old.row <- which(dat3[,"result_num"] == hydro_result_num[i])
  my.new.row <- which(rescreened_for_hydro[,"result_num"] == hydro_result_num[i])
  
  dat3[my.old.row,c(1:9)] <- rescreened_for_hydro[my.new.row,]

}

#read in abstracts that were rescreened because nearterm was "can't tell"
#or NA
rescreened_for_nearterm <- read_csv("./nearterm_rescreen.csv") %>%
  filter(!result_num %in% ill_result_num)

nearterm_result_num <- unique(rescreened_for_nearterm$result_num)

for(i in 1:length(nearterm_result_num)){
  my.old.row <- which(dat3[,"result_num"] == nearterm_result_num[i])
  my.new.row <- which(rescreened_for_nearterm[,"result_num"] == nearterm_result_num[i])
  
  dat3[my.old.row,c(1:9)] <- rescreened_for_nearterm[my.new.row,c(1:9)]

}

#read in abstracts that were rescreened after we decided we wanted
#model approach for all abstracts
rescreened_for_mod_approach <- read_csv("./uncert_rescreen.csv") %>%
  select(-date) %>%
  distinct() %>%
  filter(!result_num %in% ill_result_num)

mod_approach_result_num <- unique(rescreened_for_mod_approach$result_num)

for(i in 1:length(mod_approach_result_num)){
  my.old.row <- which(dat3[,"result_num"] == mod_approach_result_num[i])
  my.new.row <- which(rescreened_for_mod_approach[,"result_num"] == mod_approach_result_num[i])
  
  dat3[my.old.row,c(2:9)] <- rescreened_for_mod_approach[my.new.row,c(1:8)]
  
}

#making edits to screen results based on what was found in matrix review
# matrix_fail_result_num <- c(42,123,124,191,668)
# matrix_fails <- dat3 %>%
#   filter(result_num %in% matrix_fail_result_num)
#write.csv(matrix_fails, "./matrix_fails.csv",row.names = FALSE)

matrix_fails <- read_csv("./matrix_fails.csv") %>%
  mutate(notes = as.character(notes))

matrix_fail_result_num <- unique(matrix_fails$result_num)

for(i in 1:length(matrix_fail_result_num)){
  my.old.row <- which(dat3[,"result_num"] == matrix_fail_result_num[i])
  my.new.row <- which(matrix_fails[,"result_num"] == matrix_fail_result_num[i])
  
  dat3[my.old.row,] <- matrix_fails[my.new.row,]
  
}


## QUESTIONS TO ANSWER FROM INITIAL SCREEN ####
final <- dat3
#write.csv(final, "./cleaned_initial_screen.csv",row.names = FALSE)

#How many papers were not actually focusing on inland waters?
ggplot(final, aes(x = freshwater)) +
  geom_bar()

#How many freshwater papers were focused solely on hydrology?
fresh <- final %>%
  filter(freshwater == "yes; surface water" | freshwater == "yes; groundwater") %>%
  mutate(fc = ifelse(is.na(forecast) | forecast == "none of the above" | forecast == "can't tell","not a forecast","forecast"))

ggplot(fresh, aes(x = hydrological)) +
  geom_bar()

#How many freshwater papers are forecasts, hindcasts, or nowcasts?
ggplot(fresh, aes(x = fc)) +
  geom_bar()

#How many freshwater forecast papers were focused solely on hydrology?
forecast <- fresh %>%
  filter(fc == "forecast")

#grab wq papers to get target var
wq <- forecast %>%
  filter(hydrological == "no") %>%
  rename(`Result Number` = result_num)
wq1 <- left_join(wq,corrected_screen, by = "Result Number")
write.csv(wq1, "wq_for_target_var.csv",row.names = FALSE)

wq_targets <- read_csv("./wq_for_target_var.csv")
targets <- strsplit(wq_targets$`Target Variables`, split = ",")
target_vector <- data.frame(targets = unlist(targets))

targets <- ggplot(target_vector, aes(x = targets))+
  geom_bar()+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  xlab("Water Quality Target Variable")+
  ylab("# of papers")
ggsave(targets, filename = "./wq_target_vars.tif",height = 4, width = 8,
       units = "in", dpi = 300, dev = "tiff")

ggplot(forecast, aes(x = hydrological))+
  geom_bar()

#How many freshwater forecast papers include uncertainty?
ggplot(forecast, aes(x = uncertainty))+
  geom_bar()

#How many forecast papers are near-term?
ggplot(forecast, aes(x = nearterm))+
  geom_bar()

nt <- forecast %>%
  filter(nearterm == "yes")
lt <- forecast %>%
  filter(nearterm == "no")

ggplot(nt, aes(x = hydrological))+
  geom_bar()
ggplot(lt, aes(x = hydrological))+
  geom_bar()

#Uncertainty
nt_hydro <- nt %>%
  filter(hydrological == "yes")

ggplot(nt_hydro, aes(x = uncertainty))+
  geom_bar()

nt_eco <- nt %>%
  filter(hydrological == "no")

ggplot(nt_eco, aes(x = uncertainty))+
  geom_bar()

lt_hydro <- lt %>%
  filter(hydrological == "yes")

ggplot(lt_hydro, aes(x = uncertainty))+
  geom_bar()

lt_eco <- lt %>%
  filter(hydrological == "no")

ggplot(lt_eco, aes(x = uncertainty))+
  geom_bar()

#Model approach

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

Fig1<-plot_grid(p4,p3,p2,p1,p8,p7,p6,p5, align='v', vjust=1, scale = 1,
                nrow = 1, ncol = 8)
ggsave(Fig1, filename = "./Fig1.tif",height = 1.5, width = 13.5,
       units = "in", dpi = 300, dev = "tiff")

#Among forecast papers that do NOT include uncertainty, how many focus solely on hydrology?
det <- forecast %>%
  filter(uncertainty == "no")
ggplot(det, aes(x = hydrological))+
  geom_bar()

#Among forecast papers that DO include uncertainty, how many focus solely on hydrology?
unc <- forecast %>%
  filter(uncertainty == "yes")
ggplot(unc, aes(x = hydrological))+
  geom_bar()

#Among forecast papers that do NOT include uncertainty, what is the breakdown of modeling approaches?
model <- det %>%
  mutate(sim = ifelse(grepl("numerical",model_approach),1,0),
         proc = ifelse(grepl("process",model_approach),1,0),
         ts = ifelse(grepl("ARIMA",model_approach),1,0),
         emp = ifelse(grepl("empirical",model_approach),1,0),
         ml = ifelse(grepl("machine",model_approach),1,0),
         other = ifelse(grepl("other",model_approach),1,0))
model <- model %>%
  select(sim:other) %>%
  gather(sim:other, key = "model_type", value = "value") %>%
  filter(value == 1)
ggplot(model, aes(x = model_type))+
  geom_bar()

#Among forecast papers that focus on hydrology and do NOT include uncertainty, what is the breakdown of modeling approaches?
det_hydro <- det %>%
  filter(uncertainty == "no" & hydrological == "yes")
model <- det_hydro %>%
  mutate(sim = ifelse(grepl("numerical",model_approach),1,0),
         proc = ifelse(grepl("process",model_approach),1,0),
         ts = ifelse(grepl("ARIMA",model_approach),1,0),
         emp = ifelse(grepl("empirical",model_approach),1,0),
         ml = ifelse(grepl("machine",model_approach),1,0),
         other = ifelse(grepl("other",model_approach),1,0))
model <- model %>%
  select(sim:other) %>%
  gather(sim:other, key = "model_type", value = "value") %>%
  filter(value == 1)
ggplot(model, aes(x = model_type))+
  geom_bar()

#Among forecast papers that are NOT hydrology-focused and do NOT include uncertainty, what is the breakdown of modeling approaches?
det_eco <- det %>%
  filter(uncertainty == "no" & hydrological == "no")
model <- det_eco %>%
  mutate(sim = ifelse(grepl("numerical",model_approach),1,0),
         proc = ifelse(grepl("process",model_approach),1,0),
         ts = ifelse(grepl("ARIMA",model_approach),1,0),
         emp = ifelse(grepl("empirical",model_approach),1,0),
         ml = ifelse(grepl("machine",model_approach),1,0),
         other = ifelse(grepl("other",model_approach),1,0))
model <- model %>%
  select(sim:other) %>%
  gather(sim:other, key = "model_type", value = "value") %>%
  filter(value == 1)
ggplot(model, aes(x = model_type))+
  geom_bar()

#Among forecast papers that DO include uncertainty, what is the breakdown of modeling approaches?
model <- unc %>%
  mutate(sim = ifelse(grepl("numerical",model_approach),1,0),
         proc = ifelse(grepl("process",model_approach),1,0),
         ts = ifelse(grepl("ARIMA",model_approach),1,0),
         emp = ifelse(grepl("empirical",model_approach),1,0),
         ml = ifelse(grepl("machine",model_approach),1,0),
         other = ifelse(grepl("other",model_approach),1,0))
model <- model %>%
  select(sim:other) %>%
  gather(sim:other, key = "model_type", value = "value") %>%
  filter(value == 1)
ggplot(model, aes(x = model_type))+
  geom_bar()

#Among forecast papers that focus on hydrology and DO include uncertainty, what is the breakdown of modeling approaches?
unc_hydro <- unc %>%
  filter(uncertainty == "yes" & hydrological == "yes")
model <- unc_hydro %>%
  mutate(sim = ifelse(grepl("numerical",model_approach),1,0),
         proc = ifelse(grepl("process",model_approach),1,0),
         ts = ifelse(grepl("ARIMA",model_approach),1,0),
         emp = ifelse(grepl("empirical",model_approach),1,0),
         ml = ifelse(grepl("machine",model_approach),1,0),
         other = ifelse(grepl("other",model_approach),1,0))
model <- model %>%
  select(sim:other) %>%
  gather(sim:other, key = "model_type", value = "value") %>%
  filter(value == 1)
ggplot(model, aes(x = model_type))+
  geom_bar()

#Among forecast papers that are NOT hydrology-focused and DO include uncertainty, what is the breakdown of modeling approaches?
unc_eco <- unc %>%
  filter(uncertainty == "yes" & hydrological == "no")
model <- unc_eco %>%
  mutate(sim = ifelse(grepl("numerical",model_approach),1,0),
         proc = ifelse(grepl("process",model_approach),1,0),
         ts = ifelse(grepl("ARIMA",model_approach),1,0),
         emp = ifelse(grepl("empirical",model_approach),1,0),
         ml = ifelse(grepl("machine",model_approach),1,0),
         other = ifelse(grepl("other",model_approach),1,0))
model <- model %>%
  select(sim:other) %>%
  gather(sim:other, key = "model_type", value = "value") %>%
  filter(value == 1)
ggplot(model, aes(x = model_type))+
  geom_bar()

#How many near-term forecasts with uncertainty?
ntfc <- forecast %>%
  mutate(ntfc = ifelse(nearterm == "yes" & uncertainty == "yes","yes","no"))
ggplot(data = ntfc, aes(x = ntfc))+
  geom_bar()+
  ggtitle("Near-term forecast with uncertainty?")+
  xlab("")

#How many near-term ecological forecasts with uncertainty?
ntecofc <- ntfc %>%
  mutate(ntecofc = ifelse(ntfc == "yes" & hydrological == "no","yes","no"))
ggplot(data = ntecofc, aes(x = ntecofc))+
  geom_bar()+
  ggtitle("Near-term ecological forecast w/ uncert?")+
  xlab("")
check <- ntecofc %>%
  filter(ntecofc == "yes")
abstracts <- read_csv("./savedrecs.csv") %>%
  filter(`Result Number` %in% check$result_num) 
skipped_abstracts <- read_csv("./skipped_abstracts.csv") %>%
  rename(`Result Number` = `Result`) %>%
  filter(`Result Number` %in% check$result_num)
matrix_abstracts <- rbind(abstracts[,c(1,2,6,7,20,4)],skipped_abstracts[,c(1,2,11,15,50,4)])
matrix_papers <- t(matrix_abstracts)
#write.csv(matrix_papers,"./matrix_papers.csv",row.names = TRUE)

##IDEA!! Money figure that is a network diagram among different
#nodes describing attributes of a reviewed paper.

#Strength of connection is determined by how often these attributes
#co-occur, just like in community analysis

#Nodes:
#' Nearterm NT
#' Longterm LT
#' Hydrological H
#' Ecological E
#' Uncertainty UC
#' No uncertainty Det
#' Each of the various modeling approaches
#' Sim
#' Proc
#' TS
#' DD
#' ML
#' O

library(igraph)

# Create data frame where every node is true or false
# rows = papers
# cols = nodes
fc <- dat %>%
  filter(freshwater %in% c("yes; surface water","yes; groundwater") & !forecast %in% c("none of the above","can't tell"))

df <- data.frame(NT = rep(NA, times = length(dat$result_num)),
                 LT = rep(NA, times = length(dat$result_num)),
                 H = rep(NA, times = length(dat$result_num)),
                 E = rep(NA, times = length(dat$result_num)),
                 UC = rep(NA, times = length(dat$result_num)),
                 Det = rep(NA, times = length(dat$result_num)),
                 Sim = rep(NA, times = length(dat$result_num)),
                 Proc = rep(NA, times = length(dat$result_num)),
                 TS = rep(NA, times = length(dat$result_num)),
                 DD = rep(NA, times = length(dat$result_num)),
                 ML = rep(NA, times = length(dat$result_num)),
                 O = rep(NA, times = length(dat$result_num)))

for(i in 1:length(dat$result_num)){
  if(dat$nearterm[i] == "yes")
}

# Generate co-occurrence matrix with crossproduct
co_mat <- t(df) %*% df

# Set diagonal values to 0
diag(co_mat) <- 0

# Assign dim names
dimnames(co_mat) <- list(colnames(df), colnames(df))

# Create graph from adjacency matrix
# ! edge weights are equal to frequency of co-occurrence
g <- graph_from_adjacency_matrix(co_mat, mode = "upper", weighted = TRUE)

# Assign nodes weight equal to species frequency
g <- set.vertex.attribute(g, "v_weight", value = colSums(df))

plot(g, vertex.size = V(g)$v_weight * 5 + 5, edge.width = E(g)$weight * 5)


pred  <- dat %>% filter(!forecast == "none of the above" & freshwater %in% c("yes; surface water") ) %>%
  arrange(result_num)
nearterm  <- dat %>% filter(!forecast == "none of the above" & freshwater %in% c("yes; surface water") & nearterm == "yes") %>%
  arrange(result_num)
long_uncert <- dat %>% filter(!forecast == "none of the above" & freshwater %in% c("yes; surface water") & uncertainty == "yes" & nearterm == "no") %>%
  arrange(result_num)
fresh <- dat %>% filter(!forecast == "none of the above" & freshwater %in% c("yes; surface water") & nearterm == "yes" & uncertainty == "yes") %>%
  arrange(result_num)
length(unique(fresh$result_num))
hydro <- fresh %>%
  filter(hydrological == "yes")
length(unique(hydro$result_num))

machine <- hydro %>%
  filter(model_approach == )

eco <- fresh %>%
  filter(hydrological == "no" | is.na(hydrological)) %>%
  arrange(result_num)
length(unique(eco$result_num))

ff <- fresh %>%
  filter(uncertainty == "yes")
hydro_f <- hydro %>%
  filter(uncertainty == "yes")
fp <- fresh %>%
  filter(uncertainty == "no")
hydro_p <- hydro %>%
  filter(uncertainty == "no")

nearterm <- ff %>%
  filter(nearterm == "yes")

rescreen_model_approach <- dat %>%
  filter(freshwater == "yes; surface water" | freshwater == "yes; groundwater") %>%
  filter(forecast %in% c("forecast","hindcast","nowcast")) %>%
  filter(uncertainty == "no") %>%
  filter(is.na(model_approach))

rescreen_hydro <- dat %>%
  filter(freshwater == "yes; surface water" | freshwater == "yes; groundwater") %>%
  filter(is.na(hydrological))

overlap <- inner_join(rescreen_model_approach, rescreen_hydro)
all_rescreen <- full_join(rescreen_model_approach, rescreen_hydro) %>%
  arrange(result_num)

abstracts <- read_csv("./savedrecs.csv") %>%
  filter(`Result Number` %in% all_rescreen$result_num) %>%
  mutate(hydro_plus_model_approach = ifelse(`Result Number` %in% overlap$result_num,TRUE,FALSE))

abstracts <- abstracts[,c(27,1:26)]
write.csv(abstracts, file = "./abstracts_for_rescreen.csv",row.names = FALSE)
write.csv(all_rescreen, file = "./abstract_results_for_rescreen.csv",row.names = FALSE)

result_nums <- data.frame(result_num = c(1:713))
missing_nums <- anti_join(result_nums, dat)

missing <- abstracts %>%
  filter(`Result Number` %in% missing_nums$result_num)
write.csv(missing, file = "./ILL_papers_to_screen.csv",row.names = FALSE)

mistyped <- abstracts %>%
  filter(!`Result Number` %in% result_nums$result_num & !is.na(`Result Number`))


##Pulling in missed abstracts from title screen
abstracts <- read_csv("./savedrecs.csv")
titles <- read_csv("./final_search_title_screen.csv")
colnames(titles)
y <- titles %>%
  filter(`Title Screen` == "y")
missing <- anti_join(y,abstracts,by = "Article Title")
write.csv(missing, file = "./skipped_abstracts.csv", row.names = FALSE)

#get data table of abstracts that were double-screened
#currently only have 712 unique IDs b/c still waiting on 
#one full-text from ILL
n_occur <- data.frame(table(final$result_num))
dups <- n_occur[n_occur$Freq > 1,]
dup <- final[final$result_num %in% n_occur$Var1[n_occur$Freq > 1],] %>%
  arrange(result_num)

agree <- data.frame(result_num = unique(dups$Var1),
                    freshwater = rep(NA, times = length(unique(dups$Var1))),
                    forecast = rep(NA, times = length(unique(dups$Var1))),
                    uncertainty = rep(NA, times = length(unique(dups$Var1))),
                    nearterm = rep(NA, times = length(unique(dups$Var1))),
                    hydrological = rep(NA, times = length(unique(dups$Var1))))

keepers <- data.frame()

for(i in 1:length(unique(dups$Var1))){
  abstract <- dup %>%
    filter(result_num == dups$Var1[i])
  
  #agree on freshwater?
  if(length(unique(abstract$freshwater)) > 1){
    agree$freshwater[i] <- "disagree"
  } else {
    agree$freshwater[i] <- "agree"
  }
  
  #agree on hydrological?
  if(length(unique(abstract$hydrological)) > 1){
    agree$hydrological[i] <- "disagree"
  } else {
    agree$hydrological[i] <- "agree"
  }
  
  #agree on forecast?
  if(agree$freshwater[i] == "agree" & agree$hydrological[i] == "agree"){
    if(length(unique(abstract$forecast %in% c("forecast","hindcast","nowcast"))) > 1){
      agree$forecast[i] <- "disagree"
    } else {
      agree$forecast[i] <- "agree"
    }
  }
  
  #agree on uncertainty?
  if(agree$freshwater[i] == "agree" & agree$hydrological[i] == "agree" & agree$forecast[i] == "agree"){
    if(length(unique(abstract$uncertainty)) > 1){
      agree$uncertainty[i] <- "disagree"
    } else {
      agree$uncertainty[i] <- "agree"
    }
  }
  
  #agree on nearterm?
  if(agree$freshwater[i] == "agree" & agree$hydrological[i] == "agree" & agree$forecast[i] == "agree"){
    if(length(unique(abstract$nearterm)) > 1){
      agree$nearterm[i] <- "disagree"
    } else {
      agree$nearterm[i] <- "agree"
    }
  }
  
  keepers <- bind_rows(keepers, abstract %>% slice(which.max(as.Date(date, '%m/%d/%Y %H:%M'))))
  
}

ggplot(agree, aes(x = freshwater)) +
  geom_bar()
ggplot(agree, aes(x = hydrological)) +
  geom_bar()
ggplot(subset(agree, freshwater == "agree" & hydrological == "agree"), aes(x = forecast)) +
  geom_bar()
ggplot(subset(agree, freshwater == "agree" & hydrological == "agree" & forecast == "agree"), aes(x = uncertainty)) +
  geom_bar()
ggplot(subset(agree, freshwater == "agree" & hydrological == "agree" & forecast == "agree"), aes(x = nearterm)) +
  geom_bar()

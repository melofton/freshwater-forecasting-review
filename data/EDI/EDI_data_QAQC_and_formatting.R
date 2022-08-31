#Title: Cleaning results for publication on EDI
#Author: Mary Lofton
#Date: last updated 30AUG22

##SET-UP----

#clear environment
rm(list = ls())

#install.packages("pacman")
pacman::p_load(tidyverse, lubridate)

##MUNGING OF INITIAL SCREEN RESULTS----

#read in Google form abstract screen data 
dat <- read_csv("./data/final_search_abstract_screen_for_co-authors_06JUL22.csv") %>%
  ##make sure we are only keeping entries that MEL screened
  filter(`Email Address` == "melofton@vt.edu")

#make column names manageable
colnames(dat)[1:10] <- c("date","result_num","freshwater","forecast","uncertainty","nearterm","model_approach","notes","hydrological","email")

#get rid of extra columns
dat <- dat[,c(1:10)] 

#limit to abstracts that passed title screen
#need to re-assign result numbers to link w/ Google Form output
abstracts <- read_csv("./data/savedrecs.csv") %>% #this is the list of abstracts we originally screened
  select(`Result Number`,`Article Title`) %>%
  filter(complete.cases(.))

titles <- read_csv("./data/final_search_title_screen.csv") %>% #these are the results of the title screen
  filter(`Title Screen` == "y") %>%
  select(`Title Screen`,`Article Title`)

first_screen <- left_join(titles, abstracts, by = "Article Title")

skipped <- read_csv("./data/skipped_abstracts.csv") %>%
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
rescreened_for_hydro <- read_csv("./data/rescreened_abstracts.csv") %>%
  filter(!result_num %in% ill_result_num)

hydro_result_num <- unique(rescreened_for_hydro$result_num)

for(i in 1:length(hydro_result_num)){
  my.old.row <- which(dat3[,"result_num"] == hydro_result_num[i])
  my.new.row <- which(rescreened_for_hydro[,"result_num"] == hydro_result_num[i])
  
  dat3[my.old.row,c(1:9)] <- rescreened_for_hydro[my.new.row,]
  
}

#read in abstracts that were rescreened because nearterm was "can't tell"
#or NA
rescreened_for_nearterm <- read_csv("./data/nearterm_rescreen.csv") %>%
  filter(!result_num %in% ill_result_num)

nearterm_result_num <- unique(rescreened_for_nearterm$result_num)

for(i in 1:length(nearterm_result_num)){
  my.old.row <- which(dat3[,"result_num"] == nearterm_result_num[i])
  my.new.row <- which(rescreened_for_nearterm[,"result_num"] == nearterm_result_num[i])
  
  dat3[my.old.row,c(1:9)] <- rescreened_for_nearterm[my.new.row,c(1:9)]
  
}

#read in abstracts that were rescreened after we decided we wanted
#model approach for all abstracts
rescreened_for_mod_approach <- read_csv("./data/uncert_rescreen.csv") %>%
  select(-date) %>%
  distinct() %>%
  filter(!result_num %in% ill_result_num)

mod_approach_result_num <- unique(rescreened_for_mod_approach$result_num)

for(i in 1:length(mod_approach_result_num)){
  my.old.row <- which(dat3[,"result_num"] == mod_approach_result_num[i])
  my.new.row <- which(rescreened_for_mod_approach[,"result_num"] == mod_approach_result_num[i])
  
  dat3[my.old.row,c(2:9)] <- rescreened_for_mod_approach[my.new.row,c(1:8)]
  
}

#read in revised initial screen results of papers that
#were identified as not actually meeting nearterm wq forecast criteria
#during matrix analysis
matrix_fails <- read_csv("./data/matrix_fails.csv") %>%
  mutate(notes = as.character(notes))

matrix_fail_result_num <- unique(matrix_fails$result_num)

for(i in 1:length(matrix_fail_result_num)){
  my.old.row <- which(dat3[,"result_num"] == matrix_fail_result_num[i])
  my.new.row <- which(matrix_fails[,"result_num"] == matrix_fail_result_num[i])
  
  dat3[my.old.row,] <- matrix_fails[my.new.row,]
  
}

final <- dat3

#this is the version used to make figures
write.csv(final, "./data/cleaned_initial_screen.csv",row.names = FALSE)

#this is the version that goes into EDI pub
final <- read_csv("./data/cleaned_initial_screen.csv")
edi <- final %>%
  select(-date,-notes, -email)
write.csv(edi, "./data/initial_screen_results.csv",row.names = FALSE)

##MUNGING OF MATRIX ANALYSIS RESULTS----

#read in matrix data 
dat <- read_csv("./data/GCB-review matrix_23AUG22.csv")

#reformat
dat1 <- t(dat[c(1:29),]) 
titles <- row.names(dat1)
rownames(dat1) <- NULL
dat2 <- data.frame(dat1)
names(dat2) <- dat2[1,]
dat2 <- dat2[-1,]
dat2$Title <- titles[2:22]
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

#convert horizon to numeric
dat5 <- dat4 %>%
  mutate(max_horizon_days = as.double(max_horizon_days),
         min_horizon_days = as.double(min_horizon_days))
dat5$min_horizon_days[1] <- 3650

#this is the version that is used for figures
write.csv(dat5, "./data/cleaned_matrix.csv",row.names = FALSE)

#this is the version that is used for EDI
edi <- dat5 %>%
  select(-notes, -unsuitable)
write.csv(edi, "./data/matrix_analysis_results.csv",row.names = FALSE)

##MUNGING OF ALL RESULTS FOR EDI----

#read in final search results
final_search <- read_csv("./data/final_search.csv") %>%
  select(-`...70`)
colnames(final_search)

#columns to drop:
#...70

#read in title screen results
title_screen <- read_csv("./data/final_search_title_screen.csv") %>%
  select(`Article Title`,`Title Screen`)

#read in initital screen results
init <- read_csv("./data/initial_screen_results.csv")

#read in search results with result number associated 
savedrecs <- read_csv("./data/savedrecs.csv") %>%
  select(`Result Number`,`Article Title`) %>%
  filter(complete.cases(.))
skipped <- read_csv("data/skipped_abstracts.csv") %>%
  select(`Result`,`Article Title`) %>%
  rename(`Result Number` = `Result`)
result_nums <- bind_rows(savedrecs,skipped)
pass_title_screen <- left_join(title_screen,result_nums) %>%
  filter(`Title Screen` == "y") %>%
  select(-`Title Screen`)

#join final search results with title screen results
edi0 <- left_join(final_search, title_screen)

#left-join with result numbers
edi1 <- left_join(edi0, pass_title_screen) %>%
  rename(result_num = `Result Number`)

#left-join with initial screen results
edi2 <- left_join(edi1, init, by = c("result_num"))

#read in matrix results
matrix <- read_csv("./data/matrix_analysis_results.csv") %>%
  select(-Title, -Authors, -Journal, -Year, -DOI, -Reviewer)

#left-join with matrix results
edi3 <- left_join(edi2, matrix, by = c("result_num"))
colnames(edi3)

#column name and order munging
colnames(edi3)[74] <- "uncertainty_present"
colnames(edi3)[88] <- "uncertainty_method"
colnames(edi3)[70] <- "title_screen"
edi4 <- edi3[,c(1:69,71,70,72:94)]
colnames(edi4)

write.csv(edi4, "./data/EDI/freshwater-forecasting-review-results.csv",row.names = FALSE)

# COVID-19 Leader Perceptions Registered Report (Lab log)
# Clara Colombatto  
# 11/26/2020

# Load packages
library(dplyr)
library(tidyr)
library(xlsx)

# Set working directory
setwd("/Users/claracolombatto/Dropbox/PhD/projects/current/moral_covid/RR code/")

# Get data
all_data <- read.csv("raw_data.csv", na.strings = "NOTAVAIL")
all_data$EndDate <- as.Date(all_data$EndDate)

summary <- data.frame(surveyName=unique(all_data$surveyName))

write.xlsx(summary, file="summary.xlsx", 
           sheetName="Index", 
           row.names=FALSE, append = FALSE)

for (date in sort(unique(all_data$EndDate))) {
  
  print(as.Date(date, origin = "1970-12-30"))
  raw_data <- filter(all_data, EndDate <= date)
  
  summary <- data.frame(surveyName=unique(raw_data$surveyName))
  
  # Exclusions
  raw_data %>%
    filter(# Remove Preview
      Status!="Survey Preview",
      # Completes only
      Finished==TRUE,
      # Consented
      consent_age == "Yes", consent_agree == "Yes") -> data
  
  # Exclude Duplicates
  data %>%
    group_by(IPAddress) %>%
    mutate(countIPtmp = n()) %>%
    group_by(rid) %>%
    mutate(countIDtmp = n()) %>%
    rowwise() %>% 
    mutate(countIP = ifelse(is.na(IPAddress), 1, countIPtmp),
           countID = ifelse(is.na(rid), 1, countIDtmp)) %>%
    ungroup() %>%
    select(-rid) -> data
  
  summary <- merge(summary, summarise(group_by(data, surveyName), NFinished=n(), .groups='drop'))
  
  ## Exclude attention check fails
  data <- filter(data, attention_check == "TikTok")
  summary <- merge(summary, summarise(group_by(data, surveyName), NPassAtt1=n(), .groups='drop'))
  
  data <- filter(data, attention_check2 == "Roses can suffer with pests like aphids and blackfly")
  summary <- merge(summary, summarise(group_by(data, surveyName), NPassAtt2=n(), .groups='drop'))
  
  ## Exclusion 1: IP/ID
  data <- filter(data, countIP == 1, countID == 1)
  
  ## Exclusion 2: Country
  data <- filter(data, (country != "Other" & !is.na(country)))
  
  ## Exclusion 3: >50% of qs
  data %>%
    ungroup() %>%
    select(subId, gender, age, contains("_trust"), OUS1:OUS9, 
           contains("_behav"), contains("_Support"), contains("_Moral"),
           beh_compr, self_comp, starts_with("covid_"),
           country, education, ses2, politics, religious, unicef) %>%
    mutate(count_na = rowSums(is.na(.)),
           respMoreHalfKey = if_else(count_na > 36/2, 1, 0)) %>%
    select(subId, respMoreHalfKey, count_na) %>%
    right_join(data, by='subId') -> data
  
  data <- filter(data, (respMoreHalfKey == 1 | is.na(respMoreHalfKey)))
  summary <- merge(summary, summarise(group_by(data, surveyName), NValid=n(), .groups='drop'))
  
  # Pass One Comp Check
  data %>%
    mutate(CompSCheck = if_else(self_comp %in% c("How much I trusted the mayor",
                                                 "How much I trusted the official"), 1, 0),
           CompSCheck = if_else(is.na(CompSCheck), 0, CompSCheck),
           CompBCheck =  if_else(beh_compr == "The leader can transfer the full donation to UNICEF or take some of the money for themselves.", 1, 0),
           CompBCheck = if_else(is.na(CompBCheck), 0, CompBCheck),
           nPassed = ifelse(CompSCheck == 1 & CompBCheck == 1, 2, 
                            ifelse(CompSCheck == 0 & CompBCheck == 0, 0, 1))) -> data
  
  data <- filter(data, nPassed>0)
  summary <- merge(summary, summarise(group_by(data, surveyName), NPassOne=n(), .groups='drop'))
  
  data %>%
    select(subId, EndDate, CompSCheck, CompBCheck, Decision, surveyName, 
           gender, age, education, ses2, politics, religious, country, 
           contains("_behav"), contains("_trust_"), 
           contains("_Support"))  %>% 
    gather(measure, answer, -c(subId:country)) %>%
    separate(measure, c("dilemma", "measure"), "_", 
             extra = "merge", fill="left")  %>%
    mutate(measure=gsub("C|D|_", "", measure)) %>%
    filter(!is.na(answer)) %>%
    spread(measure, answer) -> data_long
  
  data_long %>%
    filter(!is.na(trust1), !is.na(trust2)) %>% 
    ungroup() %>%
    select(-behav) %>%
    na.omit() %>%
    filter(CompSCheck == 1) %>%
    select(surveyName, subId) %>%
    unique() -> subs_selfrep
  
  summary <- merge(summary, summarise(group_by(subs_selfrep, surveyName), NValidSelfRep=n(), .groups='drop'))
  
  data_long %>%
    filter(!is.na(behav)) %>% 
    ungroup() %>%
    select(-trust1, -trust2) %>%
    na.omit() %>%
    filter(CompBCheck == 1) %>%
    select(surveyName, subId) %>%
    unique() -> subs_voting
  
  summary <- merge(summary, summarise(group_by(subs_voting, surveyName), NValidVoting=n(), .groups='drop'))
  
  sum(summary$NValidSelfRep)
  sum(summary$NValidVoting)
  
  data_yesterday <- filter(data, EndDate <= max(data$EndDate)-1)
  summary <- left_join(summary, summarise(group_by(data_yesterday, surveyName), NPassOneYesterday=n(), .groups='drop'))
  
  summary$NPassOneNew <- summary$NPassOne - summary$NPassOneYesterday
  
  summary[is.na(summary)] <- 0
  
  write.xlsx(summary, file="CLP_RR_LabLog.xlsx", 
             sheetName=as.character(max(data$EndDate)), 
             row.names=FALSE, append = TRUE)

}

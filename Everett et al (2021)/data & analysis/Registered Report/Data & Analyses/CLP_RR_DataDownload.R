# COVID-19 Leader Perceptions Registered Report (Data Download)
# Clara Colombatto  
# 02/02/2021

# Load packages
library(dplyr)
library(tidyr)
library(qualtRics)

# Set working directory
setwd("/Users/claracolombatto/Dropbox/PhD/projects/current/moral_covid/RR code/")

# Download data from Qualtrics

# Set Qualtrics credentials
qualtrics_api_credentials(api_key = 'REDACTED',
                          base_url = 'REDACTED', install=TRUE)

# Get relvant surveys
all_surveys() %>%
  filter(grepl("COVID Leader Perceptions RR", name)) %>%
  mutate(country = gsub('COVID Leader Perceptions RR - ', '', name)) %>%
  filter(country != "Master Survey", country != 'ClaraCSSTests',
         name != "COVID Leader Perceptions RR - Chile",
         name != "COVID Leader Perceptions RR - UK", 
         name != "COVID Leader Perceptions RR - U.S.",
         name != "COVID Leader Perceptions RR - Korea Respondi",
         name != "COVID Leader Perceptions RR - Korea",
         !grepl("Pilot",country), !grepl("OLD",country)) -> surveys

# Download data from each survey
raw_data <- data.frame()
var_names <- data.frame(1:400)
for (row in 1:nrow(surveys)) {
  name = surveys[row, "name"]
  print(name)
  fetch_survey(surveyID=surveys[row, "id"], 
               include_display_order = TRUE,
               save_dir = "raw data/",
               verbose = FALSE,
               label = TRUE,
               convert = FALSE,
               breakout_sets = FALSE,
               force_request = TRUE,
               import_id = FALSE) %>%
    as.data.frame()  -> survey
  
  write.csv(survey, paste("raw data/", surveys[row, "name"], ".csv", sep=''), 
            row.names = F, na="NOTAVAIL")
  
  # Uniform variable name for subject IDs
  if (name == "COVID Leader Perceptions RR - Israel" | 
      name == "COVID Leader Perceptions RR - Korea dataSpring"){
    survey %>%
      select(-rid) %>%
      rename("rid" = "uid") -> survey
  } else if (grepl("Prolific", name) == T){
    survey %>%
      rename("rid" = "PROLIFIC_PID") %>%
      select(-ProlificId) -> survey
  } 
  # Uniform variable name for survey names
  if (name == "CESS COVID Leader Perceptions RR - Chile") {
    surveyName = "Chile"
  } else if (name == "COVID Leader Perceptions RR - U.S. Prolific") {
    surveyName = "U.S."
  } else if (name == "COVID Leader Perceptions RR - UK Prolific") {
    surveyName = "U.K."
  } else if (name == "COVID Leader Perceptions RR - Korea dataSpring") {
    surveyName = "Korea"
  } else {
    surveyName = gsub("COVID Leader Perceptions RR - *", "", name)
  }
  
  # Append to one dataframe
  survey %>%
    cbind(surveyName = surveyName) %>%
    plyr::rbind.fill(raw_data) -> raw_data
  
  # Append to list of var names for later checking
  var_name <-  rep(NA, 400)
  var_name[1:length(colnames(survey))] <- colnames(survey)
  var_names[[surveyName]] <- var_name
  
}

raw_data %>%
  mutate(subId = rownames(.),
         rid = replace_na(rid, "NOTAVAIL")) -> raw_data

length(surveys$country)
unique(raw_data$surveyName)

# Save data
write.csv(raw_data, "raw_data.csv", row.names = F, na="NOTAVAIL")

# Check for variable consistency across surveys
var_names %>%
  select(-X1.400) %>%
  gather(country, DV) %>%
  drop_na() %>%
  group_by(DV) %>%
  summarise(counts=n()) %>%
  filter(counts != length(unique(surveys$name)))

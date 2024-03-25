# Pre-process raw data to analyzable data
# Load packages
library(tidyverse)
library(lubridate)
library(readr)

# Load raw data
trolley_raw <- read_csv("inst/extdata/trolley_raw.csv")

# Remove all trial runs and incomplete surveys and remove unnecessary variables
trolley_preprocessed <-
  trolley_raw %>%
  # Remove those who didn't finish the questionnaire
  filter(Progress >= 98) %>%
  # Remove all practice runs
  filter(str_detect(str_to_lower(practice), "false")) %>%
  # Remove the answers for a particular lab that has unflagged practice data
  filter(!(lab == "TUR_021" & StartDate < date("2020-04-22")),
         !(lab == "AUT_003" & StartDate < date("2020-06-18")),
         !(lab == "USA_095")) %>%
  # Remove all variables that are not needed
  select(
    -c(Status:RecordedDate,
       RecipientLastName:IND_006,
       second:LS,
       JPN_003_debrief,
       image1:image6,
       PHL_004_consent:PHL_003_consent,
       confirmCode,
       Q_URL,
       imgset,
       practice))


usethis::use_data(trolley_preprocessed, overwrite = TRUE)

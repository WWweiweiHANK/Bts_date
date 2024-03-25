# Prepare raw data form source data
# Load packages
remotes::install_version("qualtRics", version = "3.1.2")

library(tidyverse)
library(qualtRics)
library(jsonlite)
library(lubridate)

# Load system data objects
load("R/sysdata.rda")

# Read the API key from a file. The file should not be on github.
# This is just a text file with one line, that is the API key.
qualtrics_api_key <- read_lines("qualtrics_api_key.txt")

# Read survey_id data
qualtrics_survey_ids <-
  qualtrics_surveys %>%
  filter(survey_name != "PSA006_master")

qualtrics_api_credentials(api_key = qualtrics_api_key,
                          base_url = "https://lapsyde.eu.qualtrics.com")

# Read data from all Qualtrics surveys and merge them into one tibble
# WARNING, only runs with qualtRics 3.1.2!
trolley_raw <-
  qualtrics_survey_ids %>%
  mutate(data = map(survey_id ,~fetch_survey( surveyID = .x,
                                              include_display_order = TRUE,
                                              force_request = TRUE,
                                              label = FALSE,
                                              convert = FALSE) %>%
                      mutate(practice = as.character(practice)))) %>%
  unnest(data)

glimpse(trolley_raw)

# Save data as external datafile
write_csv(trolley_raw, "inst/extdata/trolley_raw.csv")


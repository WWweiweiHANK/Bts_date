# Add exclusion rules to pre-processed data
# Load packages
library(tidyverse)
library(jsonlite)
library(lubridate)

# Load preprocessed data
load("data/trolley_preprocessed.rda")

# Load metadata
load("R/sysdata.rda")

# Some country codes are not in standard iso3 format, we need those for recoding
custom_countries <-
  c("LEB" = "LBN",
    "BUL" = "BGR",
    "SPA" = "ESP",
    "SWT" = "CHE")

# Flag participants for inclusion based on the preregistration
trolley <-
  trolley_preprocessed %>%
  # Aggregate randomization variables
  mutate(scenario1 = case_when(FL_24_DO_Footbridgepole == 1 ~ "Pole",
                               FL_24_DO_Footbridgeswitch == 1 ~ "Switch",
                               TRUE ~ NA_character_),
         scenario2 = case_when(FL_22_DO_Standardswitch == 1 ~ "Standardswitch",
                               FL_22_DO_Standardfootbridge == 1 ~ "Standardfootbridge",
                               FL_22_DO_Loop == 1 ~ "Loop",
                               FL_22_DO_Obstaclecollide == 1 ~ "Obstaclecollide",
                               TRUE ~ NA_character_),
         # Flag careless respondents
         include_nocareless = careless_1 != 1 & careless_2 != 1 & careless_3 != 2,
         # Flag confused participants
         include_noconfusion = confusion != 3,
         # Flag those with familiarity of the topic
         include_nofamiliarity = familiarity <= 3,
         # Flag those with technical problems
         include_notechproblem = technical_problems != 2,
         # Flag those who did not fill the questionnaire on their native language
         include_nonativelang = native_language != 2,
         # Flag those who pass all exclusion criterion
         include_noproblem =  include_nofamiliarity &
           include_nocareless &
           include_noconfusion &
           include_notechproblem &
           include_nonativelang,
         # Flag those who pass all exclusion criterion but familiarity does not matter
         include_withoutfamiliarity = include_nocareless &
           include_noconfusion &
           include_notechproblem &
           include_nonativelang,
         # Flag those who pass all exclusion criterion but they are familiar with the tasks
         include_familiar = !include_nofamiliarity &
           include_nocareless &
           include_noconfusion &
           include_notechproblem &
           include_nonativelang) %>%
  # Flag those that are eligible to analysis in each study
  left_join(select(correct_answers, -scenario2) %>% drop_na(scenario1),
            by = c("scenario1")) %>%
  # Flag responses where respondents pass the attention check but none of the other criteria are applied
  mutate(include_study1a_attention = trolley_attention == trolley_answer,
         include_study1b_attention = speedboat_attention == speedboat_answer,
         # Flag responses where respondents pass all of the checks
         include_study1a = include_study1a_attention & include_noproblem,
         include_study1b = include_study1b_attention & include_noproblem,
         # Flag responses where participants pass all the checks but both familiar and not familiar respondents are included
         # (i.e., familiarity does not matter)
         include_study1a_withoutfamiliarity = include_study1a_attention & include_withoutfamiliarity,
         include_study1b_withoutfamiliarity = include_study1b_attention & include_withoutfamiliarity,
         # Flag responses where participants pass all the checks but only familiar respondents are included
         include_study1a_familiar = include_study1a_attention & include_familiar,
         include_study1b_familiar = include_study1b_attention & include_familiar) %>%
  select(-trolley_answer, -speedboat_answer) %>%
  left_join(select(correct_answers, -scenario1) %>% drop_na(scenario2),
            by = c("scenario2")) %>%
  # Flag responses where respondents pass the attention check but none of the other criteria are applied
  mutate(include_study2a_attention = trolley_attention == trolley_answer,
         include_study2b_attention = speedboat_attention == speedboat_answer,
         # Flag responses where respondents pass all of the checks
         include_study2a = include_study2a_attention & include_noproblem,
         include_study2b = include_study2b_attention & include_noproblem,
         # Flag responses where participants pass all the checks but both familiar and not familiar respondents are included
         # (i.e., familiarity does not matter)
         include_study2a_withoutfamiliarity = include_study2a_attention & include_withoutfamiliarity,
         include_study2b_withoutfamiliarity = include_study2b_attention & include_withoutfamiliarity,
         # Flag responses where participants pass all the checks but only familiar respondents are included
         include_study2a_familiar = include_study2a_attention & include_familiar,
         include_study2b_familiar = include_study2b_attention & include_familiar) %>%
  select(-trolley_answer, -speedboat_answer) %>%
  # Add processed variables
  mutate(country3 = str_extract(lab, "[A-Z]+") %>%
           # Some country codes are not standard iso3 codes; replace
           recode(., !!!custom_countries),
         # Some labs collected data in other countries; correct
         country3 = case_when(lab == "GBR_001" ~ "PAK",
                              lab == "GBR_031" ~ "BRA",
                              lab == "GBR_060" ~ "DNK",
                              TRUE ~ country3),
         # Add region, based on the survey name
         Region = str_remove(survey_name, "PSA006_"),
         # Age is a multiple choice question that starts(1) at 18
         Age = age_1 + 17,
         # The sex variable was renamed as Gender during the original analysis but this is erroneous
         # Since the original question asked about sex
         # Thus we comment this line out
         # Gender = sex,
         # select those that have a higher education
         edu_high = education_leve > 2,
         # Higher education is recorded differently in Germany and Austria
         edu_high_ger = education_level_germ > 4,
         `Higher education` = case_when(country3 %in% c("AUT", "DEU") ~ edu_high_ger,
                                        TRUE ~ edu_high)) %>%
  select(-edu_high, -edu_high_ger)

# Testing the dataset
trolley %>%
  filter(include_study2a_withoutfamiliarity) %>%
  nrow()

trolley %>%
  filter(include_allstudy_withoutfamiliarity) %>%
  nrow()

trolley %>%
  filter(include_study2a) %>%
  nrow()

trolley %>%
  filter(include_study2a_withoutfamiliarity) %>%
  select(trolley_3_rate, trolley_4_rate, trolley_5_rate, trolley_6_rate) %>%
  filter(!all(is.na(trolley_3_rate), is.na(trolley_4_rate), is.na(trolley_5_rate), is.na(trolley_6_rate))) %>%
  nrow()

trolley %>%
  filter(include_allstudy_withoutfamiliarity) %>%
  select(trolley_3_rate, trolley_4_rate, trolley_5_rate, trolley_6_rate) %>%
  filter(!all(is.na(trolley_3_rate), is.na(trolley_4_rate), is.na(trolley_5_rate), is.na(trolley_6_rate))) %>%
  nrow()

usethis::use_data(trolley, overwrite = TRUE)

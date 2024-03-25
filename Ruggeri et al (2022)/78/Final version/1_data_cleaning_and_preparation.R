
##########################################################################################################
## This document will reproduce the analysis of the "The globalizability of temporal discounting". 
## Author 1, Author2, ...
## Submitted for publication to Nature Human Behavior
## This document is preregistered at OSF. 
## The following functions are necessary to handle data wrangling as well as to compute the different scores.
##########################################################################################################


# This series of scripts will reproduce the analysis of the "The globalizability of temporal discounting". This document is preregistered at OSF. 
## This script will clean and prepare the datasets for model estimation

########################
# 0. Utility functions
########################

## The following functions are necessary to handle data wrangling as well as to compute the different scores.

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("scales")) install.packages("scales")

source("999_1_auxiliary_functions.R")

options(digits = 3)
options(scipen = 99)


########################
# 1. Data cleaning
########################

## In this section we upload the pilot dataset. 
### We include a identification number for each case, compute age, apply the attention check and estimate spending allocation in relative (%) terms. 
### We also select a main ethnic background for each individual (to be changed for allowing mixed ethnic background if used in analyses in the future). 
### We also allocate delay-speedup questions next to each other for future computations.
### We further clean variables not explored in these analyses but of potential interest for researchers (e.g., time_vs_money). 
dat_org <- read_csv(file = "999_2_data_original_1_10_2021.csv", 
                    col_types = cols(
                      "Q31_4_TEXT" = col_character(),
                      "Q31_5_TEXT" = col_character(),
                      "Q34_10_TEXT" = col_character(),
                      "Q34_1_TEXT" = col_character(),
                      "Q34_2_TEXT" = col_character(),
                      "Q34_5_TEXT" = col_character(),
                      "Q34_4_TEXT" = col_character(),
                      "Q34_6_TEXT"= col_character(),
                      "Q34_7_TEXT"= col_character(),
                      "Q34_12_TEXT" = col_character(), 
                      "Q34_23_TEXT" = col_character(), 
                      "Q34_16_TEXT" = col_character(), 
                      "Q34_19_TEXT" = col_character(), 
                      "Q34_12_TEXT - Parent Topics" = col_character(),
                      "Q34_12_TEXT - Topics" = col_character(),
                      "Q34_9_TEXT" = col_character(),
                      "Japan_pay" = col_character(),
                      "Q41" = col_character(),
                      "RecipientEmail" = col_character(),
                      "Province" = col_character(),
                      "Q34_21_TEXT" = col_character(),
                      "Q34_11_TEXT" = col_character(),
                      "Q_RecaptchaScore" = col_character()
                    ))

dat <- dat_org %>%
  #Data cleaning and recoding of categories for main variables. 
  rename(
    Gain550 = "Q2",
    Gain600 = "Q3",
    Gain750 = "Q4",
    Gain510 = "Q5",
    Gain505 = "Q6",
    Pay550 = "Q7",
    Pay600 = "Q8",
    Pay750 = "Q9",
    Pay510 = "Q10",
    Pay505 = "Q11",
    Gain5500 = "Q12",
    Gain6000 = "Q13",
    Gain7500 = "Q14",
    Gain5100 = "Q15",
    Gain5050 = "Q16",
    duration = "Duration (in seconds)",
    disc_debt = "Discretionary spend_2",
    disc_non_fin = "Discretionary spend_3",
    disc_savings = "Discretionary spend_4",
    disc_invest = "Discretionary spend_5",
    expectation = "Q24",
    bills = "Q25",
    income = "Q26_1",
    debt = "Q26_2",
    assets = "Q26_3",
    financial_2020 = "Q27",
    situation_debt = "Q28",
    child_situation = "Q29",
    Gender = "Q31",
    EducationCompleted = "Q32",
    Employment = "Q33",
    Ethnic = "Q34")   %>%
  
  mutate(Code = fct_recode(Residence, 
                           "ARG" = "Argentina",
                           "AUS" = "Australia",
                           "AUT" = "Austria",
                           "BEL" = "Belgium",
                           "BIH" = "Bosnia_and_Herzegovina",
                           "BRA" = "Brazil",
                           "BRG" = "Bulgaria",
                           "CAN" = "Canada",
                           "CHN" = "China",
                           "HRV" = "Croatia",
                           "CZE" = "Czechia",
                           "DNK" = "Denmark",
                           "EST" = "Estonia",
                           "EGY" = "Egypt",
                           "ETH" = "Ethiopia",
                           "FRA" = "France",
                           "GEO" = "Georgia",
                           "GHA" = "Ghana",
                           "DEU" = "Germany",
                           "IND" = "India",
                           "IDN" = "Indonesia",
                           "IRN" = "Iran",
                           "IRL" = "Ireland",
                           "ISR" = "Israel",
                           "ITA" = "Italy",
                           "JPN" = "Japan",
                           "JOR" = "Jordan",
                           "KAZ" = "Kazakhstan",
                           "KEN" = "Kenya",
                           "LBN" = "Lebanon",
                           "MYS" = "Malaysia",
                           "MEX" = "Mexico",
                           "MDA" = "Moldova",
                           "MNE" = "Montenegro",
                           "NPL" = "Nepal",
                           "NLD" = "Netherlands",
                           "NZL" = "New_Zealand",
                           "NGA" = "Nigeria",
                           "MKD" = "North_Macedonia",
                           "NOR" = "Norway",
                           "PAK" = "Pakistan",
                           "PAN" = "Panama",
                           "PRY" = "Paraguay",
                           "POL" = "Poland",
                           "PRT" = "Portugal",
                           "ROU" = "Romania",
                           "SRB" = "Serbia",
                           "SRP" = "Singapore",
                           "SVK" = "Slovakia",
                           "SVN" = "Slovenia",
                           "ZAF" = "South_Africa",
                           "KOR" = "South_Korea",
                           "ESP" = "Spain",
                           "SWE" = "Sweden",
                           "CHE" = "Switzerland",
                           "TUR" = "Turkey",
                           "UKR" = "Ukraine",
                           "GBR" = "United_Kingdom",
                           "USA" = "United_States",
                           "URY" = "Uruguay", 
                           "VNM" = "Vietnam" 
  )) %>%
  
  #https://datahelpdesk.worldbank.org/knowledgebase/articles/906519
  mutate(Cntry_income = fct_recode(Residence, 
                                   "Upper-middle" = "Argentina",
                                   "High-income" = "Australia",
                                   "High-income" = "Austria",
                                   "High-income" = "Belgium",
                                   "Upper-middle" = "Bosnia_and_Herzegovina",
                                   "Upper-middle" = "Brazil",
                                   "Upper-middle" = "Bulgaria",
                                   "High-income" = "Canada",
                                   "Upper-middle" = "China",
                                   "High-income" = "Croatia",
                                   "High-income" = "Czechia",
                                   "High-income" = "Denmark",
                                   "High-income" = "Estonia",
                                   "Lower-middle" = "Ethiopia",
                                   "Lower-middle" = "Egypt",
                                   "High-income" = "France",
                                   "Upper-middle" = "Georgia",
                                   "Lower-middle" = "Ghana",
                                   "High-income" = "Germany",
                                   "Lower-middle" = "India",
                                   "Lower-middle" = "Indonesia",
                                   "Lower-middle" = "Iran",
                                   "High-income" = "Ireland",
                                   "High-income" = "Israel",
                                   "High-income" = "Italy",
                                   "High-income" = "Japan",
                                   "Upper-middle" = "Jordan",
                                   "Upper-middle" = "Kazakhstan",
                                   "Lower-middle" = "Kenya",
                                   "Upper-middle" = "Lebanon",
                                   "Upper-middle" = "Malaysia",
                                   "Upper-middle" = "Mexico",
                                   "Upper-middle" = "Moldova",
                                   "Upper-middle" = "Montenegro",
                                   "Lower-middle" = "Nepal",
                                   "High-income" = "Netherlands",
                                   "High-income" = "New_Zealand",
                                   "Lower-middle" = "Nigeria",
                                   "Upper-middle" = "North_Macedonia",
                                   "High-income" = "Norway",
                                   "Lower-middle" = "Pakistan",
                                   "Upper-middle" = "Panama",
                                   "Upper-middle" = "Paraguay",
                                   "High-income" = "Poland",
                                   "High-income" = "Portugal",
                                   "High-income" = "Romania",
                                   "Upper-middle" = "Serbia",
                                   "High-income" = "Singapore",
                                   "High-income" = "Slovakia",
                                   "High-income" = "Slovenia",
                                   "Upper-middle" = "South_Africa",
                                   "High-income" = "South_Korea",
                                   "High-income" = "Spain",
                                   "High-income" = "Sweden",
                                   "High-income" = "Switzerland",
                                   "Upper-middle" = "Turkey",
                                   "Lower-middle" = "Ukraine",
                                   "High-income" = "United_Kingdom",
                                   "High-income" = "United_States",
                                   "High-income" = "Uruguay", 
                                   "Lower-middle" = "Vietnam" 
  ))  %>%   
  
  mutate(Cntry_income = relevel(Cntry_income, "Lower-middle", "Upper-middle", "High-income")) %>% 
  
  mutate(Employment = fct_recode(Employment, 
                                 
                                 "Employed full-time" = "Employed full-time  (salary and full benefits)",
                                 "Employed full-time" = "Employed full-time (25+ hours per week)",
                                 "Employed full-time" = "Employed full-time (full salary)",
                                 "Employed full-time" = "Employed full-time (salary and full benefits)" ,
                                 
                                 "Employed part-time" = "Employed part-time (hourly or only partial wage and benefits)",
                                 "Employed part-time" = "Employed part-time (hourly or only partial wage and limited benefits",
                                 "Employed part-time" = "Employed part-time (hourly or only partial wage)",
                                 "Employed part-time" = "Employed part-time (less than 25 hours per week)",
                                 "Employed part-time" = "Employed part-time (hourly or only partial wage and limited benefits)",
                                 
                                 "Military" = "Full-time National Serviceman (NSF)",
                                 "Military" = "Mandatory military service",
                                 "Military" = "Mandatory military service (excluding career military)",
                                 "Military" = "Serving mandatory military service",
                                 
                                 "Not in paid employment (personal reasons)" = "Not in paid employment (unable to work due to health/personal reasons)", 
                                 "Not in paid employment (personal reasons)" = "Not in paid employed (unable to work due to health/personal reasons)", 
                                 "Not in paid employment (personal reasons)" = "Not in paid employment (by choice)",
                                 
                                 "Not in paid employment (looking)" = "Not in paid employed (looking for work)",
                                 "Not in paid employment (looking)" = "Not in paid employment (looking for work)", 
                                 "Not in paid employment (personal reasons)" = "Not in paid employment (unable to work due to health/personal reasons",
                                 
                                 "Retired" = "Retr",
                                 
                                 "Self-employed" = "self-employed",
                                 "Self-employed" = "Self-Employed"
                                 
  )) %>% 
  
  
  mutate(EducationCompleted = fct_recode(EducationCompleted, 
                                         
                                         "No formal ed." = "No formal education",
                                         "No formal ed." = "No Formal Education Completed",
                                         
                                         "Primary ed." = "Primary (Elementary/Middle School)",
                                         "Primary ed." = "Primary or secondary", 
                                         "Primary ed." = "Primary school",
                                         "Primary ed." = "Intermediate school",
                                         "Primary ed." = "Primary/Elementary School",
                                         
                                         "Secondary ed." = "High School",
                                         "Secondary ed." = "High school education (grammar or vocational)",
                                         "Secondary ed." = "Short degree",
                                         "Secondary ed." = "Secondary",
                                         "Secondary ed." = "Secondary (High School)",
                                         "Secondary ed." = "Secondary school",
                                         "Secondary ed." = "Secondary school (high school)",
                                         "Secondary ed." = "Secondary school (high school) full matriculation",
                                         "Secondary ed." = "Secondary school (high school) no matriculation", 
                                         "Secondary ed." = "Secondary school (with maturita)",  
                                         "Secondary ed." = "Secondary school (without maturita)",  
                                         "Secondary ed." = "Middle School",  
                                         
                                         
                                         "Technical ed." = "Medium-length degree",
                                         "Technical ed." = "Post-secondary diploma",
                                         "Technical ed." = "Technical / Non-university higher education",
                                         "Technical ed." = "Tertiary professional school", 
                                         "Technical ed." = "Vocational / trade school",  
                                         "Technical ed." = "Vocational education based on basic education",   
                                         "Technical ed." = "Vocational education based on secondary education (high school",   
                                         "Technical ed." = "Vocational school",   
                                         "Technical ed." = "Vocational School",   
                                         "Technical ed." = "Vocational school, 1-year studies, or equivalent",     
                                         "Technical ed." = "Vocational School, Trade School, or Community College",     
                                         
                                         "Bachelor" = "Bachelor's (academic or applied studies)",
                                         "Bachelor" = "Bachelor's degree",
                                         "Bachelor" = "College or university (Bachelor or equivalent)",
                                         "Bachelor" = "College or university (Bachelors or equivalent)",
                                         "Bachelor" = "College or university (Bachelor's or equivalent)",
                                         "Bachelor" = "Higher Education (Bachelors, Masters or Doctoral degree)",
                                         "Bachelor" =  "Incomplete Higher education", 
                                         "Bachelor" =  "University", 
                                         
                                         "Graduate" = "Graduate (Masters or equivalent)",
                                         "Graduate" = "Graduate (Master's or equivalent)",
                                         "Graduate" = "Graduate degree or MBA",
                                         "Graduate" = "Post-graduate education (master's or specialized education)",
                                         "Graduate" = "Postgraduate (Masters or equivalent)",
                                         
                                         "MBA" = "MBA",
                                         
                                         "PhD" = "Doctoral degree",
                                         "PhD" = "Doctoral Degree",
                                         "PhD" = "Doctoral degree or equivalent",
                                         "PhD" = "PhD / Doctoral degree",
                                         
  )) %>% 
  
  mutate(Gender = fct_recode(Gender, 
                             "Female" = "Woman",
                             "Male" = "Man", 
                             "Other" = "I prefer to declare myself as:", 
                             "Other" = "I prefer to use:", 
                             "Other" = "I prefer to use" 
  )) %>% 
  
  select_all(funs(gsub(" ", "_", .))) %>% 
  select_all(funs(gsub("-", "_", .))) %>% 
  mutate(duration = as.numeric(duration)) %>%
  mutate(Progress = as.numeric(Progress)) %>%
  mutate (Age = 2021 - as.numeric(Q30)) %>%
  
  ## Collapse all open-text responses to ethnic background and gender identification into a single column 
  mutate(Ethinic_text = coalesce(Q34_16_TEXT, Q34_12_TEXT, Q34_10_TEXT, 
                                 Q34_1_TEXT, Q34_19_TEXT, Q34_9_TEXT, Q34_6_TEXT,
                                 Q34_12_TEXT___Parent_Topics, Q34_12_TEXT___Topics, 
                                 Q34_23_TEXT, Q34_5_TEXT, Q34_21_TEXT, Q34_11_TEXT)) %>% 
  
  mutate(Other_gender = coalesce(Q31_3_TEXT, Q31_4_TEXT, Q31_5_TEXT)) %>% 
  
  
  mutate(Risk_preference = case_when(
    str_detect(Risk_preference, "A 25% chance of") ~ 4,              
    str_detect(Risk_preference, "A 50% chance of") ~ 3,              
    str_detect(Risk_preference, "A 67% chance of") ~ 2,              
    str_detect(Risk_preference, "A 75% chance of") ~ 1,              
    str_detect(Risk_preference, "Guarante") ~ 0)) %>%   
  
  mutate(Interval_markup = fct_recode(Interval_markup, 
                                      "Receive $5000 right now" = "$5,000 right now",
                                      "Receive $5000 right now" = "$5000 right now",
                                      "Receive $5000 right now" = "Receive $5,000 right now",
                                      
                                      "Receive $500 per month for 12 months" = "$500 per month for 12 months",
                                      
                                      "Receive $6000 in 12 months" = "$6,000 in 12 months",
                                      "Receive $6000 in 12 months" = "$6000 in 12 months", 
                                      "Receive $6000 in 12 months" = "Receive $6,000 in 12 months" 
  )) %>% 
  
  mutate(Time_vs_Money = fct_recode(Time_vs_Money, 
                                    "One extra week of salary for no additional work in 2021" = "One extra week of salary for no additional work in 2013e.c.",
                                    "One extra week of salary for no additional work in 2021" = "One extra week of salary for no additional work in year 1400",
                                    "One extra week of salary for no additional work in 2021" = "One extra week of salary with no additional work in 2021",
                                    
                                    "One extra week of vacation with no change in salary in 2021" = "One extra week of holiday with no change in salary in 2021",
                                    "One extra week of vacation with no change in salary in 2021" = "One extra week of vacation with no change in salary in year 1400",
                                    "One extra week of vacation with no change in salary in 2021" = "One extra week of holidays with no change in salary in 2021",
                                    "One extra week of vacation with no change in salary in 2021" = "One extra week of vacation with no change in salary in 2013e.c.",
                                    "One extra week of vacation with no change in salary in 2021" = "One extra week of vacation without any change in the salary in 2021",
                                    
                                    "Two extra weeks of salary for no additional work in 2022" = "Two extra weeks of salary for no additional work in 2014e.c.",
                                    "Two extra weeks of salary for no additional work in 2022" = "Two extra weeks of salary for no additional work in 2022",
                                    "Two extra weeks of salary for no additional work in 2022" = "Two extra weeks of salary for no additional work in year 1401",
                                    "Two extra weeks of salary for no additional work in 2022" = "Two extra weeks of salary without any additional work in 2022",
                                    
                                    
                                    "Two extra week of vacation with no change in salary in 2022" = "Two extra weeks of holiday with no change in salary in 2022",
                                    "Two extra week of vacation with no change in salary in 2022" = "Two extra weeks of holidays with no change in salary in 2022",
                                    "Two extra week of vacation with no change in salary in 2022" = "Two extra weeks of vacation with no change in salary in 2014e.c.",
                                    "Two extra week of vacation with no change in salary in 2022" = "Two extra weeks of vacation with no change in salary in 2022",
                                    "Two extra week of vacation with no change in salary in 2022" = "Two extra weeks of vacation with no change in salary in year 1401",
                                    "Two extra week of vacation with no change in salary in 2022" = "Two extra weeks of vacation without any change in the salary in 2022")) %>%
  
  ### We add the initial data quality checks. Those include pass the attention check, minimum and maximum response times and minimum progress of 90%
  filter(
    str_detect(Attention_check, "PASS"),
    str_detect(Status, "IP"),
    duration > (median(duration) - mad(duration)*3),
    duration > 120,
    Progress > 90) %>% 
  
  ### We remove implausible responses for age and income 
  mutate (income = replace(income, which(income <0), NA),
          debt = replace(debt, which(debt <0), NA),
          assets = replace(assets, which(assets <0), NA)) %>% 
  
  filter(is.na(Age)|Age<100) %>% 
  
  filter(is.na(Other_gender)|
           str_detect(replace_na(Other_gender,""), "gender")|
           str_detect(replace_na(Other_gender,""), "Gender")|
           str_detect(replace_na(Other_gender,""), "NB")|
           str_detect(replace_na(Other_gender,""), "No ")|
           str_detect(replace_na(Other_gender,""), "No")|
           str_detect(replace_na(Other_gender,""), "no ")|
           str_detect(replace_na(Other_gender,""), "non")|
           str_detect(replace_na(Other_gender,""), "rans")) %>% 
  
  ### We clean the dataset to keep only the most relevant columns and in the desired order.
  distinct(ResponseId, .keep_all = TRUE) %>% 
  relocate(Delay_speedup_2, .after = Delay_speedup_1) %>% 
  
  select(-StartDate, -EndDate, -Status, -Finished, -RecordedDate, 
         -RecipientLastName, -RecipientFirstName, -RecipientEmail,
         -ExternalReference,
         -DistributionChannel, -Consent, 
         -Attention_check, -duration,
         -starts_with("Q34"), 
         -starts_with("Q31"))

### We categorize individual background and countries by different criteria. These are not used in the current analysis (e.g., Minority groups), but of interest in future analyses.

dat <- dat %>% 
  
  ## Represented majority groups (over 40% and groups over 10%)
  mutate(Majority = case_when( 
    
    Code == "DNK" ~ NA_character_,
    Code == "DEU" ~ NA_character_,
    Code == "FRA" ~ NA_character_,
    Code == "NLD" ~ NA_character_,
    Code == "NOR" ~ NA_character_,
    Code == "SWE" ~ NA_character_,
    Code == "CHE" ~ NA_character_,
    
    str_detect(Ethnic,"Immigrant") ~ "Immigrant",
    str_detect(Ethnic,"immigrant") ~ "Immigrant",
    str_detect(Ethnic,"another country") ~ "Immigrant",
    str_detect(Ethnic,"Born outside") ~ "Immigrant",
    str_detect(Ethnic,"born outside") ~ "Immigrant",
    str_detect(Ethnic,"Foreign-born") ~ "Immigrant",
    str_detect(Ethnic,"Foreign-born") ~ "Immigrant",
    str_detect(Ethnic,"Foreigner") ~ "Immigrant",
    str_detect(Ethnic, "not to answer") ~ NA_character_,
    
    
    Code == "ARG" & Ethnic == "White" ~ "Majority",
    Code == "AUS" & Ethnic == "Australian" ~ "Majority",
    Code == "AUT" & Ethnic == "White" ~ "Majority",
    Code == "BIH" & Ethnic == "Bosniak" ~ "Majority",
    Code == "BEL" & Ethnic == "I was born in Belgium and so were my (grand)parents" ~ "Majority",
    Code == "BRA" & Ethnic == "White" ~ "Majority",
    Code == "BRA" & Ethnic == "Brazilian" ~ "Majority",
    Code == "BRA" & Ethnic == "Brazilian,White" ~ "Majority",
    Code == "BRG" & Ethnic == "Bulgarian" ~ "Majority",
    Code == "CAN" & Ethnic == "White" ~ "Majority",
    Code == "CHN" & Ethnic == "Han" ~ "Majority",
    Code == "HRV" & Ethnic == "Croat" ~ "Majority",
    Code == "CZE" & Ethnic == "Czech" ~ "Majority",
    Code == "EST" & Ethnic == "Estonian" ~ "Majority",
    Code == "ETH" & Ethnic == "Oromo" ~ "Majority",
    Code == "ETH" & Ethnic == "Amhara" ~ "Majority",
    Code == "EGY" & Ethnic == "Egyptian" ~ "Majority",
    Code == "ETH" & Ethnic == "Oromo,Amhara" ~ "Majority",
    Code == "FRA" & Ethnic == "Citizen of France" ~ "Majority",
    Code == "GEO" & Ethnic == "Georgian" ~ "Majority",
    Code == "GHA" & Ethnic == "Akan" ~ "Majority",
    Code == "IND" & str_detect(Ethnic, "Hindu") ~ "Majority",
    Code == "IDN" & Ethnic == "Javanese" ~ "Majority",
    Code == "IDN" & Ethnic == "Sundanese" ~ "Majority",
    Code == "IDN" & Ethnic == "Javanese,Sundanese" ~ "Majority",
    Code == "IRN" & Ethnic == "Fars" ~ "Majority",
    Code == "IRL" & Ethnic == "White Irish" ~ "Majority",
    Code == "ISR" & str_detect(Ethnic,"Hiloni") ~ "Majority",
    Code == "ISR" & str_detect(Ethnic,"Ashkenazi") ~ "Majority",
    Code == "ITA" & Ethnic == "Italian" ~ "Majority",
    Code == "JPN" & Ethnic == "Japanese" ~ "Majority",
    Code == "JOR" & Ethnic == "Arab/Middle Eastern" ~ "Majority",
    Code == "KAZ" & Ethnic == "Kazakh" ~ "Majority",
    Code == "KEN" & Ethnic == "Kikuyu" ~ "Majority",
    Code == "KEN" & Ethnic == "Luo" ~ "Majority",
    Code == "KEN" & Ethnic == "Luhya" ~ "Majority",
    Code == "KEN" & Ethnic == "Kalenjin" ~ "Majority",
    Code == "KEN" & Ethnic == "Kamba" ~ "Majority",
    Code == "KOR" & Ethnic == "Korean citizen without migration background (First or second generation)" ~ "Majority",
    Code == "LBN" & Ethnic == "Lebanese" ~ "Majority",
    Code == "MYS" & Ethnic == "Bumiputera" ~ "Majority",
    Code == "MYS" & Ethnic == "Chinese" ~ "Majority",
    Code == "MYS" & Ethnic == "Bumiputera,Chinese" ~ "Majority",
    Code == "MEX" & Ethnic == "White" ~ "Majority",
    Code == "MDA" & Ethnic == "Moldavian/Romanian" ~ "Majority",
    Code == "MNE" & Ethnic == "Montenegrins" ~ "Majority",
    Code == "NPL" & Ethnic == "Brahman" ~ "Majority",
    Code == "NPL" & Ethnic == "Chhetri " ~ "Majority",
    Code == "NZL" & Ethnic == "New Zealand European" ~ "Majority",
    Code == "NGA" & Ethnic == "Yoruba" ~ "Majority",
    Code == "NGA" & Ethnic == "Igbo" ~ "Majority",
    Code == "NGA" & Ethnic == "Hausa" ~ "Majority",
    Code == "MKD" & Ethnic == "Macedonians" ~ "Majority",
    Code == "NOR" & Ethnic == "Born in Norway to Norwegian-born parents" ~ "Majority",
    Code == "PAK" & Ethnic == "Punjabi" ~ "Majority",
    Code == "PAK" & Ethnic == "Punjabi,Pashtun" ~ "Majority",
    Code == "PAK" & Ethnic == "Punjabi,Sindhi" ~ "Majority",
    Code == "PAK" & Ethnic == "Pashtun" ~ "Majority",
    Code == "PAK" & Ethnic == "Sindhi" ~ "Majority",
    Code == "PAN" & Ethnic == "White" ~ "Majority",
    Code == "PRT" & Ethnic == "White" ~ "Majority",
    Code == "PRY" & Ethnic == "White" ~ "Majority",
    Code == "POL" & Ethnic == "White" ~ "Majority",
    Code == "POL" & Ethnic == "Polish" ~ "Majority",
    Code == "ROU" & Ethnic == "Romanian" ~ "Majority",
    Code == "SRB" & Ethnic == "Serbian" ~ "Majority",
    Code == "SRP" & Ethnic == "Chinese" ~ "Majority",
    Code == "SVK" & Ethnic == "Slovak" ~ "Majority",
    Code == "SVN" & Ethnic == "Slovenian" ~ "Majority",
    Code == "ZAF" & Ethnic == "African" ~ "Majority",
    Code == "ESP" & Ethnic == "White" ~ "Majority",
    Code == "ESP" & Ethnic == "Hispanic" ~ "Majority",
    Code == "TUR" & Ethnic == "Turkish Citizen" ~ "Majority",
    Code == "UKR" & Ethnic == "Ukrainian" ~ "Majority",
    Code == "GBR" & Ethnic == "White (English/Welsh/Scottish/Northern Irish/British)" ~ "Majority",
    Code == "GBR" & Ethnic == "White (Irish)" ~ "Majority",
    Code == "USA" & Ethnic == "White" ~ "Majority",
    Code == "URY" & Ethnic == "White" ~ "Majority",
    Code == "VNM" & Ethnic == "Kinh" ~ "Majority",
    
    
    str_detect(Ethnic,"Mixed") ~ "Mixed race",
    str_detect(Ethnic,",") ~ "Mixed race",
    str_detect(Ethnic,"Other") ~  NA_character_,
    is.na(Ethnic) ~ NA_character_,
    
    TRUE ~ "Minority"))


## we resolve an issue with incorrect wording from the Georgian data (see Supplementary information). These cases are identified in the variable named "Deviations" (which also separates paid and unpaid Japan samples)
set.seed(10)
x <- levels(as.factor(dat$Pay750))
prob <- prop.table(table(dat$Pay750))
dat[dat$Code == "GEO" & dat$Pay600 == "Paying $600 in 12 months" & !is.na(dat$Pay600) & is.na(dat$Pay750), "Pay750"] <- sample(rep(x, round(34*prob)))
ids <- which(dat$Code == "GEO" & dat$Pay600 == "Paying $600 in 12 months" & !is.na(dat$Pay600) & is.na(dat$Pay750))
dat$Japan_pay[ids] <- "Georgian_imputted"
dat <- dat %>% 
  rename(Deviations = "Japan_pay") %>% 
  mutate(Deviations, fct_recode(Deviations, 
                                "Nopay_Japan" = "No", 
                                "Payed_Japan" = "Yes"))
rm(x, prob, ids)

## We prepare the data for fixing an second issue regarding responses from Estonia (see Supplementary information)
dat[dat$Code == "EST", "Common_difference"] <- "Receiving $500 in 12 months (1 year)"
dat[dat$Code == "EST", "Subadditivity"] <- "Receiving $500 right now"

## Add country information

### We incorporate GDP, GINI and average net individual income as country-level information to the dataset. Data is obtained for each country from the World Bank website.
dat_glob <- read_delim("999_3_country_information.csv", ";", escape_double = FALSE, trim_ws = TRUE)
dat <- merge(dat, dat_glob, by= "Residence", all.x=TRUE)
dat <- rename(dat, Country_income = Adjusted_income)
rm(dat_glob)


dat <- dat %>% 
  group_by(Residence) %>% 
  
  ### we expending rates to proportions
  mutate (disc_debt = as.numeric(disc_debt)/mean(QuestionTot)) %>%
  mutate (disc_non_fin = as.numeric(disc_non_fin)/mean(QuestionTot)) %>%
  mutate (disc_savings = as.numeric(disc_savings)/mean(QuestionTot)) %>%
  mutate (disc_invest = as.numeric(disc_invest)/mean(QuestionTot)) %>%
  
  ### we standardize income, debt and asssets values to dollars  
  mutate (income = as.numeric(income) * mean(as.numeric(Coin_to_dollar))) %>% 
  mutate (debt = as.numeric(debt) * mean(as.numeric(Coin_to_dollar))) %>% 
  mutate (assets = as.numeric(assets) * mean(as.numeric(Coin_to_dollar))) %>% 
  
  ungroup() %>%  
  
  #Remove implausible responses     
  filter(income < 1*10^8) %>% 
  filter(debt < 1*10^8) %>% 
  filter(assets < 1*10^8)


##  We clean the data an adapt the questions to be processed in future analysis. 
### The main issue to be considered is that different individual see a different number of questions with different options depending on their own decisions. 
### For each question see, we compute individual choices (i.e., 0 = sooner option chosen, 1 = later option chosen). 
### We create a new dataset (dat_item) including item characteristics (i.e., amount presented for the sooner and later rewards, delay presented for sooner and later rewards, question identifier, block identifier). 

dat <- dat %>% 
  group_by(Residence) %>%
  mutate (pdev = case_when(
    (str_detect(child_situation, "poor") | str_detect(child_situation, "Poor")) & (income >= Country_income) ~ 3 ,
    (str_detect(child_situation, "poor") | str_detect(child_situation, "Poor")) & (income < Country_income) ~ 1,
    TRUE ~ 2
  )) %>% 
  ungroup() %>% 
  mutate(pdev = case_when(
    pdev == 1 ~ "Below average",
    pdev == 2 ~ "Above average",
    TRUE ~ "PositiveDev")) %>% 
  group_by(ResponseId) %>% 
  mutate(indineq = income - Country_income) %>% 
  ungroup()


### We clean the data an adapt the questions to be processed in future analysis. We take into consideration that different individuals see a different number of questions with different options depending on their own decisions (i.e., we have an adaptative survery). For each question see, we compute individual choices (i.e., 0 = sooner option chosen, 1 = later option chosen). We create a new dataset (dat_item) including item characteristics (i.e., amount presented for the sooner and later rewards, delay presented for sooner and later rewards, question identifier, block identifier). 

dat_item <- 
  dat %>% 
  arrange(ResponseId) %>%   
  mutate(amount_later16 = case_when(
    Gain550 == "Receiving $550 in 12 months" & Gain510 == "Receiving $500 right now" ~ 550,
    Gain600 == "Receiving $600 in 12 months" ~ 600,
    Gain750 == "Receiving $500 right now" | Gain750 == "Receiving $750 in 12 months" ~ 750,
    Gain510 == "Receiving $510 in 12 months" & Gain505 == "Receiving $500 right now" ~ 510,
    Gain505 == "Receiving $505 in 12 months" ~ 505)) %>% 
  
  mutate(amount_later17 = case_when(
    Gain550 == "Receiving $550 in 12 months" & Gain510 == "Receiving $500 right now" ~ 600,
    Gain600 == "Receiving $600 in 12 months" ~ 700,
    Gain750 == "Receiving $750 in 12 months" | Gain750 == "Receiving $500 right now" ~ 1000,
    Gain510 == "Receiving $510 in 12 months" & Gain505 == "Receiving $500 right now" ~ 520,
    Gain505 == "Receiving $505 in 12 months" ~ 510)) %>% 
  
  
  mutate(amount_later18 = case_when(
    amount_later16 == 505  ~ 505,
    amount_later16 == 510  ~ 510,
    amount_later16 == 550  ~ 550,
    amount_later16 == 600  ~ 600,
    amount_later16 == 750  ~ 750
  )) %>% 
  
  mutate(amount_later19 = case_when(
    amount_later16 == 505  ~ 505,
    amount_later16 == 510  ~ 510,
    amount_later16 == 550  ~ 550,
    amount_later16 == 600  ~ 600,
    amount_later16 == 750  ~ 750
  )) %>% 
  
  pivot_longer(Gain550:Delay_speedup_2, 
               names_to = c("question")) %>%
  
  mutate (delay_sooner = rep(c(rep(0, 15), 12, 0, 0, 0), length(unique(ResponseId))),
          delay_later  = rep(c(rep(12, 15), 24, 24, 12, 12),length(unique(ResponseId))),
          amount_sooner = rep(c(rep(500, 10), 
                                rep(5000, 5),
                                rep(500, 4)), length(unique(ResponseId)))) %>%
  
  mutate (amount_later = case_when(
    question == "Gain550" | question == "Pay550" ~ 550,
    question == "Gain600" | question == "Pay600" ~ 600,
    question == "Gain750" | question == "Pay750" ~ 750,
    question == "Gain510" | question == "Pay510"~ 510,
    question == "Gain505" | question == "Pay505"~ 505,
    question == "Gain5500" ~ 5500,
    question == "Gain6000" ~ 6000,
    question == "Gain7500" ~ 7500,
    question == "Gain5100" ~ 5100,
    question == "Gain5050" ~ 5050,
    question == "Common_difference" ~ amount_later16,
    question == "Subadditivity" ~ amount_later17,
    question == "Delay_speedup_1" ~ amount_later18,
    question == "Delay_speedup_2" ~ amount_later19
  )) %>%
  
  
  mutate (question = paste("Q",rep(1:ncol(select(dat, Gain550:"Delay_speedup_2")), nrow(dat)), sep ="")) %>%
  mutate (question_num = as.numeric(gsub("Q", "", question))) %>%
  
  mutate ( block = case_when(
    question_num < 6 ~ "block1",
    question_num >= 6 & question_num < 11 ~ "block2",
    question_num >= 11 & question_num < 16 ~ "block3",
    question_num == 16 ~ "anom1",
    question_num == 17 ~ "anom2",
    question_num == 18 ~ "anom3",
    question_num == 19 ~ "anom3",
  )) %>% 
  
  mutate (choice = case_when(
    is.na(value) ~ NA_real_,
    str_detect(value, "500 right now") ~ 0,
    str_detect(value, "5000 right now") ~ 0,
    str_detect(value, "5500 in 12 months") ~ 1,
    str_detect(value, "7500 in 12 months") ~ 1,
    str_detect(value, "500 in 12 months") ~ 0,
    str_detect(value, "Reduce") ~ 0,
    TRUE  ~ 1
  )) %>% 
  
  rename(choice_text = "value") %>% 
  relocate(choice_text, .after = choice) %>%
  
  select(-amount_later16, -amount_later17, -amount_later18, -amount_later19) %>%
  drop_na(choice) %>% 
  mutate_if(is.character,as.factor)


###############################
# 2. Computing anomalies/scores
###############################

## We estimate the anomalies rates for the five anomalies (present bias = presbias; absolute magnitude = absolmag, gain-loss asymmetry = gainloss; delay-speedup = delayspeed; subadditivity = subaddit). 
## We use answers to the three baseline sets of questions plus the answers to the common difference, subadditivity, and delay-speedup items to compute the temporal discount scores.

## Computing the rates of the five anomalies

presbias <- as.numeric(get_latest_answer(dat_item, 6)$choice != get_second_choices(dat_item,"Q16"))
absolmag <- as.numeric(get_latest_answer(dat_item, 6)$choice != as.numeric(get_latest_answer(dat_item, 16)$choice))
gainloss <- as.numeric(get_latest_answer(dat_item, 6)$choice == as.numeric(get_latest_answer(dat_item, 11)$choice))
delayspeed <- as.numeric(get_latest_answer(dat_item, 6)$choice != get_second_choices(dat_item,"Q19"))
subaddit_all <- data.frame (sub1 = as.numeric(get_latest_answer(dat_item, 6)$choice + 
                                                get_second_choices(dat_item,"Q16")),
                            sub2 = get_second_choices(dat_item,"Q17"))

subaddit <-  mutate(subaddit_all, sub = case_when(
  sub1 == 0 & sub2 == 1 ~ 1,
  sub1 == 2 & sub2 == 0 ~ 1,
  TRUE ~ 0))$sub

anomalies_data <- list()


## We apply the function fixer_anom to estimate whether the anomalies are observed and whether those are consistent with the theory or not.
## Given a first and a second set of responses, plus an ID identifier, it will classify the pattern of responses in anomaly/not anomaly and in the first case, whether it is consistent or not.
## In the second case (not anomaly), it will also inform whether the first decision was a sooner or a later choice.
anomalies_data[[1]] <- fixer_anom (data.frame(fc = get_latest_answer(dat_item, 6)$choice,
                                              sc =  get_second_choices(dat_item,"Q16"),
                                              ResponseId = unique(dat_item$ResponseId)), type = "presbias")

anomalies_data[[2]] <- fixer_anom (data.frame(fc = get_latest_answer(dat_item, 6)$choice,
                                              sc =  as.numeric(get_latest_answer(dat_item, 16)$choice),
                                              ResponseId = unique(dat_item$ResponseId)), type = "absolmag")

anomalies_data[[3]] <- fixer_anom (data.frame(fc = get_latest_answer(dat_item, 6)$choice,
                                              sc =  as.numeric(get_latest_answer(dat_item, 11)$choice),
                                              ResponseId = unique(dat_item$ResponseId)), type = "gainloss")

anomalies_data[[4]] <- fixer_anom (data.frame(fc =  get_latest_answer(dat_item, 6)$choice,
                                              sc =  get_second_choices(dat_item,"Q19"),
                                              ResponseId = unique(dat_item$ResponseId)), type = "delay")

anomalies_data[[5]] <- fixer_anom (data.frame(fc = subaddit_all$sub1,
                                              sc =  subaddit_all$sub2,
                                              ResponseId = unique(dat_item$ResponseId)), type = "subaddit")

anomalies_congruent <- anomalies_data %>% 
  
  ### We are only interested in individual responses
  reduce(left_join, by = "ResponseId") %>%
  rename ( "presbias" = "response.x",
           "absolmag" = "response.y",
           "gainloss" = "response.x.x",
           "delayspeed" = "response.y.y",
           "subaddit" = "response") %>% 
  
  select(ResponseId, presbias, absolmag, gainloss, delayspeed, subaddit) %>% 
  mutate_at(vars(presbias, absolmag, gainloss, delayspeed, subaddit),
            funs(case_when(
              . == "Congruent" ~ 1,
              TRUE ~ 0)))

##  We calculate the temporal discount scores from responses to each block plus items Q17 to Q19 (anomalies)
### Careful with reversed scores when estimating the scores

scores <- get_scores(get_latest_answer(dat_item, 6))$score +
  5 -get_scores(get_latest_answer(dat_item, 11))$score +
  get_scores(get_latest_answer(dat_item, 16))$score +
  1-get_second_choices(dat_item, "Q16") +
  1-get_second_choices(dat_item, "Q17") +
  get_second_choices(dat_item, "Q18") +
  get_second_choices(dat_item, "Q19")

### We merge these scores and the anomalies rates with the original dataset 
q <- dat_item %>% 
  count(ResponseId) %>% 
  arrange()        

dat_item <- dat_item %>% 
  arrange(ResponseId) %>%
  mutate (score = rep(scores, q$n),
          presbias = rep(anomalies_congruent$presbias, q$n),
          absolmag = rep(anomalies_congruent$absolmag, q$n),
          gainloss = rep(anomalies_congruent$gainloss, q$n),
          delayspeed = rep(anomalies_congruent$delayspeed, q$n),
          subaddit = rep(anomalies_congruent$subaddit, q$n))

### We correct the scores for the Estonian sample (see Supplementary Data for details)
est_scores <- rescale(dat_item[dat_item$Code == "EST", ]$score, to = c(0, 19), 
                      from = range(0,17, na.rm = TRUE))
dat_item[dat_item$Code == "EST", ]$score <- est_scores
dat_item[dat_item$Code == "EST", ]$presbias <- NA
dat_item[dat_item$Code == "EST", ]$subaddit <- NA

## We remove unnecessary columns and variables
dat_unique <- dat_item %>% 
  distinct(ResponseId, .keep_all = TRUE) %>% 
  select(-question, -delay_sooner, -delay_later, - amount_sooner, -amount_later, -question_num, -block, -choice, -choice_text)

rm(q,scores, presbias, absolmag, gainloss, delayspeed, subaddit, est_scores)

###############################
# 2. Filters & Standarization
###############################

## In our analyses, we standardize and center (to their respective group mean) all variables. GDP is log transformed.
## We also transform several levels of the categorical controls (gender, education and employment) to present coherent, consistent categories across countries.
## The clean  dataframe in an object called dat_filter.

dat_filter <- dat_unique %>% 
  
  group_by(Code) %>% 
  
  #Remove additional ~100 improbable responses      
  filter(income - median(income, na.rm =T) < 100*mad(income, na.rm = T)) %>% 
  filter(assets - median(assets, na.rm =T) < 1000*mad(assets, na.rm = T)) %>% 
  
  #Remove additional ~250 improbable responses 
  filter(!(income < 1 & (Employment == "Employed full-time" | Employment == "Employed part-time"))) %>%
  
  ## We mean center and standardize all our individual-level variables
  mutate(age.cwc = Age-mean(Age, na.rm = T)) %>%
  mutate(age.cwc = c(scale(age.cwc))) %>%
  
  mutate(indineq.cwc = indineq-mean(indineq)) %>%
  mutate(indineq.cwc = c(scale(indineq.cwc))) %>%
  
  mutate(Risk_preference.cwc = Risk_preference-mean(Risk_preference)) %>%
  mutate(Risk_preference.cwc = c(scale(Risk_preference.cwc))) %>%
  
  mutate(income.cwc = income-mean(income)) %>%
  mutate(income.cwc = c(scale(income.cwc))) %>%
  
  mutate(debt.cwc = debt-mean(debt)) %>%
  mutate(debt.cwc = c(scale(debt.cwc))) %>%
  
  mutate(assets.cwc = assets-mean(assets)) %>%
  mutate(assets.cwc = c(scale(assets.cwc))) %>% 
  
  mutate(disc_debt.cwc = disc_debt-mean(disc_debt)) %>%
  mutate(disc_debt.cwc = c(scale(disc_debt.cwc))) %>%
  
  mutate(disc_invest.cwc = disc_invest-mean(disc_invest)) %>%
  mutate(disc_invest.cwc = c(scale(disc_invest.cwc))) %>%
  
  mutate(disc_savings.cwc = disc_savings-mean(disc_savings)) %>%
  mutate(disc_savings.cwc = c(scale(disc_savings.cwc))) %>%
  
  mutate(disc_non_fin.cwc = disc_non_fin-mean(disc_non_fin)) %>%
  mutate(disc_non_fin.cwc = c(scale(disc_non_fin.cwc))) %>% 
  
  
  ungroup() %>% 
  
  ## We grand-mean center and standardize all our individual-level variables
  mutate(score.gmcs = c(scale(score))) %>%
  mutate(GINI.gmc = GINI-mean(GINI, na.rm = T)) %>%
  mutate(GINI.gmc = c(scale(GINI.gmc))) %>% 
  
  mutate(GDP.log = log(GDP)) %>%
  mutate(GDP.gmc = GDP.log-mean(GDP.log, na.rm = T)) %>%
  mutate(GDP.gmc = c(scale(GDP.gmc))) %>%
  
  mutate(inflation.gmc = Inflation-mean(Inflation, na.rm = T)) %>%
  mutate(inflation.gmc = c(scale(inflation.gmc))) %>%
  
  ## We include a last changes of some relevant variables, including changing the order of certain levels and changes in labeling.
  mutate(Gender = fct_recode(Gender, "Other" = "Prefer not to answer"),
         EducationCompleted = fct_recode(EducationCompleted, 
                                         "Primary ed." = "No formal ed.",
                                         "Graduate" = "MBA", 
                                         "Graduate" = "PhD"),
         Employment = fct_recode(Employment, 
                                 "Employed full-time" = "Military")) %>% 
  
  mutate(
    Gender = fct_relevel(Gender, "Female", "Male", "Other"),
    EducationCompleted = fct_relevel(EducationCompleted, 
                                     "Bachelor", 
                                     "Primary ed.", 
                                     "Secondary ed.", 
                                     "Technical ed.",
                                     "Graduate"),
    
    Employment = fct_relevel(Employment, 
                             "Employed full-time",
                             "Self-employed",
                             "Employed part-time",
                             "Not in paid employment (looking)",
                             "Not in paid employment (personal reasons)",
                             "Full-time student",
                             "Retired"),
    
    Majority = fct_relevel(Majority, 
                           "Majority",
                           "Minority",
                           "Immigrant",
                           "Mixed race"))

# We save our final datasets at item (dat_item), individual without filter (dat_unique), and final dataset (dat_filter) in a single R.Data
save(dat_item, dat_unique, dat_filter, file = "1_2_data_files.RData")

# We write an specific .csv for each created dataset.
write.csv(dat_item, file = "1_3_dat_unique_item_1_10_2021.csv")
write.csv(dat_unique, file = "1_4_dat_unique_id_1_10_2021.csv")
write.csv(dat_filter, file = "1_5_dat_transform_1_10_2021.csv")

# sessionInfo()
#R version 4.0.2 (2020-06-22)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 10 x64 (build 19042)

#Matrix products: default

#Matrix products: default

#locale:
#[1] LC_COLLATE=Spanish_Spain.1252  LC_CTYPE=Spanish_Spain.1252    LC_MONETARY=Spanish_Spain.1252
#[4] LC_NUMERIC=C                   LC_TIME=Spanish_Spain.1252    

#attached base packages:
#  [1] stats     graphics  grDevices utils     datasets  methods   base     

#other attached packages:
#[1] scales_1.1.1         forcats_0.5.1        stringr_1.4.0        dplyr_1.0.4          purrr_0.3.4         
#[6] readr_1.4.0          tidyr_1.1.2          tibble_3.0.6         ggplot2_3.3.3        tidyverse_1.3.0     
#[11] RevoUtils_11.0.2     RevoUtilsMath_11.0.0

#loaded via a namespace (and not attached):
#[1] Rcpp_1.0.6        cellranger_1.1.0  pillar_1.6.1      compiler_4.0.2    dbplyr_2.1.0      tools_4.0.2      
#[7] lubridate_1.7.9.2 jsonlite_1.7.2    lifecycle_1.0.0   gtable_0.3.0      pkgconfig_2.0.3   rlang_0.4.10     
#[13] reprex_1.0.0      cli_2.5.0         rstudioapi_0.13   DBI_1.1.1         haven_2.3.1       withr_2.4.2      
#[19] xml2_1.3.2        httr_1.4.2        fs_1.5.0          generics_0.1.0    vctrs_0.3.8       hms_1.0.0        
#[25] grid_4.0.2        tidyselect_1.1.1  glue_1.4.2        R6_2.5.0          fansi_0.4.2       readxl_1.3.1     
#[31] modelr_0.1.8      magrittr_2.0.1    backports_1.2.1   ellipsis_0.3.2    rvest_0.3.6       assertthat_0.2.1 
#[37] colorspace_2.0-0  utf8_1.1.4        stringi_1.5.3     munsell_0.5.0     broom_0.7.5       crayon_1.4.1

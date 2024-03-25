library(tidyverse)
library(googlesheets4)
library(writexl)

trolley_infosheet_raw <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1hacxT8-8Sd_ABBe_hyw3O4rc_2O7JgErlgW-o3SpdxM/edit?usp=sharing")

colnames(trolley_infosheet_raw)

# Number of distinct collaborators
trolley_infosheet_raw %>%
  n_distinct("Full Name as used for publishing")

core_team <-
  trolley_infosheet_raw  %>%
  filter(!is.na(`Order in publication`)) %>%
  mutate(`Core team` = 1L)

methodology <-
  trolley_infosheet_raw %>%
  filter(Methodology)

not_core_team <-
  trolley_infosheet_raw  %>%
  filter(is.na(`Order in publication`)) %>%
  arrange(Surname) %>%
  mutate(`Order in publication` = 7:269,
         `Core team` = 0L,
         `Writing - review & editing` = TRUE,
         Investigation = TRUE) %>%
  mutate_at(
    vars(11:14, 16:24),
    ~ case_when(is.na(.) ~ FALSE,
                !is.na(.) ~ .,
                TRUE ~ NA))

trolley_infosheet <- bind_rows(core_team, not_core_team)

trolley_infosheet <-
  trolley_infosheet %>%
  mutate(Funding = case_when(Funding %in% c("No", "none", "None") ~ NA_character_,
                                         TRUE ~ Funding)) %>%
  arrange(`Order in publication`)

trolley_infosheet <-
  trolley_infosheet %>%
  mutate(`Email address` = NA_character_)

# Save data as external datafile
write_csv(trolley_infosheet, "inst/extdata/trolley_infosheet.csv")

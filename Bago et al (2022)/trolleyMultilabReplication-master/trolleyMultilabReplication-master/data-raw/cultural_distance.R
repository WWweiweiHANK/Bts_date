library(tidyverse)
library(countrycode)

# Read raw cultural distance data
cultural_distance_raw <- read_csv("data-raw/collectivism_similarity_matrix-from_culturaldistance.com.csv")

# Prepare processed data
cultural_distance <-
  cultural_distance_raw %>%
  transmute(country = names(.),
            Collectivism = `United States2010-2014`) %>%
  extract(country,
          into = c("Country", "distance_year"),
          regex = "(.*)(\\d{4}-\\d{4})") %>%
  # Get the latest similarity data
  group_by(Country) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  # USA should have 0 distance (not NA)
  mutate(Collectivism = if_else(is.na(Collectivism), 0, Collectivism),
         country3 = countryname(sourcevar = Country,
                                destination = "iso3c"))
# TODO: Check why serbia and montenegro country code is NA
# TODO: Do we need this code here at all?
# Proposal: Country should be added as a variable in the data_raw/trolley.R file to the trolley.rda table

# Save the processed datafile
usethis::use_data(cultural_distance, overwrite = TRUE)

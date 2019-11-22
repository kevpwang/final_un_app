library(fs)
library(janitor)
library(tidyverse)

# load from cleaned UN RDS

un_cleaned <- read_rds("clean-data/un_cleaned.rds")

# opened CSV in Excel and found that data begins with row 4
# `X65` automatically assigned to unnamed column

all_gdps <- read_csv("raw-data/all_gdps.csv", skip = 3,
                     col_types = cols(
                       .default = col_double(),
                       `Country Name` = col_character(),
                       `Country Code` = col_character(),
                       `Indicator Name` = col_character(),
                       `Indicator Code` = col_character(),
                       `1960` = col_logical(),
                       `2019` = col_logical(),
                       X65 = col_logical()
                       )
                     ) %>% 
  
  # remove these columns because values are all NAs
  
  select(-`1960`, -`2019`, -`X65`) %>% 
  
  # drop entirety of ANY rows with NA values between 1961-2018.
  # gather horizontally spread years into vertical columns to match
  # format of UN data.
  
  drop_na() %>%
  gather(key = "year", value = "growth", `1961`:`2018`) %>% 
  clean_names() %>% 
  select(country_name, country_code, year, growth) %>%
  
  # year is chr for some reason
  # make growth a decimal for later display with scales::percent()
  
  mutate(year = as.numeric(year),
         growth = .01 * growth) %>% 
  arrange(year) %>% 
  
  # remove all countries not also in UN data.
  # use country code for this bc format of names can vary.
  
  filter(country_code %in% unique(un_cleaned$country))

# write cleaned data to RDS for later use

write_rds(all_gdps, "clean-data/all_gdps.rds")

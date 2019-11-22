library(fs)
library(janitor)
library(tidyverse)

un_cleaned <- read_rds("clean-data/un_cleaned.rds")

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
  select(-`1960`, -`2019`, -`X65`) %>% 
  drop_na() %>%
  gather(key = "year", value = "growth", `1961`:`2018`) %>% 
  clean_names() %>% 
  select(country_name, country_code, year, growth) %>% 
  mutate(year = as.numeric(year),
         growth = .01 * growth) %>% 
  arrange(year) %>% 
  filter(country_code %in% unique(un_cleaned$country))

write_rds(all_gdps, "clean-data/all_gdps.rds")

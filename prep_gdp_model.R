library(fs)
library(janitor)
library(tidyverse)

un_cleaned <- read_rds("clean-data/un_cleaned.rds")
all_majs <- read_rds("clean-data/all_majs.rds") %>% 
  mutate(country = as.character(country))
all_gdps <- read_rds("clean-data/all_gdps.rds") %>% 
  arrange(country_code)

majs_1961 <- all_majs %>% 
  filter(year >= 1961) %>% 
  filter(country %in% unique(all_gdps$country_code))

majs_gdps <- left_join(majs_1961, all_gdps, by = c("country" = "country_code", "year"))

write_rds(majs_gdps, "clean-data/majs_gdps.rds")

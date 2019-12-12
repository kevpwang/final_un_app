library(fs)
library(janitor)
library(tidyverse)

# load cleaned majorities data
# change factor to chr to remove potential warning for left_join()

all_majs <- read_rds("clean-data/all_majs.rds") %>% 
  mutate(country = as.character(country)) %>% 
  select(-countryname)

# load cleaned GDP data

all_gdps <- read_rds("clean-data/all_gdps.rds") %>% 
  arrange(country_code)

# GDP data begins in 1961, so limit majorities data range of years.
# remove all countries not also in GDP data, again 
# using standardized country code.

majs_1961 <- all_majs %>% 
  filter(year >= 1961) %>% 
  filter(country %in% unique(all_gdps$country_code))

# left_join() to preserve country codes; make sure to specify
# name difference in columns that have same data

majs_gdps <- left_join(majs_1961, all_gdps, by = c("country" = "country_code", "year"))

# write cleaned & joined data as RDS for direct use in Shiny app

write_rds(majs_gdps, "clean-data/majs_gdps.rds")

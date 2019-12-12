library(fs)
library(janitor)
library(tidyverse)

# load cleaned UN data

un_cleaned <- read_rds("clean-data/un_cleaned.rds")

# recode all vote codes for easy comparison

votes <- un_cleaned %>%
  filter(vote %in% c(1, 3)) %>%
  mutate(vote = recode(vote, `1` = "yes", `3` = "no")) %>%
  select(year, country, countryname, vote, yes, no, unres, short)

# dummy df created for comparison

yes_no <- votes %>% select(yes, no)

# excludes abstentions & absences: otherwise would result in
# anomalous majority proportions unrelated to real data

total_votes <- votes %>%
  group_by(country, year) %>% 
  summarize(total = n())

# convoluted ifelse() needed to compare VALUE of
# particular vote to COLUMN NAME ('yes' or 'no')
# keep countryname for use in app with issues_majs

all_majs <- votes %>% 
  mutate(in_minority = ifelse(colnames(yes_no)[max.col(yes_no)] != vote, TRUE, FALSE)) %>%
  group_by(country, countryname, year) %>%
  summarize(maj = sum(in_minority == FALSE)) %>%
  
  # use inner_join() because mutate(total = total_votes$total) throws a length error
  # NB: can pipe into inner_join()
  
  inner_join(total_votes, by = c("country", "year")) %>%
  mutate(prop_maj = maj / total) %>% 
  
  # NB: ungroup() lest metadata interfere with later analysis
  
  ungroup()

# write cleaned data to RDS for later use

write_rds(all_majs, "clean-data/all_majs.rds")
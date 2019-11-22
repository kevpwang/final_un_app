library(fs)
library(janitor)
library(tidyverse)

un_cleaned <- read_rds("clean-data/un_cleaned.rds")

votes <- un_cleaned %>%
  filter(vote %in% c(1, 3)) %>%
  mutate(vote = recode(vote, `1` = "yes", `3` = "no")) %>%
  select(year, country, countryname, vote, yes, no, unres, short)

# dummy df created for comparison

yes_no <- votes %>% select(yes, no)

total_votes <- votes %>%
  group_by(country, year) %>% 
  summarize(total = n())

all_majs <- votes %>% 
  mutate(in_minority = ifelse(colnames(yes_no)[max.col(yes_no)] != vote, TRUE, FALSE)) %>%
  group_by(country, year) %>%
  summarize(maj = sum(in_minority == FALSE)) %>% 
  inner_join(total_votes, by = c("country", "year")) %>%
  mutate(prop_maj = maj / total)

write_rds(all_majs, "clean-data/all_majs.rds")
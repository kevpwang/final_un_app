
# some votes have the `no` column as NA. Since `yes` votes
# on those are overwhelmingly, reasonable to assume that
# the NAs is a data error and replace NAs with 0

un_data <- read.csv("raw-data/un_data.csv") %>%
  clean_names() %>%
  arrange(year) %>%
  replace_na(list(no = 0))

# write to clean-data folder for later use

write_rds(un_data, "clean-data/un_cleaned.rds")
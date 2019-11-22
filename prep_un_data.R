un_data <- read.csv("raw-data/UNdata.csv") %>%
  clean_names() %>%
  arrange(year) %>%
  replace_na(list(no = 0))

write_rds(un_data, "clean-data/un_cleaned.rds")
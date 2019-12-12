library(tidyverse)
library(janitor)

# some votes have the `no` column as NA. Since `yes` votes
# on those are overwhelmingly, reasonable to assume that
# the NAs is a data error and replace NAs with 0.

# set encoding to latin1 so that special characters render properly

un_data <- read.csv("raw-data/un_data.csv", fileEncoding = "latin1") %>%
  clean_names() %>%
  arrange(year) %>%
  replace_na(list(no = 0))

# write to clean-data folder for later use

write_rds(un_data, "clean-data/un_cleaned.rds")

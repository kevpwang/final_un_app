library(fs)
library(janitor)
library(tidyverse)

un_cleaned <- read_rds("clean-data/un_cleaned.rds")
all_majs <- read_rds("clean-data/all_majs.rds")
all_gdps <- read_rds("clean-data/all_gdps.rds")
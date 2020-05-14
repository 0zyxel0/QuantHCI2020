library(purrr)
library(tidyverse)
library(jsonlite)

path <- "class-logs"
files <- dir(path, pattern = "*.json")

data <- files %>%
  map_df(~fromJSON(file.path(path, .), flatten = TRUE))

yelp <- fromJSON("paper-logs/5307_log_Mix_1438008139.txt")
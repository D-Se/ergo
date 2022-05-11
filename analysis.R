library(tidyverse)
library(jsonlite)
url <- "http://rpkg.gepuro.net/download"

df <- jsonlite::fromJSON(url, simplifyDataFrame = T)[[1]] |>
  as_tibble()

clean_df <- df |>
  filter(! title %in% c("", "What the Package Does (Title Case)", "What the Package Does (One Line, Title Case)")) |>
  distinct(title, .keep_all = T)



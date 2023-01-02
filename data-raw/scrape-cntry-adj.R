library(rvest)
library(tidyverse)
library(countryscales)

url <- "https://www.ef.com/wwen/english-resources/english-grammar/nationalities/"

html <- read_html(url)

country_adjectives <- html |>
  html_table() |>
  pluck(2) |>
  select(cntry = 1, adjective = 2)

locales |>
  select("locale", "cntry") |>
  mutate(cntry_tidy = gsub(", .*$", "", cntry)) |>
  left_join(country_adjectives, by = c("cntry_tidy" = "cntry")) |>
  filter(!is.na(adjective))

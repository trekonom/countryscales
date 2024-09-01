## code to prepare `g20` dataset goes here

library(rvest)
library(tidyverse)
library(countrycode)

url <- "https://en.wikipedia.org/wiki/G20"

country <- rvest::read_html(url) |>
  rvest::html_table() |>
  pluck(3) |>
  filter(!grepl("Union", Member)) |>
  distinct(Member) |>
  pull(Member)

iso2c <- countrycode(country, origin = "country.name", destination = "iso2c")
iso3c <- countrycode(country, origin = "country.name", destination = "iso3c")

g20 <- tibble(
  country = country,
  iso2c = iso2c,
  iso3c = iso3c
) |>
  left_join(countries |> select(-name), by = "iso2c", relationship = "many-to-many") |>
  filter(!locale %in% c("gn-AR", "en-IN", "fr-CA"), !(iso2c == "ZA" & language != "af")) |>
  distinct()

usethis::use_data(g20, overwrite = TRUE)

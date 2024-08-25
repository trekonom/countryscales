library(httr)
library(jsonlite)
library(tidyverse)

resp <- httr::GET("https://cdn.simplelocalize.io/public/v1/locales")

countries <- httr::content(resp, encoding = "UTF-8", as = "text") |>
  jsonlite::fromJSON()

countries <- countries |>
  unnest_wider(all_of(c("language", "country")), names_sep = ".") |>
  select(
    locale,
    language = language.iso_639_1,
    name = country.name,
    iso2c = country.iso_3166_1_alpha2
  )

usethis::use_data(countries, overwrite = TRUE)

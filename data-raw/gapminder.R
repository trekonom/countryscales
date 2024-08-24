# Pakete laden ------------------------------------------------------------
library(tidyverse)
library(httr)
library(readxl)
library(janitor)

# create directory for storing raw XLs
if (!dir.exists(raw_csv_dir <- "data-raw/gapminder_raw/")) {
  dir.create(raw_csv_dir)
}

read_data <- function(url) {
  GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))
  read_excel(tf, "Data")
}

tidy_data <- function(df, label) {
  df %>%
    rename(country = 1) %>%
    gather(year, !!rlang::sym(label), -country)
}
# Get data

## Geo-Informationen
url <- "https://docs.google.com/spreadsheets/d/1qHalit8sXC0R8oVXibc2wa2gY7bkwGzOybEMTWp-08o/export?format=xlsx"

GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")))
excel_sheets(tf)
geo_raw <- read_excel(tf, 2L)

## Indicators ----
urls <- list(
  life_exp = "https://docs.google.com/spreadsheet/pub?key=phAwcNAVuyj2tPLxKvvnNPA&output=xlsx",
  gdp_per_cap = "https://docs.google.com/spreadsheet/pub?key=phAwcNAVuyj1jiMAkmq1iMg&output=xlsx",
  pop = "https://docs.google.com/spreadsheet/pub?key=phAwcNAVuyj0XOoBL_n5tAQ&output=xlsx"
)

gapminder_raw <- map(urls, read_data)

# Raw data ----

write.csv(geo_raw, here::here(raw_csv_dir, "geo.csv"),
  row.names = FALSE, quote = FALSE
)
iwalk(gapminder_raw, ~ write.csv(.x,
  here::here(raw_csv_dir, paste0(.y, ".csv")),
  row.names = FALSE, quote = FALSE
))

# Tidy data  ----

geo <- geo_raw %>%
  clean_names(., case = "snake") %>%
  select(code = geo, country = name, region = four_regions) %>%
  mutate(region = str_to_title(region))

gapminder_proc <- gapminder_raw %>%
  imap(tidy_data) %>%
  reduce(full_join, by = c("country", "year")) %>%
  filter(!is.na(country)) %>%
  mutate(year = as.integer(year))

gapminder_clean <- gapminder_proc %>%
  drop_na()

countries_data_all <- gapminder_clean %>%
  select(country, year) %>%
  mutate(is_df = TRUE) %>%
  spread(year, is_df) %>%
  drop_na() %>%
  select(country)

# Final data: countries with ob for all years and variables ----

gapminder <- gapminder_clean %>%
  semi_join(countries_data_all) %>%
  inner_join(geo) %>%
  select(country, code, region, everything())

# Data for 2015 ----
gapminder15 <- gapminder %>%
  filter(year == 2015)

# Save data ----

write.csv(
  gapminder, "data-raw/csv/gapminder.csv",
  row.names = FALSE, quote = TRUE
)
write.csv(
  gapminder15, "data-raw/csv/gapminder15.csv",
  row.names = FALSE, quote = TRUE
)

usethis::use_data(gapminder, overwrite = TRUE)
usethis::use_data(gapminder15, overwrite = TRUE)

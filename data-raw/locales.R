## code to prepare `locales` dataset goes here
library(rvest)
library(tidyverse)

url <- "https://lh.2xlibre.net/locales/"
html <- read_html(url)

locales <- html %>%
  html_table() %>%
  pluck(1) %>%
  select(locale = 1, lang = 2, cntry = 3)

urls <- html %>%
  html_nodes("a[href^='/locale']") %>%
  map_dfr(function(x) {
    data.frame(
      locale = html_text2(x),
      url = html_attr(x, "href")
    )
  })

base_url <- "https://lh.2xlibre.net"
locales <- locales %>%
  right_join(urls, by = "locale") %>%
  filter(!grepl("@", locale)) %>%
  mutate(
    lang_iso2 = gsub("^([a-z]{2}).*?$", "\\1", locale),
    cntry_iso2 = gsub("^.*?_([A-Z]{2})$", "\\1", locale),
    cntry_iso2 = tolower(cntry_iso2),
    url = paste0(base_url, url),
    lang = gsub("\\—", "", lang),
    lang = trimws(lang),
    cntry = stringr::str_to_title(cntry)
  )

read_locale <- function(locale_str) {
  url <- locales[locales$locale == locale_str, "url", drop = TRUE]

  html <- rvest::read_html(url)

  tbls <- html |>
    rvest::html_nodes("div.cat_section") |>
    rvest::html_table() |>
    lapply(function(x) {
      if (nrow(x) > 0) {
        names(x) <- c("comment", "value", "example")
        x$value <- gsub("\\n.*$", "", x$value)
      }
      x
    })

  names(tbls) <- html |>
    rvest::html_nodes("h3.cat") |>
    rvest::html_text2()
  names(tbls) <- gsub("^.*?\\((.*)\\)", "\\1", names(tbls))

  return(tbls)
}

locale_str <- locales$locale
names(locale_str) <- locale_str

locale_specs <- purrr::map(locale_str, read_locale)

read_specs <- function(x, what) {
  xx <- x[[what]]["value"]
  xx <- as.data.frame(t(xx))
  names(xx) <- gsub("^.*?\\(([a-z_]+)\\).*$", "\\1", x[[what]][["comment"]])
  row.names(xx) <- NULL
  xx
}
safe_read_specs <- purrr::safely(read_specs)
map_specs <- function(locale_specs, what) {
  purrr::map(locale_specs, safe_read_specs, what = what) %>%
    transpose() %>%
    pluck("result") %>%
    bind_rows(.id = "locale") %>%
    mutate(across(!locale, str_replace, pattern = "^glib.*$", replacement = NA_character_))
}

lc_numeric <- map_specs(locale_specs, what = "LC_NUMERIC")
lc_monetary <- map_specs(locale_specs, what = "LC_MONETARY") %>%
  mutate(positive_sign = coalesce(positive_sign, positive_sign...1, positive_sign...6)) %>%
  select(-matches("\\.{3}\\d$"))

locales <- locales %>%
  right_join(lc_numeric) %>%
  right_join(lc_monetary)

locales <- locales %>%
  filter(!is.na(url)) %>%
  mutate(
    across(c(decimal_point, thousands_sep, mon_decimal_point, mon_thousands_sep), stringi::stri_escape_unicode),
    across(c(thousands_sep), ~ ifelse(grepl("local", .x, fixed = TRUE), NA_character_, .x)),
    across(c(decimal_point), ~ ifelse(grepl("^% see LC_MONETARY", .x), mon_decimal_point, .x)),
    across(c(thousands_sep, mon_thousands_sep), ~ ifelse(grepl("% <NNBSP> (0X202F)", .x, fixed = TRUE), "\\u20ff", .x))
  )

locales[] <- lapply(locales[], stringi::stri_escape_unicode)
locales[] <- lapply(locales[], function(x) gsub('\\\\', '\\', x, fixed = TRUE))

usethis::use_data(locales, overwrite = TRUE)

#dplyr::distinct(locales, locale, decimal_point, thousands_sep) |> View()

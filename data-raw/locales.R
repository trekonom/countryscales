## code to prepare `locales2` dataset goes here
library(i18n)
library(tidyverse)

PATTERN_SEPARATOR <- ";"
QUOTE <- "'"
PATTERN_DIGIT <- "#"
PATTERN_ZERO_DIGIT <- "0"
PATTERN_GROUPING_SEPARATOR <- ","
PATTERN_DECIMAL_SEPARATOR <- "."
PATTERN_CURRENCY_SIGN <- "\u00A4"
PATTERN_PER_MILLE <- "\u2030"
PER_MILLE_SCALE <- 1000
PATTERN_PERCENT <- "%"
PERCENT_SCALE <- 100
PATTERN_EXPONENT <- "E"
PATTERN_PLUS <- "+"

sep_by <- function(x) {
  sep_by <- x
  sep_by[] <- "0"

  sep_by[
    grepl("\u00A4\u00a0(#|0)", x) | grepl("(#|0)\u00a0\u00A4", x)
  ] <- "1"

  sep_by[
    grepl("\u00A4\u00a0(\\+|\\-)(#|0)", x) |
      grepl("(#|0)(\\+|\\-)\u00a0\u00A4", x)
  ] <- "2"

  sep_by
}

percent_sep_by <- function(x) {
  sep_by <- x
  sep_by[] <- "0"

  sep_by[
    grepl("%\u00a0(#|0)", x) | grepl("(#|0)\u00a0%", x)
  ] <- "1"

  sep_by
}

percent_precedes <- function(x) {
  percent_precedes <- logical(length(x))

  percent_precedes[grepl("^%\u00a0?(#|0)", x)] <- TRUE

  percent_precedes
}

cs_precedes <- function(x) {
  cs_precedes <- x
  cs_precedes[] <- TRUE

  cs_precedes[grepl("(#|0).*?\u00A4", x)] <- FALSE

  cs_precedes
}

sign_posn <- function(x) {
  sign_posn <- x
  sign_posn[] <- 1

  sign_posn[
    grepl("\u00A4.+?(\\+|\\-)", x) &
      grepl("(0|#).*?(\\+|\\-)", x)
  ] <- 2

  sign_posn[grepl("(\\+|\\-)\u00A4", x)] <- 3
  sign_posn[grepl("\u00A4(\\+|\\-)", x)] <- 4

  sign_posn
}

symbol_sign <- function(x, pattern, default) {
  symbol_sign <- x
  symbol_sign[] <- ""

  symbol_sign[grepl(pattern, x)] <- default

  symbol_sign
}

locales <- i18n::numbers |>
  separate_wider_delim(currency_format,
    names = c("p_currency_format", "n_currency_format"),
    delim = ";",
    too_few = "align_start",
    cols_remove = FALSE
  ) |>
  mutate(
    n_currency_format = coalesce(
      n_currency_format,
      paste0("-", p_currency_format)
    ),
    across(c(p_currency_format, n_currency_format), trimws),
    across(
      c(p_currency_format, n_currency_format,
        minus_sign, plus_sign, percent_sign),
      ~ gsub("\u200e", "", .x, fixed = TRUE)
    ),
    across(
      c(p_currency_format, n_currency_format),
      ~ gsub("\u200f", "", .x, fixed = TRUE)
    )
  )

x <- locales |>
  count(p_currency_format, n_currency_format) |>
  pull(n_currency_format)

locales <- locales |>
  # Fix. Wrong group and decimal delimiters
  mutate(
    group = case_match(
      locale,
      "en-ZA" ~ ",",
      .default = group
    ),
    decimal = case_match(
      locale,
      "en-ZA" ~ ".",
      .default = decimal
    )
  ) |>
  mutate(
    n_cs_neg = str_locate(n_currency_format, "\\-")[, 1],
    p_cs_precedes = cs_precedes(p_currency_format),
    n_cs_precedes = cs_precedes(n_currency_format),
    p_sep_by_space = sep_by(p_currency_format),
    n_sep_by_space = sep_by(n_currency_format),
    p_sign_posn = sign_posn(p_currency_format),
    n_sign_posn = sign_posn(n_currency_format),
    p_sign = symbol_sign(p_currency_format, "\\+", plus_sign),
    n_sign = symbol_sign(n_currency_format, "\\-", minus_sign),
    thousands_sep = group,
    decimal_point = decimal,
    mon_thousands_sep = group,
    mon_decimal_point = decimal,
    percent_sep_by = percent_sep_by(percent_format),
    percent_precedes = percent_precedes(percent_format),
    style_negative = case_match(
      minus_sign,
      "\u2212" ~ "minus",
      .default = "hyphen"
    ),
    style_positive = "none"
  )

x <- locales |>
  count(
    p_currency_format, n_currency_format,
    p_cs_precedes, n_cs_precedes,
    p_sep_by_space, n_sep_by_space,
    p_sign_posn, n_sign_posn,
    p_sign, n_sign
  )

locales <- locales |>
  bind_rows(
    locales |>
      left_join(default_locales, by = c("locale" = "base_locale")) |>
      filter(!is.na(default_locale)) |>
      rename(locale_name = locale) |>
      mutate(
        locale = default_locale
      ) |>
      select(-default_locale)
  )

locales <- locales |>
  mutate(locale_name = coalesce(locale_name, locale)) |>
  arrange(locale)

# Only latin numbering systems
locales_latn <- locales[
  locales$default_numbering_system == "latn",
  "locale",
  drop = TRUE
]
# No special groupings
locales_grouping <- locales[
  !grepl("#,##,#", locales$decimal_format, fixed = TRUE),
  "locale",
  drop = TRUE
]
locales_to_include <- intersect(locales_latn, locales_grouping)

locales <- locales |>
  filter(locale %in% locales_to_include)
usethis::use_data(locales, overwrite = TRUE)

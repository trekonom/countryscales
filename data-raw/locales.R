## code to prepare `locales2` dataset goes here
library(i18n)
library(tidyverse)

source("data-raw/utils-locales.R")

locales <- i18n::numbers |>
  # Fixes.
  mutate(
    # Wrong group and decimal delimiters
    group = case_match(
      locale,
      "en-ZA" ~ ",",
      .default = group
    ),
    decimal = case_match(
      locale,
      "en-ZA" ~ ".",
      .default = decimal
    ),
    # Currency format
    currency_format = case_match(
      locale,
      c(
        "zgh", "twq", "ses",
        "seh", "sbp", "rwk",
        "rn", "luo", "lu", "lg",
        "ksb", "khq", "kab", "dje",
        "bez", "agq"
      ) ~ "#,##0.00\u00a0\u00a4",
      c(
        "he"
      ) ~ "\u00a0#,##0.00\u00a0\u00a4;-#,##0.00\u00a0\u00a4",
      c(
        "ur", "und", "nds-NL", "nds",
        "lkt", "lag", "ks-Deva"
      ) ~ "\u00a4#,##0.00",
      c(
        "af", "und", "nds-NL", "nds",
        "lkt", "lag", "ks-Deva"
      ) ~ "\u00a4#,##0.00",
      .default = currency_format
    ),
    currency_format = case_when(
      grepl("^af", locale) ~ "\u00a4\u00a0#,##0.00",
      grepl("^shi", locale) ~ "#,##0.00\u00a0\u00a4",
      grepl("^ar", locale) ~ "#,##0.00\u00a0\u00a4",
      grepl("^ms", locale) ~ "\u00a4\u00a0#,##0.00",
      grepl("^en\\-(AU)", locale) ~ "\u00a4\u00a0#,##0.00;-\u00a4\u00a0#,##0.00",
      grepl("^en\\-(FI|DE|BE|AT)", locale) ~ "\u00a4#,##0.00;-\u00a4#,##0.00",
      grepl("^en\\-(NL)", locale) ~ "\u00a4#,##0.00;-\u00a4#,##0.00",
      grepl("^en\\-(SI)", locale) ~ "\u00a4#,##0.00",
      grepl("^es\\-(NI|PA|MX|HN|GT|BZ|BR|419)", locale) ~ "\u00a4\u00a0#,##0.00;-\u00a4\u00a0#,##0.00",
      grepl("^es\\-(BO|CR)", locale) ~ "\u00a4\u00a0#,##0.00",
      grepl("^es\\-(VE)", locale) ~ "\u00a4\u00a0#,##0.00;\u00a4-#,##0.00",
      grepl("^es\\-(PY)", locale) ~ "\u00a4\u00a0#,##0.00;\u00a4\u00a0-#,##0.00",
      .default = currency_format
    )
  ) |>
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
      c(
        p_currency_format, n_currency_format,
        minus_sign, plus_sign, percent_sign
      ),
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
    style_positive = "none",
    mon_thousands_sep = case_when(
      grepl("^en\\-(SI|NL|DE|FI|BE|AT)", locale) ~ ",",
      grepl("^de-AT", locale) ~ ".",
      .default = mon_thousands_sep
    ),
    mon_decimal_point = case_when(
      grepl("^en\\-(SI|NL|DE|FI|BE|AT)", locale) ~ ".",
      .default = mon_decimal_point
    )
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

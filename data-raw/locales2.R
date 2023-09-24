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
      c(p_currency_format, n_currency_format),
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
    mon_decimal_point = decimal
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
    filter(locales, locale == "de") |>
      mutate(locale = "de-DE"),
    filter(locales, locale == "fr") |>
      mutate(locale = "fr-FR"),
    filter(locales, locale == "en") |>
      mutate(locale = "en-US")
  ) |>
  arrange(locale)

locales2 <- locales
usethis::use_data(locales2, overwrite = TRUE)

# parseAffix <- function(x) {
#   affix = ""
#   inQuote = FALSE
#   while (parseCharacterAffix(affix) && pattern.read().isNotEmpty) {}
#   return toString(affix)
# }
#
#
#
# bool parseCharacterAffix(StringBuffer affix) {
#   if (pattern.atEnd) return false;
#   var ch = pattern.peek();
#   if (ch == QUOTE) {
#     var peek = pattern.peek(2);
#     if (peek.length == 2 && peek[1] == QUOTE) {
#       pattern.pop();
#       affix.write(QUOTE); // 'don''t'
#     } else {
#       inQuote = !inQuote;
#     }
#     return true;
#   }
#
#
#   if (inQuote) {
#     affix.write(ch);
#   } else {
#     switch (ch) {
#       case PATTERN_DIGIT:
#         case PATTERN_ZERO_DIGIT:
#         case PATTERN_GROUPING_SEPARATOR:
#         case PATTERN_DECIMAL_SEPARATOR:
#         case PATTERN_SEPARATOR:
#         return false;
#       case PATTERN_CURRENCY_SIGN:
#         // TODO(alanknight): Handle the local/global/portable currency signs
#       affix.write(currencySymbol);
#       break;
#       case PATTERN_PERCENT:
#         if (result.multiplier != 1 && result.multiplier != PERCENT_SCALE) {
#           throw const FormatException('Too many percent/permill');
#         }
#       result.multiplier = PERCENT_SCALE;
#       affix.write(symbols.PERCENT);
#       break;
#       case PATTERN_PER_MILLE:
#         if (result.multiplier != 1 && result.multiplier != PER_MILLE_SCALE) {
#           throw const FormatException('Too many percent/permill');
#         }
#       result.multiplier = PER_MILLE_SCALE;
#       affix.write(symbols.PERMILL);
#       break;
#       default:
#         affix.write(ch);
#     }
#   }
#   return true;
# }

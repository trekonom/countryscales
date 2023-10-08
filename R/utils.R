"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}


check_locale <- function(x) {
  x <- match.arg(x, unique(countryscales::locales$locale))
  countryscales::locales[countryscales::locales$locale %in% x, ]
}

check_mark <- function(x, locale, what) {
  x <- x %||% locale[[what]]
  stringi::stri_unescape_unicode(x)
}

check_big <- function(x, locale) {
  check_mark(x, locale, "thousands_sep")
}

check_big_currency <- function(x, locale) {
  check_mark(x, locale, "mon_thousands_sep")
}

check_decimal_currency <- function(x, locale) {
  check_mark(x, locale, "mon_decimal_point")
}

check_suffix <- function(x, locale, suffix = "%") {
  x <- check_mark(x, locale, "p_sep_by_space")
  paste0(strrep(" ", x), suffix)
}

check_decimal <- function(x, locale) {
  check_mark(x, locale, "decimal_point")
}

check_p_sep_space <- function(x, locale) {
  check_mark(x, locale, "p_sep_by_space")
}

check_n_sep_space <- function(x, locale) {
  check_mark(x, locale, "n_sep_by_space")
}

currency_symbol <- function(locale, currency) {
  i18n::cldr_currencies(
    locale = locale_name(locale),
    currency = i18n::currency_code_list[[currency]]
  )
}

locale_name <- function(locale) {
  countryscales::locales[
    countryscales::locales$locale == locale,
    "locale_name",
    drop = TRUE
  ]
}

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}


check_locale <- function(x, call = rlang::caller_env()) {
  valid <- unique(countryscales::locales$locale)

  if (is.character(x) && length(x) == 1 && x %in% valid) {
    return(countryscales::locales[countryscales::locales$locale == x, ])
  }

  shown <- if (is.character(x) && length(x) == 1) {
    sprintf('"%s"', x)
  } else {
    paste(format(x), collapse = ", ")
  }

  suggestions <- if (is.character(x) && length(x) == 1) {
    agrep(x, valid, max.distance = 0.2, value = TRUE)
  }

  rlang::abort(
    c(
      sprintf("`locale` must be a valid locale code, not %s.", shown),
      if (length(suggestions) > 0) {
        c(i = sprintf(
          "Did you mean %s?",
          paste(sprintf('"%s"', suggestions), collapse = " or ")
        ))
      },
      c(i = "Run `show_locales()` to see all supported locale codes.")
    ),
    class = "countryscales_error_invalid_locale",
    call = call
  )
}

check_mark <- function(x, locale, what) {
  x %||% locale[[what]]
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

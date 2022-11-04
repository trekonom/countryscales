"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}


check_locale <- function(x) {
  x <- gsub("\\-", "_", x)
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

check_decimal <- function(x, locale) {
  check_mark(x, locale, "decimal_point")
}

check_suffix <- function(x, locale) {
  x <- check_mark(x, locale, "p_sep_by_space")
  paste0(strrep(" ", x), "%")
}

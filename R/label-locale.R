#' Formatting numbers
#'
#' The label_xxx_locale family of functions makes it easy to format numbers
#'     in decimal format, as percentages or as currencies.
#'
#' * `label_number_locale` formats numbers in decimal format.
#' * `label_percent_locale` formats numbers as percentages.
#' * `label_currency_locale` formats numbers as currencies.
#'
#' @inheritParams scales::label_number
#' @param locale locale string. Defaults to "en-US"
#' @param p_sep_by separator between currency symbol and positive
#'     monteary value
#' @param n_sep_by separator between currency symbol and negative
#'     monteary value
#' @param currency currency symbol
#'
#' @name label-locale
#'
#' @examples
#' \dontrun{
#' require(scales)
#' demo_continuous(
#'   c(-1e6, 1e6),
#'   labels = label_number_locale(locale = "fr-FR")
#' )
#' demo_continuous(
#'   c(-1, 1),
#'   label_percent_locale(locale = "it-IT", accuracy = .01)
#' )
#' demo_continuous(
#'   c(-1, 1),
#'   labels = label_currency_locale(
#'     locale = "ja-JP", accuracy = .1,
#'     currency = "JPY"
#'   )
#' )
#' }
NULL

#' @rdname label-locale
#' @export
label_number_locale <- function(accuracy = NULL, scale = 1,
                                prefix = "", suffix = "",
                                big.mark = NULL, decimal.mark = NULL,
                                locale = "en-US",
                                trim = TRUE, ...) {
  locale <- check_locale(locale)
  big.mark <- check_big(big.mark, locale)
  decimal.mark <- check_decimal(decimal.mark, locale)
  style_negative <- locale[["minus_sign"]]
  style_positive <- locale[["style_positive"]]

  label_number(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    style_negative = style_negative,
    style_positive = style_positive,
    trim = trim, ...
  )
}

#' @rdname label-locale
#' @export
label_percent_locale <- function(accuracy = NULL, scale = 100,
                                 prefix = NULL, suffix = NULL,
                                 big.mark = NULL, decimal.mark = NULL,
                                 locale = "en-US",
                                 trim = TRUE, ...) {
  locale <- check_locale(locale)
  big.mark <- check_big(big.mark, locale)
  decimal.mark <- check_decimal(decimal.mark, locale)
  style_negative <- locale[["minus_sign"]]
  style_positive <- locale[["style_positive"]]
  percent_sign <- locale[["percent_sign"]]

  if (locale[["percent_precedes"]]) {
    prefix <- prefix %||% paste0(
      percent_sign,
      rep("\u00a0", locale[["percent_sep_by"]])
    )
    suffix <- suffix %||% ""
  } else {
    prefix <- prefix %||% ""
    suffix <- suffix %||% paste0(
      rep("\u00a0", locale[["percent_sep_by"]]), percent_sign
    )
  }

  label_number(
    accuracy = accuracy, scale = scale, prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark, trim = trim,
    style_negative = style_negative,
    style_positive = style_positive,
    ...
  )
}

#' @rdname label-locale
#' @export
label_currency_locale <- function(accuracy = NULL, scale = 1,
                                  prefix = NULL,
                                  suffix = NULL,
                                  big.mark = NULL,
                                  decimal.mark = NULL,
                                  p_sep_by = NULL,
                                  n_sep_by = NULL,
                                  currency = "USD",
                                  locale = "en-US",
                                  trim = TRUE, ...) {
  currency <- currency_symbol(locale, currency)
  locale <- check_locale(locale)
  big.mark <- check_big_currency(big.mark, locale)
  decimal.mark <- check_decimal_currency(decimal.mark, locale)

  p_sign <- locale[["p_sign"]]
  n_sign <- locale[["n_sign"]]

  p_cs_precedes <- locale[["p_cs_precedes"]]
  n_cs_precedes <- locale[["n_cs_precedes"]]

  p_sign_posn <- locale[["p_sign_posn"]]
  n_sign_posn <- locale[["n_sign_posn"]]

  p_sep_by <- check_p_sep_space(p_sep_by, locale)
  n_sep_by <- check_n_sep_space(n_sep_by, locale)

  label_currency(
    accuracy = accuracy, scale = scale, currency = currency,
    big.mark = big.mark, decimal.mark = decimal.mark, trim = trim,
    p_sign = p_sign,
    n_sign = n_sign,
    p_cs_precedes = p_cs_precedes,
    n_cs_precedes = n_cs_precedes,
    p_sep_by = p_sep_by,
    n_sep_by = n_sep_by,
    p_sign_posn = p_sign_posn,
    n_sign_posn = n_sign_posn,
    ...
  )
}

prefix_currency <- function(locale, currency) {
  if (is.na(locale[["p_cs_precedes"]]) || isFALSE(locale[["p_cs_precedes"]])) {
    return("")
  } else {
    currency
  }
}

suffix_currency <- function(locale, currency) {
  if (is.na(locale[["p_cs_precedes"]]) || isTRUE(locale[["p_cs_precedes"]])) {
    return("")
  } else {
    currency
  }
}

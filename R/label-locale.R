#' Formatting numbers
#'
#' The label_xxx_locale family of functions makes it easy to format numbers
#'     in decimal format, as percentages or as currencies.
#'
#' * the `number` ones format axis text as numbers.
#' * the `percent` ones format axis text as percentages.
#' * the `dollar` ones format axis text as dollars.
#' * the `euro` ones format axis text as euros.
#'
#' @inheritParams scales::label_number
#' @param prefix Symbol to display before value.
#' @param suffix Symbol to display after value.
#' @param locale locale string. Defaults to "en-US"
#' @param p_sep_by separator between currency symbol and positive
#'     monteary value
#' @param n_sep_by separator between currency symbol and negative
#'     monteary value
#' @param currency symbol
#'
#' @name label-locale
NULL

#' @rdname label-locale
#' @export
label_number_locale <- function(accuracy = NULL, scale = 1, prefix = "",
                                suffix = "", big.mark = NULL, decimal.mark = NULL,
                                locale = "en-US",
                                trim = TRUE, ...) {
  locale <- check_locale(locale)
  big.mark <- check_big(big.mark, locale)
  decimal.mark <- check_decimal(decimal.mark, locale)

  label_number(
    accuracy = accuracy, scale = scale, prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark, trim = trim, ...
  )
}

#' @rdname label-locale
#' @export
label_percent_locale <- function(accuracy = NULL, scale = 100, prefix = NULL,
                                 suffix = NULL, big.mark = NULL, decimal.mark = NULL,
                                 locale = "en-US",
                                 trim = TRUE, ...) {
  locale <- check_locale(locale)
  big.mark <- check_big(big.mark, locale)
  decimal.mark <- check_decimal(decimal.mark, locale)
  style_negative <- locale[["style_negative"]]
  style_positive <- locale[["style_positive"]]

  if (locale[["percent_precedes"]]) {
    prefix <- prefix %||% paste0("%", rep("\u00a0", locale[["percent_sep_by"]]))
    suffix <- suffix %||% ""
  } else {
    prefix <- prefix %||% ""
    suffix <- suffix %||% paste0(rep("\u00a0", locale[["percent_sep_by"]]), "%")
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
                                  currency = .dollar,
                                  locale = "en-US",
                                  trim = TRUE, ...) {
  # locale <- "de-CH"
  locale <- check_locale(locale)
  big.mark <- check_big_currency(big.mark, locale)
  decimal.mark <- check_decimal_currency(decimal.mark, locale)

  p_sign <- "" %||% locale[["positive_sign"]]
  n_sign <- "-" %||% locale[["negative_sign"]]

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

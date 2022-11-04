#' Formatteing numbers
#'
#' The label_xxx_xxx family of functions makes it easy to format numbers
#'     in decimal format, as percentages or as currencies.
#'
#' * the `_number` ones format axis text as numbers.
#' * the `_percent` ones format axis text as percentages.
#' * the `_dollar` ones format axis text as dollars.
#' * the `_euro` ones format axis text as euros.
#'
#' @inheritParams scales::label_number
#' @param prefix Symbol to display before value.
#' @param suffix Symbol to display after value.
#' @param locale locale string. Defaults to "en_US"
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

  label_number(accuracy = accuracy, scale = scale, prefix = prefix, suffix = suffix,
               big.mark = big.mark, decimal.mark = decimal.mark, trim = trim, ...)
}

#' @rdname label-locale
#' @export
label_perent_locale <- function(accuracy = NULL, scale = 1, prefix = "",
                                suffix = NULL, big.mark = NULL, decimal.mark = NULL,
                                locale = "en-US",
                                trim = TRUE, ...) {

  locale <- check_locale(locale)
  big.mark <- check_big(big.mark, locale)
  decimal.mark <- check_decimal(decimal.mark, locale)
  suffix <- suffix %||% paste0(rep(" ", locale[["p_sep_by_space"]]), "%")

  label_number(accuracy = accuracy, scale = scale, prefix = prefix, suffix = suffix,
               big.mark = big.mark, decimal.mark = decimal.mark, trim = trim, ...)
}

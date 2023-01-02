#' Swiss Style Formatting of Numbers
#'
#' The label_xxx_xxx family of functions makes it easy to format numbers
#'     in decimal format, as percentages or as currencies.
#'
#' * the `_number` ones format axis text as numbers.
#' * the `_percent` ones format axis text as percentages.
#' * the `_dollar` ones format axis text as dollars.
#' * the `_euro` ones format axis text as euros.
#'
#' @param x a numeric
#' @inheritParams scales::label_number
#' @param prefix Symbol to display before value.
#' @param suffix Symbol to display after value.
#'
#' @name label-ch
#'
#' @export
#'
#' @example inst/ex/ex-label_number_ch.R
label_number_ch <- function(accuracy = 1, scale = 1, prefix = "", suffix = "", big.mark = NULL, decimal.mark = NULL, trim = TRUE, ...) {
  label_number_locale(accuracy = accuracy, scale = scale, prefix = prefix, suffix = suffix,
               big.mark = big.mark, decimal.mark = decimal.mark, locale = "de-CH", trim = trim, ...)
}

#' @rdname label-ch
#' @export
number_ch <- function(x, accuracy = 1, scale = 1, prefix = "", suffix = "", big.mark = NULL, decimal.mark = NULL, trim = TRUE, ...) {
  label_number_ch(accuracy = accuracy, scale = scale, big.mark = big.mark, decimal.mark = decimal.mark, trim = trim, ...)(x)
}

#' @rdname label-ch
#' @export
label_percent_ch <- function(accuracy = 1, scale = 100, prefix = "", suffix = NULL, big.mark = NULL, decimal.mark = NULL, trim = TRUE, ...) {
  label_percent_locale(accuracy = accuracy, scale = scale, prefix = prefix, suffix = suffix,
               big.mark = big.mark, decimal.mark = decimal.mark, locale = "de-CH", trim = trim, ...)
}

#' @rdname label-ch
#' @export
percent_ch <- function(x, accuracy = 1, scale = 100, prefix = "", suffix = NULL, big.mark = NULL, decimal.mark = NULL, trim = TRUE, ...) {
  label_percent_ch(accuracy = accuracy, scale = scale, prefix = prefix, suffix = suffix,
               big.mark = big.mark, decimal.mark = decimal.mark, ltrim = trim, ...)(x)
}

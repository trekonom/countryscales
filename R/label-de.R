#' German Style Formatting of Numbers
#'
#' The label_xxx_xxx family of functions makes it easy to format numbers
#'     in decimal format, as percentages or as currencies.
#'
#' * `label_number_de` formats numbers in decimal format.
#' * `label_percent_de` formats numbers as percentages.
#' * `label_currency_de` formats numbers as currencies.
#'
#' @param x a numeric
#' @inheritParams label_number_locale
#'
#' @name label-de
#'
#' @export
#'
#' @example inst/ex/ex-label-de.R
label_number_de <- function(accuracy = 1, scale = 1, prefix = "", suffix = "", big.mark = NULL, decimal.mark = NULL, trim = TRUE, ...) {
  label_number_locale(accuracy = accuracy, scale = scale, prefix = prefix, suffix = suffix,
               big.mark = big.mark, decimal.mark = decimal.mark, locale = "de-DE", trim = trim, ...)
}

#' @rdname label-de
#' @export
number_de <- function(x, accuracy = 1, scale = 1, prefix = "", suffix = "", big.mark = NULL, decimal.mark = NULL, trim = TRUE, ...) {
  label_number_de(accuracy = accuracy, scale = scale, big.mark = big.mark, decimal.mark = decimal.mark, trim = trim, ...)(x)
}

#' @rdname label-de
#' @export
label_percent_de <- function(accuracy = 1, scale = 100, prefix = "", suffix = NULL, big.mark = NULL, decimal.mark = NULL, trim = TRUE, ...) {
  label_percent_locale(accuracy = accuracy, scale = scale, prefix = prefix, suffix = suffix,
               big.mark = big.mark, decimal.mark = decimal.mark, locale = "de-DE", trim = trim, ...)
}

#' @rdname label-de
#' @export
percent_de <- function(x, accuracy = 1, scale = 100, prefix = "", suffix = NULL, big.mark = NULL, decimal.mark = NULL, trim = TRUE, ...) {
  label_percent_de(accuracy = accuracy, scale = scale, prefix = prefix, suffix = suffix,
               big.mark = big.mark, decimal.mark = decimal.mark, ltrim = trim, ...)(x)
}

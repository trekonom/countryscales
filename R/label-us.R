#' US Style Formatting of Numbers
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
#' @name label-us
#'
#' @export
#'
#' @example inst/ex/ex-label-us.R
label_number_us <- function(accuracy = 1, scale = 1, prefix = "", suffix = "", big.mark = NULL, decimal.mark = NULL, trim = TRUE, ...) {
  label_number_locale(accuracy = accuracy, scale = scale, prefix = prefix, suffix = suffix,
                      big.mark = big.mark, decimal.mark = decimal.mark, locale = "en-US", trim = trim, ...)
}

#' @rdname label-us
#' @export
number_us <- function(x, accuracy = 1, scale = 1, prefix = "", suffix = "", big.mark = NULL, decimal.mark = NULL, trim = TRUE, ...) {
  label_number_us(accuracy = accuracy, scale = scale, big.mark = big.mark, decimal.mark = decimal.mark, trim = trim, ...)(x)
}

#' @rdname label-us
#' @export
label_percent_us <- function(accuracy = 1, scale = 100, prefix = "", suffix = NULL, big.mark = NULL, decimal.mark = NULL, trim = TRUE, ...) {
  label_percent_locale(accuracy = accuracy, scale = scale, prefix = prefix, suffix = suffix,
                       big.mark = big.mark, decimal.mark = decimal.mark, locale = "en-US", trim = trim, ...)
}

#' @rdname label-us
#' @export
percent_us <- function(x, accuracy = 1, scale = 100, prefix = "", suffix = NULL, big.mark = NULL, decimal.mark = NULL, trim = TRUE, ...) {
  label_percent_us(accuracy = accuracy, scale = scale, prefix = prefix, suffix = suffix,
                   big.mark = big.mark, decimal.mark = decimal.mark, ltrim = trim, ...)(x)
}

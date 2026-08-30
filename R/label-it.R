#' Italian Style Formatting of Numbers
#'
#' The label_xxx_it family of functions makes it easy to format numbers
#'     in decimal format, as percentages or as currencies.
#'
#' * `label_number_it` formats numbers in decimal format.
#' * `label_percent_it` formats numbers as percentages.
#' * `label_currency_it` formats numbers as currencies.
#'
#' @inheritParams label_number_locale
#'
#' @name label-it
#'
#' @export
#'
#' @example inst/ex/ex-label-it.R
label_number_it <- function(accuracy = 1, scale = 1,
                            prefix = "", suffix = "",
                            big.mark = NULL, decimal.mark = NULL,
                            trim = TRUE, ...) {
  label_number_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "it-IT",
    trim = trim,
    ...
  )
}

#' @rdname label-it
#' @export
label_percent_it <- function(accuracy = 1, scale = 100,
                             prefix = "", suffix = NULL,
                             big.mark = NULL, decimal.mark = NULL,
                             trim = TRUE, ...) {
  label_percent_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "it-IT",
    trim = trim,
    ...
  )
}

#' @rdname label-it
#' @export
label_currency_it <- function(accuracy = 1, scale = 1, currency = "EUR",
                              prefix = NULL, suffix = NULL,
                              big.mark = NULL, decimal.mark = NULL,
                              trim = TRUE, ...) {
  label_currency_locale(
    accuracy = accuracy, scale = scale, currency = currency,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "it-IT",
    trim = trim,
    ...
  )
}


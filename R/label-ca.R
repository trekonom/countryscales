#' Canadian Style Formatting of Numbers
#'
#' The label_xxx_ca family of functions makes it easy to format numbers
#'     in decimal format, as percentages or as currencies.
#'
#' * `label_number_ca` formats numbers in decimal format.
#' * `label_percent_ca` formats numbers as percentages.
#' * `label_currency_ca` formats numbers as currencies.
#'
#' @inheritParams label_number_locale
#'
#' @name label-ca
#'
#' @export
#'
#' @example inst/ex/ex-label-ca.R
label_number_ca <- function(accuracy = 1, scale = 1,
                            prefix = "", suffix = "",
                            big.mark = NULL, decimal.mark = NULL,
                            trim = TRUE, ...) {
  label_number_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "en-CA",
    trim = trim,
    ...
  )
}

#' @rdname label-ca
#' @export
label_percent_ca <- function(accuracy = 1, scale = 100,
                             prefix = "", suffix = NULL,
                             big.mark = NULL, decimal.mark = NULL,
                             trim = TRUE, ...) {
  label_percent_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "en-CA",
    trim = trim,
    ...
  )
}

#' @rdname label-ca
#' @export
label_currency_ca <- function(accuracy = 1, scale = 1, currency = "CAD",
                              prefix = NULL, suffix = NULL,
                              big.mark = NULL, decimal.mark = NULL,
                              trim = TRUE, ...) {
  label_currency_locale(
    accuracy = accuracy, scale = scale, currency = currency,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "en-CA",
    trim = trim,
    ...
  )
}


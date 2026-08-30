#' Mexican Style Formatting of Numbers
#'
#' The label_xxx_mx family of functions makes it easy to format numbers
#'     in decimal format, as percentages or as currencies.
#'
#' * `label_number_mx` formats numbers in decimal format.
#' * `label_percent_mx` formats numbers as percentages.
#' * `label_currency_mx` formats numbers as currencies.
#'
#' @inheritParams label_number_locale
#'
#' @name label-mx
#'
#' @export
#'
#' @example inst/ex/ex-label-mx.R
label_number_mx <- function(accuracy = 1, scale = 1,
                            prefix = "", suffix = "",
                            big.mark = NULL, decimal.mark = NULL,
                            trim = TRUE, ...) {
  label_number_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "es-MX",
    trim = trim,
    ...
  )
}

#' @rdname label-mx
#' @export
label_percent_mx <- function(accuracy = 1, scale = 100,
                             prefix = "", suffix = NULL,
                             big.mark = NULL, decimal.mark = NULL,
                             trim = TRUE, ...) {
  label_percent_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "es-MX",
    trim = trim,
    ...
  )
}

#' @rdname label-mx
#' @export
label_currency_mx <- function(accuracy = 1, scale = 1, currency = "MXN",
                              prefix = NULL, suffix = NULL,
                              big.mark = NULL, decimal.mark = NULL,
                              trim = TRUE, ...) {
  label_currency_locale(
    accuracy = accuracy, scale = scale, currency = currency,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "es-MX",
    trim = trim,
    ...
  )
}


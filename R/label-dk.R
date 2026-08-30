#' Danish Style Formatting of Numbers
#'
#' The label_xxx_dk family of functions makes it easy to format numbers
#'     in decimal format, as percentages or as currencies.
#'
#' * `label_number_dk` formats numbers in decimal format.
#' * `label_percent_dk` formats numbers as percentages.
#' * `label_currency_dk` formats numbers as currencies.
#'
#' @inheritParams label_number_locale
#'
#' @name label-dk
#'
#' @export
#'
#' @example inst/ex/ex-label-dk.R
label_number_dk <- function(accuracy = 1, scale = 1,
                            prefix = "", suffix = "",
                            big.mark = NULL, decimal.mark = NULL,
                            trim = TRUE, ...) {
  label_number_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "da-DK",
    trim = trim,
    ...
  )
}

#' @rdname label-dk
#' @export
label_percent_dk <- function(accuracy = 1, scale = 100,
                             prefix = "", suffix = NULL,
                             big.mark = NULL, decimal.mark = NULL,
                             trim = TRUE, ...) {
  label_percent_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "da-DK",
    trim = trim,
    ...
  )
}

#' @rdname label-dk
#' @export
label_currency_dk <- function(accuracy = 1, scale = 1, currency = "DKK",
                              prefix = NULL, suffix = NULL,
                              big.mark = NULL, decimal.mark = NULL,
                              trim = TRUE, ...) {
  label_currency_locale(
    accuracy = accuracy, scale = scale, currency = currency,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "da-DK",
    trim = trim,
    ...
  )
}


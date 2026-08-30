#' British Style Formatting of Numbers
#'
#' The label_xxx_gb family of functions makes it easy to format numbers
#'     in decimal format, as percentages or as currencies.
#'
#' * `label_number_gb` formats numbers in decimal format.
#' * `label_percent_gb` formats numbers as percentages.
#' * `label_currency_gb` formats numbers as currencies.
#'
#' @inheritParams label_number_locale
#'
#' @name label-gb
#'
#' @export
#'
#' @example inst/ex/ex-label-gb.R
label_number_gb <- function(accuracy = 1, scale = 1,
                            prefix = "", suffix = "",
                            big.mark = NULL, decimal.mark = NULL,
                            trim = TRUE, ...) {
  label_number_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "en-GB",
    trim = trim,
    ...
  )
}

#' @rdname label-gb
#' @export
label_percent_gb <- function(accuracy = 1, scale = 100,
                             prefix = "", suffix = NULL,
                             big.mark = NULL, decimal.mark = NULL,
                             trim = TRUE, ...) {
  label_percent_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "en-GB",
    trim = trim,
    ...
  )
}

#' @rdname label-gb
#' @export
label_currency_gb <- function(accuracy = 1, scale = 1, currency = "GBP",
                              prefix = NULL, suffix = NULL,
                              big.mark = NULL, decimal.mark = NULL,
                              trim = TRUE, ...) {
  label_currency_locale(
    accuracy = accuracy, scale = scale, currency = currency,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "en-GB",
    trim = trim,
    ...
  )
}


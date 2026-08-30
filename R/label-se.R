#' Swedish Style Formatting of Numbers
#'
#' The label_xxx_se family of functions makes it easy to format numbers
#'     in decimal format, as percentages or as currencies.
#'
#' * `label_number_se` formats numbers in decimal format.
#' * `label_percent_se` formats numbers as percentages.
#' * `label_currency_se` formats numbers as currencies.
#'
#' @inheritParams label_number_locale
#'
#' @name label-se
#'
#' @export
#'
#' @example inst/ex/ex-label-se.R
label_number_se <- function(accuracy = 1, scale = 1,
                            prefix = "", suffix = "",
                            big.mark = NULL, decimal.mark = NULL,
                            trim = TRUE, ...) {
  label_number_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "sv-SE",
    trim = trim,
    ...
  )
}

#' @rdname label-se
#' @export
label_percent_se <- function(accuracy = 1, scale = 100,
                             prefix = "", suffix = NULL,
                             big.mark = NULL, decimal.mark = NULL,
                             trim = TRUE, ...) {
  label_percent_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "sv-SE",
    trim = trim,
    ...
  )
}

#' @rdname label-se
#' @export
label_currency_se <- function(accuracy = 1, scale = 1, currency = "SEK",
                              prefix = NULL, suffix = NULL,
                              big.mark = NULL, decimal.mark = NULL,
                              trim = TRUE, ...) {
  label_currency_locale(
    accuracy = accuracy, scale = scale, currency = currency,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "sv-SE",
    trim = trim,
    ...
  )
}


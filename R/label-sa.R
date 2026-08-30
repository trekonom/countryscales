#' Saudi Style Formatting of Numbers
#'
#' The label_xxx_sa family of functions makes it easy to format numbers
#'     in decimal format, as percentages or as currencies.
#'
#' * `label_number_sa` formats numbers in decimal format.
#' * `label_percent_sa` formats numbers as percentages.
#' * `label_currency_sa` formats numbers as currencies.
#'
#' @inheritParams label_number_locale
#'
#' @name label-sa
#'
#' @export
#'
#' @example inst/ex/ex-label-sa.R
label_number_sa <- function(accuracy = 1, scale = 1,
                            prefix = "", suffix = "",
                            big.mark = NULL, decimal.mark = NULL,
                            trim = TRUE, ...) {
  label_number_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "ar-SA",
    trim = trim,
    ...
  )
}

#' @rdname label-sa
#' @export
label_percent_sa <- function(accuracy = 1, scale = 100,
                             prefix = NULL, suffix = NULL,
                             big.mark = NULL, decimal.mark = NULL,
                             trim = TRUE, ...) {
  label_percent_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "ar-SA",
    trim = trim,
    ...
  )
}

#' @rdname label-sa
#' @export
label_currency_sa <- function(accuracy = 1, scale = 1, currency = "SAR",
                              prefix = NULL, suffix = NULL,
                              big.mark = NULL, decimal.mark = NULL,
                              trim = TRUE, ...) {
  label_currency_locale(
    accuracy = accuracy, scale = scale, currency = currency,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "ar-SA",
    trim = trim,
    ...
  )
}


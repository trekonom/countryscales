#' Turkish Style Formatting of Numbers
#'
#' The label_xxx_tr family of functions makes it easy to format numbers
#'     in decimal format, as percentages or as currencies.
#'
#' * `label_number_tr` formats numbers in decimal format.
#' * `label_percent_tr` formats numbers as percentages.
#' * `label_currency_tr` formats numbers as currencies.
#'
#' @inheritParams label_number_locale
#'
#' @name label-tr
#'
#' @export
#'
#' @example inst/ex/ex-label-tr.R
label_number_tr <- function(accuracy = 1, scale = 1,
                            prefix = "", suffix = "",
                            big.mark = NULL, decimal.mark = NULL,
                            trim = TRUE, ...) {
  label_number_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "tr-TR",
    trim = trim,
    ...
  )
}

#' @rdname label-tr
#' @export
label_percent_tr <- function(accuracy = 1, scale = 100,
                             prefix = NULL, suffix = NULL,
                             big.mark = NULL, decimal.mark = NULL,
                             trim = TRUE, ...) {
  label_percent_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "tr-TR",
    trim = trim,
    ...
  )
}

#' @rdname label-tr
#' @export
label_currency_tr <- function(accuracy = 1, scale = 1, currency = "TRY",
                              prefix = NULL, suffix = NULL,
                              big.mark = NULL, decimal.mark = NULL,
                              trim = TRUE, ...) {
  label_currency_locale(
    accuracy = accuracy, scale = scale, currency = currency,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "tr-TR",
    trim = trim,
    ...
  )
}


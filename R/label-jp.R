#' Japanese Style Formatting of Numbers
#'
#' The label_xxx_jp family of functions makes it easy to format numbers
#'     in decimal format, as percentages or as currencies.
#'
#' * `label_number_jp` formats numbers in decimal format.
#' * `label_percent_jp` formats numbers as percentages.
#' * `label_currency_jp` formats numbers as currencies.
#'
#' @inheritParams label_number_locale
#'
#' @name label-jp
#'
#' @export
#'
#' @example inst/ex/ex-label-jp.R
label_number_jp <- function(accuracy = 1, scale = 1,
                            prefix = "", suffix = "",
                            big.mark = NULL, decimal.mark = NULL,
                            trim = TRUE, ...) {
  label_number_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "ja-JP",
    trim = trim,
    ...
  )
}

#' @rdname label-jp
#' @export
label_percent_jp <- function(accuracy = 1, scale = 100,
                             prefix = "", suffix = NULL,
                             big.mark = NULL, decimal.mark = NULL,
                             trim = TRUE, ...) {
  label_percent_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "ja-JP",
    trim = trim,
    ...
  )
}

#' @rdname label-jp
#' @export
label_currency_jp <- function(accuracy = 1, scale = 1, currency = "JPY",
                              prefix = NULL, suffix = NULL,
                              big.mark = NULL, decimal.mark = NULL,
                              trim = TRUE, ...) {
  label_currency_locale(
    accuracy = accuracy, scale = scale, currency = currency,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "ja-JP",
    trim = trim,
    ...
  )
}


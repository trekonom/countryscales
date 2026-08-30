#' South Korean Style Formatting of Numbers
#'
#' The label_xxx_kr family of functions makes it easy to format numbers
#'     in decimal format, as percentages or as currencies.
#'
#' * `label_number_kr` formats numbers in decimal format.
#' * `label_percent_kr` formats numbers as percentages.
#' * `label_currency_kr` formats numbers as currencies.
#'
#' @inheritParams label_number_locale
#'
#' @name label-kr
#'
#' @export
#'
#' @example inst/ex/ex-label-kr.R
label_number_kr <- function(accuracy = 1, scale = 1,
                            prefix = "", suffix = "",
                            big.mark = NULL, decimal.mark = NULL,
                            trim = TRUE, ...) {
  label_number_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "ko-KR",
    trim = trim,
    ...
  )
}

#' @rdname label-kr
#' @export
label_percent_kr <- function(accuracy = 1, scale = 100,
                             prefix = "", suffix = NULL,
                             big.mark = NULL, decimal.mark = NULL,
                             trim = TRUE, ...) {
  label_percent_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "ko-KR",
    trim = trim,
    ...
  )
}

#' @rdname label-kr
#' @export
label_currency_kr <- function(accuracy = 1, scale = 1, currency = "KRW",
                              prefix = NULL, suffix = NULL,
                              big.mark = NULL, decimal.mark = NULL,
                              trim = TRUE, ...) {
  label_currency_locale(
    accuracy = accuracy, scale = scale, currency = currency,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "ko-KR",
    trim = trim,
    ...
  )
}


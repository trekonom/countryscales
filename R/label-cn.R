#' Chinese Style Formatting of Numbers
#'
#' The label_xxx_cn family of functions makes it easy to format numbers
#'     in decimal format, as percentages or as currencies.
#'
#' * `label_number_cn` formats numbers in decimal format.
#' * `label_percent_cn` formats numbers as percentages.
#' * `label_currency_cn` formats numbers as currencies.
#'
#' @inheritParams label_number_locale
#'
#' @name label-cn
#'
#' @export
#'
#' @example inst/ex/ex-label-cn.R
label_number_cn <- function(accuracy = 1, scale = 1,
                            prefix = "", suffix = "",
                            big.mark = NULL, decimal.mark = NULL,
                            trim = TRUE, ...) {
  label_number_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "zh-Hans-CN",
    trim = trim,
    ...
  )
}

#' @rdname label-cn
#' @export
label_percent_cn <- function(accuracy = 1, scale = 100,
                             prefix = "", suffix = NULL,
                             big.mark = NULL, decimal.mark = NULL,
                             trim = TRUE, ...) {
  label_percent_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "zh-Hans-CN",
    trim = trim,
    ...
  )
}

#' @rdname label-cn
#' @export
label_currency_cn <- function(accuracy = 1, scale = 1, currency = "CNY",
                              prefix = NULL, suffix = NULL,
                              big.mark = NULL, decimal.mark = NULL,
                              trim = TRUE, ...) {
  label_currency_locale(
    accuracy = accuracy, scale = scale, currency = currency,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "zh-Hans-CN",
    trim = trim,
    ...
  )
}


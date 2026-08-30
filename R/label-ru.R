#' Russian Style Formatting of Numbers
#'
#' The label_xxx_ru family of functions makes it easy to format numbers
#'     in decimal format, as percentages or as currencies.
#'
#' * `label_number_ru` formats numbers in decimal format.
#' * `label_percent_ru` formats numbers as percentages.
#' * `label_currency_ru` formats numbers as currencies.
#'
#' @inheritParams label_number_locale
#'
#' @name label-ru
#'
#' @export
#'
#' @example inst/ex/ex-label-ru.R
label_number_ru <- function(accuracy = 1, scale = 1,
                            prefix = "", suffix = "",
                            big.mark = NULL, decimal.mark = NULL,
                            trim = TRUE, ...) {
  label_number_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "ru-RU",
    trim = trim,
    ...
  )
}

#' @rdname label-ru
#' @export
label_percent_ru <- function(accuracy = 1, scale = 100,
                             prefix = NULL, suffix = NULL,
                             big.mark = NULL, decimal.mark = NULL,
                             trim = TRUE, ...) {
  label_percent_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "ru-RU",
    trim = trim,
    ...
  )
}

#' @rdname label-ru
#' @export
label_currency_ru <- function(accuracy = 1, scale = 1, currency = "RUB",
                              prefix = NULL, suffix = NULL,
                              big.mark = NULL, decimal.mark = NULL,
                              trim = TRUE, ...) {
  label_currency_locale(
    accuracy = accuracy, scale = scale, currency = currency,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "ru-RU",
    trim = trim,
    ...
  )
}


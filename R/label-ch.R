#' Swiss Style Formatting of Numbers
#'
#' The label_xxx_xxx family of functions makes it easy to format numbers
#'     in decimal format, as percentages or as currencies.
#'
#' * `label_number_ch` formats numbers in decimal format.
#' * `label_percent_ch` formats numbers as percentages.
#' * `label_currency_ch` formats numbers as currencies.
#'
#' @param x a numeric
#' @inheritParams label_number_locale
#'
#' @name label-ch
#'
#' @export
#'
#' @example inst/ex/ex-label-ch.R
label_number_ch <- function(accuracy = 1, scale = 1,
                            prefix = "", suffix = "",
                            big.mark = NULL, decimal.mark = NULL,
                            trim = TRUE, ...) {
  label_number_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "de-CH",
    trim = trim,
    ...
  )
}

#' @rdname label-ch
#' @export
number_ch <- function(x, accuracy = 1, scale = 1,
                      prefix = "", suffix = "",
                      big.mark = NULL, decimal.mark = NULL,
                      trim = TRUE, ...) {
  label_number_ch(
    accuracy = accuracy, scale = scale,
    big.mark = big.mark, decimal.mark = decimal.mark,
    trim = trim,
    ...
  )(x)
}

#' @rdname label-ch
#' @export
label_percent_ch <- function(accuracy = 1, scale = 100,
                             prefix = "", suffix = NULL,
                             big.mark = NULL, decimal.mark = NULL,
                             trim = TRUE, ...) {
  label_percent_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "de-CH",
    trim = trim,
    ...
  )
}

#' @rdname label-ch
#' @export
percent_ch <- function(x, accuracy = 1, scale = 100,
                       prefix = "", suffix = NULL,
                       big.mark = NULL, decimal.mark = NULL,
                       trim = TRUE, ...) {
  label_percent_ch(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    trim = trim,
    ...
  )(x)
}

#' @rdname label-ch
#' @export
label_currency_ch <- function(accuracy = 1, scale = 1, currency = "CHF",
                              prefix = NULL, suffix = NULL,
                              big.mark = NULL, decimal.mark = NULL,
                              trim = TRUE, ...) {
  label_currency_locale(
    accuracy = accuracy, scale = scale, currency = currency,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "de-CH",
    trim = trim,
    ...
  )
}

#' @rdname label-ch
#' @export
currency_ch <- function(x, accuracy = 1, scale = 1, currency = "CHF",
                        prefix = NULL, suffix = NULL,
                        big.mark = NULL, decimal.mark = NULL,
                        trim = TRUE, ...) {
  label_currency_ch(
    accuracy = accuracy, scale = scale, currency = currency,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    trim = trim,
    ...
  )(x)
}

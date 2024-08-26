#' US Style Formatting of Numbers
#'
#' The label_xxx_xxx family of functions makes it easy to format numbers
#'     in decimal format, as percentages or as currencies.
#'
#' * `label_number_us` formats numbers in decimal format.
#' * `label_percent_us` formats numbers as percentages.
#' * `label_currency_us` formats numbers as currencies.
#'
#' @param x a numeric
#' @inheritParams label_number_locale
#'
#' @name label-us
#'
#' @export
#'
#' @example inst/ex/ex-label-us.R
label_number_us <- function(accuracy = 1, scale = 1,
                            prefix = "", suffix = "",
                            big.mark = NULL, decimal.mark = NULL,
                            trim = TRUE, ...) {
  label_number_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "en-US",
    trim = trim,
    ...
  )
}

#' @rdname label-us
#' @export
number_us <- function(x, accuracy = 1, scale = 1,
                      prefix = "", suffix = "",
                      big.mark = NULL, decimal.mark = NULL,
                      trim = TRUE, ...) {
  label_number_us(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    trim = trim,
    ...
  )(x)
}

#' @rdname label-us
#' @export
label_percent_us <- function(accuracy = 1, scale = 100,
                             prefix = "", suffix = NULL,
                             big.mark = NULL, decimal.mark = NULL,
                             trim = TRUE, ...) {
  label_percent_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "en-US",
    trim = trim,
    ...
  )
}

#' @rdname label-us
#' @export
percent_us <- function(x, accuracy = 1, scale = 100,
                       prefix = "", suffix = NULL,
                       big.mark = NULL, decimal.mark = NULL,
                       trim = TRUE, ...) {
  label_percent_us(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    trim = trim,
    ...
  )(x)
}

#' @rdname label-us
#' @export
label_currency_us <- function(accuracy = 1, scale = 1, currency = "USD",
                              prefix = NULL, suffix = NULL,
                              big.mark = NULL, decimal.mark = NULL,
                              trim = TRUE, ...) {
  label_currency_locale(
    accuracy = accuracy, scale = scale, currency = currency,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "en-US",
    trim = trim,
    ...
  )
}

#' @rdname label-us
#' @export
currency_us <- function(x, accuracy = 1, scale = 1, currency = "USD",
                        prefix = NULL, suffix = NULL,
                        big.mark = NULL, decimal.mark = NULL,
                        trim = TRUE, ...) {
  label_currency_us(
    accuracy = accuracy, scale = scale, currency = currency,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    trim = trim,
    ...
  )(x)
}

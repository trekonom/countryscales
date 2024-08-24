#' German Style Formatting of Numbers
#'
#' The label_xxx_xxx family of functions makes it easy to format numbers
#'     in decimal format, as percentages or as currencies.
#'
#' * `label_number_de` formats numbers in decimal format.
#' * `label_percent_de` formats numbers as percentages.
#' * `label_currency_de` formats numbers as currencies.
#'
#' @param x a numeric
#' @inheritParams label_number_locale
#'
#' @name label-de
#'
#' @export
#'
#' @examples
#' \dontrun{
#' require(scales)
#' demo_continuous(
#'   c(-1e6, 1e6),
#'   labels = label_number_de()
#' )
#' demo_continuous(
#'   c(-1, 1),
#'   labels = label_percent_de(accuracy = .01)
#' )
#' demo_continuous(
#'   c(-1, 1),
#'   labels = label_currency_de(accuracy = .1)
#' )
#' }
label_number_de <- function(accuracy = 1, scale = 1,
                            prefix = "", suffix = "",
                            big.mark = NULL, decimal.mark = NULL,
                            trim = TRUE, ...) {
  label_number_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "de-DE",
    trim = trim,
    ...
  )
}

#' @rdname label-de
#' @export
number_de <- function(x, accuracy = 1, scale = 1,
                      prefix = "", suffix = "",
                      big.mark = NULL, decimal.mark = NULL,
                      trim = TRUE, ...) {
  label_number_de(
    accuracy = accuracy, scale = scale,
    big.mark = big.mark, decimal.mark = decimal.mark,
    trim = trim,
    ...
  )(x)
}

#' @rdname label-de
#' @export
label_percent_de <- function(accuracy = 1, scale = 100,
                             prefix = "", suffix = NULL,
                             big.mark = NULL, decimal.mark = NULL,
                             trim = TRUE, ...) {
  label_percent_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "de-DE",
    trim = trim,
    ...
  )
}

#' @rdname label-de
#' @export
percent_de <- function(x, accuracy = 1, scale = 100,
                       prefix = "", suffix = NULL,
                       big.mark = NULL, decimal.mark = NULL,
                       trim = TRUE, ...) {
  label_percent_de(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    trim = trim,
    ...
  )(x)
}

#' @rdname label-de
#' @export
label_currency_de <- function(accuracy = 1, scale = 1, currency = "EUR",
                              prefix = NULL, suffix = NULL,
                              big.mark = NULL, decimal.mark = NULL,
                              trim = TRUE, ...) {
  label_currency_locale(
    accuracy = accuracy, scale = scale, currency = currency,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "de-DE",
    trim = trim,
    ...
  )
}

#' @rdname label-de
#' @export
currency_de <- function(x, accuracy = 1, scale = 1, currency = "EUR",
                        prefix = NULL, suffix = NULL,
                        big.mark = NULL, decimal.mark = NULL,
                        trim = TRUE, ...) {
  label_currency_de(
    accuracy = accuracy, scale = scale, currency = currency,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    trim = trim,
    ...
  )(x)
}

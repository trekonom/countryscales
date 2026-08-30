#' South African Style Formatting of Numbers
#'
#' The label_xxx_za family of functions makes it easy to format numbers
#'     in decimal format, as percentages or as currencies.
#'
#' * `label_number_za` formats numbers in decimal format.
#' * `label_percent_za` formats numbers as percentages.
#' * `label_currency_za` formats numbers as currencies.
#'
#' @inheritParams label_number_locale
#'
#' @name label-za
#'
#' @export
#'
#' @example inst/ex/ex-label-za.R
label_number_za <- function(accuracy = 1, scale = 1,
                            prefix = "", suffix = "",
                            big.mark = NULL, decimal.mark = NULL,
                            trim = TRUE, ...) {
  label_number_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "af-ZA",
    trim = trim,
    ...
  )
}

#' @rdname label-za
#' @export
label_percent_za <- function(accuracy = 1, scale = 100,
                             prefix = NULL, suffix = NULL,
                             big.mark = NULL, decimal.mark = NULL,
                             trim = TRUE, ...) {
  label_percent_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "af-ZA",
    trim = trim,
    ...
  )
}

#' @rdname label-za
#' @export
label_currency_za <- function(accuracy = 1, scale = 1, currency = "ZAR",
                              prefix = NULL, suffix = NULL,
                              big.mark = NULL, decimal.mark = NULL,
                              trim = TRUE, ...) {
  label_currency_locale(
    accuracy = accuracy, scale = scale, currency = currency,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "af-ZA",
    trim = trim,
    ...
  )
}


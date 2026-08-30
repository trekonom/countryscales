#' French Style Formatting of Numbers
#'
#' The label_xxx_fr family of functions makes it easy to format numbers
#'     in decimal format, as percentages or as currencies.
#'
#' * `label_number_fr` formats numbers in decimal format.
#' * `label_percent_fr` formats numbers as percentages.
#' * `label_currency_fr` formats numbers as currencies.
#'
#' @inheritParams label_number_locale
#'
#' @name label-fr
#'
#' @export
#'
#' @example inst/ex/ex-label-fr.R
label_number_fr <- function(accuracy = 1, scale = 1,
                            prefix = "", suffix = "",
                            big.mark = NULL, decimal.mark = NULL,
                            trim = TRUE, ...) {
  label_number_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "fr-FR",
    trim = trim,
    ...
  )
}

#' @rdname label-fr
#' @export
label_percent_fr <- function(accuracy = 1, scale = 100,
                             prefix = "", suffix = NULL,
                             big.mark = NULL, decimal.mark = NULL,
                             trim = TRUE, ...) {
  label_percent_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "fr-FR",
    trim = trim,
    ...
  )
}

#' @rdname label-fr
#' @export
label_currency_fr <- function(accuracy = 1, scale = 1, currency = "EUR",
                              prefix = NULL, suffix = NULL,
                              big.mark = NULL, decimal.mark = NULL,
                              trim = TRUE, ...) {
  label_currency_locale(
    accuracy = accuracy, scale = scale, currency = currency,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "fr-FR",
    trim = trim,
    ...
  )
}


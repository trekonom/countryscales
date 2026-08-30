#' Norwegian Style Formatting of Numbers
#'
#' The label_xxx_no family of functions makes it easy to format numbers
#'     in decimal format, as percentages or as currencies.
#'
#' * `label_number_no` formats numbers in decimal format.
#' * `label_percent_no` formats numbers as percentages.
#' * `label_currency_no` formats numbers as currencies.
#'
#' @inheritParams label_number_locale
#'
#' @name label-no
#'
#' @export
#'
#' @example inst/ex/ex-label-no.R
label_number_no <- function(accuracy = 1, scale = 1,
                            prefix = "", suffix = "",
                            big.mark = NULL, decimal.mark = NULL,
                            trim = TRUE, ...) {
  label_number_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "nb-NO",
    trim = trim,
    ...
  )
}

#' @rdname label-no
#' @export
label_percent_no <- function(accuracy = 1, scale = 100,
                             prefix = "", suffix = NULL,
                             big.mark = NULL, decimal.mark = NULL,
                             trim = TRUE, ...) {
  label_percent_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "nb-NO",
    trim = trim,
    ...
  )
}

#' @rdname label-no
#' @export
label_currency_no <- function(accuracy = 1, scale = 1, currency = "NOK",
                              prefix = NULL, suffix = NULL,
                              big.mark = NULL, decimal.mark = NULL,
                              trim = TRUE, ...) {
  label_currency_locale(
    accuracy = accuracy, scale = scale, currency = currency,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "nb-NO",
    trim = trim,
    ...
  )
}


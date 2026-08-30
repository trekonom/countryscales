#' Indonesian Style Formatting of Numbers
#'
#' The label_xxx_id family of functions makes it easy to format numbers
#'     in decimal format, as percentages or as currencies.
#'
#' * `label_number_id` formats numbers in decimal format.
#' * `label_percent_id` formats numbers as percentages.
#' * `label_currency_id` formats numbers as currencies.
#'
#' @inheritParams label_number_locale
#'
#' @name label-id
#'
#' @export
#'
#' @example inst/ex/ex-label-id.R
label_number_id <- function(accuracy = 1, scale = 1,
                            prefix = "", suffix = "",
                            big.mark = NULL, decimal.mark = NULL,
                            trim = TRUE, ...) {
  label_number_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "id-ID",
    trim = trim,
    ...
  )
}

#' @rdname label-id
#' @export
label_percent_id <- function(accuracy = 1, scale = 100,
                             prefix = "", suffix = NULL,
                             big.mark = NULL, decimal.mark = NULL,
                             trim = TRUE, ...) {
  label_percent_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "id-ID",
    trim = trim,
    ...
  )
}

#' @rdname label-id
#' @export
label_currency_id <- function(accuracy = 1, scale = 1, currency = "IDR",
                              prefix = NULL, suffix = NULL,
                              big.mark = NULL, decimal.mark = NULL,
                              trim = TRUE, ...) {
  label_currency_locale(
    accuracy = accuracy, scale = scale, currency = currency,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "id-ID",
    trim = trim,
    ...
  )
}


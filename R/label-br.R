#' Brazilian Style Formatting of Numbers
#'
#' The label_xxx_br family of functions makes it easy to format numbers
#'     in decimal format, as percentages or as currencies.
#'
#' * `label_number_br` formats numbers in decimal format.
#' * `label_percent_br` formats numbers as percentages.
#' * `label_currency_br` formats numbers as currencies.
#'
#' @inheritParams label_number_locale
#'
#' @name label-br
#'
#' @export
#'
#' @example inst/ex/ex-label-br.R
label_number_br <- function(accuracy = 1, scale = 1,
                            prefix = "", suffix = "",
                            big.mark = NULL, decimal.mark = NULL,
                            trim = TRUE, ...) {
  label_number_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "pt-BR",
    trim = trim,
    ...
  )
}

#' @rdname label-br
#' @export
label_percent_br <- function(accuracy = 1, scale = 100,
                             prefix = "", suffix = NULL,
                             big.mark = NULL, decimal.mark = NULL,
                             trim = TRUE, ...) {
  label_percent_locale(
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "pt-BR",
    trim = trim,
    ...
  )
}

#' @rdname label-br
#' @export
label_currency_br <- function(accuracy = 1, scale = 1, currency = "BRL",
                              prefix = NULL, suffix = NULL,
                              big.mark = NULL, decimal.mark = NULL,
                              trim = TRUE, ...) {
  label_currency_locale(
    accuracy = accuracy, scale = scale, currency = currency,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    locale = "pt-BR",
    trim = trim,
    ...
  )
}


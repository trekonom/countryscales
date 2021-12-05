#' Label currencies (e.g. 2,5€, 50€ or $5,3, $100)
#'
#' Format numbers as currencies
#'
#' @details German wrapper around scales::label_dollar with default \code{big.mark = "."} and
#'    \code{decimal.mark = ","}. For \code{euro_de} the prefix defaults to ""
#'    and the suffix to the euro sign, for \code{dollar_de} the prefix defaults
#'    to "$" and the suffix to "".
#'
#' @inheritParams scales::label_dollar
#' @param ... additional arguments passed to \code{\link[scales]{label_dollar}}, etc.
#'
#' @seealso \code{\link[scales]{label_dollar}}
#'
#' @export
#'
#' @example inst/ex/ex-label_euro_de.R
label_euro_de <- function(accuracy = 1, scale = 1, big.mark = ".", decimal.mark = ",", prefix = "", suffix = "\u20ac", trim = TRUE, ...) {
  scales::label_dollar(
    accuracy = accuracy, scale = scale,
    big.mark = big.mark, decimal.mark = decimal.mark,
    prefix = prefix, suffix = suffix, trim = trim, ...
  )
}

#' @rdname label_euro_de
#' @export
euro_de <- function(x, accuracy = 1, scale = 1, big.mark = ".", decimal.mark = ",", prefix = "", suffix = "\u20ac", trim = TRUE, ...) {
  scales::label_dollar(
    accuracy = accuracy, scale = scale,
    big.mark = big.mark, decimal.mark = decimal.mark,
    prefix = prefix, suffix = suffix, trim = trim, ...
  )(x)
}

#' @rdname label_euro_de
#' @export
label_dollar_de <- function(accuracy = 1, scale = 1, big.mark = ".", decimal.mark = ",", prefix = "$", suffix = "", trim = TRUE, ...) {
  scales::label_dollar(
    accuracy = accuracy, scale = scale,
    big.mark = big.mark, decimal.mark = decimal.mark,
    prefix = prefix, suffix = suffix, trim = trim, ...
  )
}

#' @rdname label_euro_de
#' @export
dollar_de <- function(x, accuracy = 1, scale = 1, big.mark = ".", decimal.mark = ",", prefix = "$", suffix = "", trim = TRUE, ...) {
  scales::label_dollar(x,
    accuracy = accuracy, scale = scale,
    big.mark = big.mark, decimal.mark = decimal.mark,
    prefix = prefix, suffix = suffix, trim = trim, ...
  )(x)
}

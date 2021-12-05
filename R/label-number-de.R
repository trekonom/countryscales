#' Label numbers in decimal format (e.g. 0,12, 1.234)
#'
#' Force decimal display of numbers inserts a point every three digits.
#'
#' @details German wrapper around scales::label_number with default \code{big.mark = "."} and
#'    \code{decimal.mark = ","}
#'
#' @inheritParams scales::label_number
#' @param ... additional arguments passed to \code{\link[scales]{label_number}}, etc.
#'
#' @seealso \code{\link[scales]{label_number}}
#'
#' @export
#'
#' @example inst/ex/ex-label_number_de.R
label_number_de <- function(accuracy = 1, scale = 1, big.mark = ".", decimal.mark = ",", trim = TRUE, ...) {
  scales::label_number(accuracy = accuracy, scale = scale, big.mark = big.mark, decimal.mark = decimal.mark, trim = trim, ...)
}

#' @rdname label_number_de
#' @export
number_de <- function(x, accuracy = 1, scale = 1, big.mark = ".", decimal.mark = ",", trim = TRUE, ...) {
  scales::label_number(accuracy = accuracy, scale = scale, big.mark = big.mark, decimal.mark = decimal.mark, trim = trim, ...)(x)
}

#' Label percentages (e.g. 2,5%, 50%)
#'
#' Force display of numbers as percentages.
#'
#' @details Wrappers around the corrsponding functions from the
#'    [scales](https://scales.r-lib.org/) package with default \code{big.mark = "."} and
#'    \code{decimal.mark = ","}
#'
#' @inheritParams scales::label_percent
#' @param ... additional arguments passed to \code{\link[scales]{label_percent}}, etc.
#'
#' @seealso \code{\link[scales]{label_percent}}
#'
#' @export
#'
#' @example inst/ex/ex-label_percent_de.R
label_percent_de <- function(accuracy = 1, scale = 100, big.mark = ".", decimal.mark = ",", prefix = "",
                             suffix = "%", trim = TRUE, ...) {
  scales::label_percent(
    accuracy = accuracy, scale = scale, big.mark = big.mark, decimal.mark = decimal.mark, prefix = "",
    suffix = suffix, trim = trim, ...
  )
}

#' @rdname label_percent_de
#' @export
percent_de <- function(x, accuracy = 1, scale = 100, big.mark = ".", decimal.mark = ",", prefix = "",
                       suffix = "%", trim = TRUE, ...) {
  scales::label_percent(
    accuracy = accuracy, scale = scale, big.mark = big.mark, decimal.mark = decimal.mark, prefix = "",
    suffix = suffix, trim = trim, ...
  )(x)
}

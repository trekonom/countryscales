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
label_euro <- function(accuracy = NULL, scale = 1, big.mark = ",", decimal.mark = ".", prefix = "", suffix = .euro, trim = TRUE,
                       largest_with_cents = 1e+05, negative_parens = FALSE, ...) {
  scales::label_dollar(
    accuracy = accuracy, scale = scale,
    big.mark = big.mark, decimal.mark = decimal.mark,
    prefix = prefix, suffix = suffix, trim = trim,
    largest_with_cents = largest_with_cents, negative_parens = negative_parens,
    ...
  )
}

#' @rdname label_euro
#' @export
euro <- function(x, accuracy = 1, scale = 1, big.mark = ".", decimal.mark = ",", prefix = "", suffix = .euro, trim = TRUE,
                 largest_with_cents = 1e+05, negative_parens = FALSE, ...) {
  scales::label_dollar(
    accuracy = accuracy, scale = scale,
    big.mark = big.mark, decimal.mark = decimal.mark,
    prefix = prefix, suffix = suffix, trim = trim,
    largest_with_cents = largest_with_cents, negative_parens = negative_parens, ...
  )(x)
}

#' @rdname label_euro
#' @export
label_euro_de <- function(accuracy = 1, scale = 1, suffix = .euro, trim = TRUE,
                          largest_with_cents = 1e+05, negative_parens = FALSE, ...) {
  scales::label_dollar(
    accuracy = accuracy, scale = scale,
    big.mark = ".", decimal.mark = ",",
    prefix = "", suffix = suffix, trim = trim,
    largest_with_cents = largest_with_cents, negative_parens = negative_parens,
    ...
  )
}

#' @rdname label_euro
#' @export
euro_de <- function(x, accuracy = 1, scale = 1, suffix = .euro, trim = TRUE,
                    largest_with_cents = 1e+05, negative_parens = FALSE, ...) {
  scales::label_dollar(
    accuracy = accuracy, scale = scale,
    big.mark = ".", decimal.mark = ",",
    prefix = "", suffix = suffix, trim = trim,
    largest_with_cents = largest_with_cents, negative_parens = negative_parens,
    ...
  )(x)
}

#' @rdname label_euro
#' @export
label_dollar_de <- function(accuracy = 1, scale = 1, prefix = .dollar, trim = TRUE,
                            largest_with_cents = 1e+05, negative_parens = FALSE, ...) {
  scales::label_dollar(
    accuracy = accuracy, scale = scale,
    big.mark = ".", decimal.mark = ",",
    prefix = prefix, suffix = "", trim = trim,
    largest_with_cents = largest_with_cents, negative_parens = negative_parens, ...
  )
}

#' @rdname label_euro
#' @export
dollar_de <- function(x, accuracy = 1, scale = 1, prefix = .dollar, trim = TRUE,
                      largest_with_cents = 1e+05, negative_parens = FALSE, ...) {
  scales::label_dollar(
    accuracy = accuracy, scale = scale,
    big.mark = ".", decimal.mark = ",",
    prefix = prefix, suffix = "", trim = trim,
    largest_with_cents = largest_with_cents, negative_parens = negative_parens, ...
  )(x)
}

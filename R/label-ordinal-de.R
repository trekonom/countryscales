#' Label ordinal numbers (1., 2., 3., etc)
#'
#' Round values to integers and display as ordinal values.
#'
#' @details German wrapper around scales::label_ordinal which by default uses
#'     German rules to convert numerics to ordnial and with default \code{big.mark = "."}.
#'
#' @inheritParams scales::label_ordinal
#' @param digit in German ordinal numbers are displayed with a suffix "."
#' @param gender maskulin or feminin or neutral gender for German ordinal.
#'
#' @export
#'
#' @examples
#' require(scales)
#'
#' demo_continuous(c(1, 5))
#' demo_continuous(c(1, 5), labels = label_ordinal_de())
#'
#' # Note that ordinal rounds values, so you may need to adjust the breaks too
#' demo_continuous(c(1, 10))
#' demo_continuous(c(1, 10), labels = label_ordinal_de())
#' demo_continuous(c(1, 10),
#'   labels = label_ordinal_de(),
#'   breaks = scales::breaks_width(2)
#' )
label_ordinal_de <- function(prefix = "",
                             suffix = "",
                             big.mark = ".",
                             rules = ordinal_german(),
                             ...) {
  scales::label_ordinal(
    prefix = "",
    suffix = "",
    big.mark = ".",
    rules = ordinal_german(),
    ...
  )
}

#' @rdname label_ordinal_de
#' @export
ordinal_german <- function(gender = c("maskulin", "feminin", "neutral"), digit = TRUE) {
  if (!missing(gender)) digit <- FALSE
  suffix <- if (!digit) {
    switch(match.arg(gender),
      maskulin = "ter",
      feminin = "te",
      neutral = "tes"
    )
  } else {
    "."
  }
  label <- list(".")
  names(label) <- suffix
  label
}

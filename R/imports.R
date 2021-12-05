# Helper functions. Copy and paste from ggplot2
is.waive <- function(x) {
  inherits(x, "waiver")
}
is.sec_axis <- function(x) {
  inherits(x, "AxisSecondary")
}
is.formula <- function(x) {
  inherits(x, "formula")
}
set_sec_axis <- function(sec.axis, scale) {
  if (!is.waive(sec.axis)) {
    if (is.formula(sec.axis)) sec.axis <- sec_axis(sec.axis)
    if (!is.sec_axis(sec.axis)) stop("Secondary axes must be specified using 'sec_axis()'")
    scale$secondary.axis <- sec.axis
  }
  return(scale)
}

# is.null helper
"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

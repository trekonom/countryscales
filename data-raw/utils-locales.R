PATTERN_SEPARATOR <- ";"
QUOTE <- "'"
PATTERN_DIGIT <- "#"
PATTERN_ZERO_DIGIT <- "0"
PATTERN_GROUPING_SEPARATOR <- ","
PATTERN_DECIMAL_SEPARATOR <- "."
PATTERN_CURRENCY_SIGN <- "\u00A4"
PATTERN_PER_MILLE <- "\u2030"
PER_MILLE_SCALE <- 1000
PATTERN_PERCENT <- "%"
PERCENT_SCALE <- 100
PATTERN_EXPONENT <- "E"
PATTERN_PLUS <- "+"

sep_by <- function(x) {
  sep_by <- x
  sep_by[] <- "0"

  sep_by[
    grepl("\u00a4\u00a0(#|0)", x) |
      grepl("\u00a4(\\+|\\-)\u00a0(#|0)", x) |
      grepl("(#|0)\u00a0\u00a4", x)
  ] <- "1"

  sep_by[
    grepl("\u00a4\u00a0(\\+|\\-)(#|0)", x) |
      grepl("(#|0)(\\+|\\-)\u00a0\u00a4", x)
  ] <- "2"

  sep_by
}

percent_sep_by <- function(x) {
  sep_by <- x
  sep_by[] <- "0"

  sep_by[
    grepl("%\u00a0(#|0)", x) | grepl("(#|0)\u00a0%", x)
  ] <- "1"

  sep_by
}

percent_precedes <- function(x) {
  percent_precedes <- logical(length(x))

  percent_precedes[grepl("^%\u00a0?(#|0)", x)] <- TRUE

  percent_precedes
}

cs_precedes <- function(x) {
  cs_precedes <- x
  cs_precedes[] <- TRUE

  cs_precedes[grepl("(#|0).*?\u00A4", x)] <- FALSE

  cs_precedes
}

sign_posn <- function(x) {
  sign_posn <- x
  sign_posn[] <- 1

  sign_posn[
    grepl("\u00a4.+?(\\+|\\-)", x) &
      grepl("(0|#).*?(\\+|\\-)", x)
  ] <- 2

  sign_posn[grepl("(\\+|\\-)\u00a4", x)] <- 3
  sign_posn[grepl("\u00a4\u00a0?(\\+|\\-)", x)] <- 4

  sign_posn
}

symbol_sign <- function(x, pattern, default) {
  symbol_sign <- x
  symbol_sign[] <- ""

  symbol_sign[grepl(pattern, x)] <- default

  symbol_sign
}

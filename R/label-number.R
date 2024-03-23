#  https://github.com/r-lib/scales/blob/main/R/label-number.R
#  https://github.com/r-lib/scales/blob/main/LICENSE.md

#' Label numbers in decimal format (e.g. 0.12, 1,234)
#'
#' @inheritParams scales::label_number
#'
#' @noRd
label_number <- function(accuracy = NULL, scale = 1, prefix = "",
                         suffix = "", big.mark = " ", decimal.mark = ".",
                         style_positive = c("none", "plus"),
                         style_negative = c("hyphen", "minus", "parens"),
                         scale_cut = NULL,
                         trim = TRUE, ...) {
  # force_all(
  #   accuracy,
  #   scale,
  #   prefix,
  #   suffix,
  #   big.mark,
  #   decimal.mark,
  #   style_positive,
  #   style_negative,
  #   scale_cut,
  #   trim,
  #   ...
  # )
  function(x) {
    number(
      x,
      accuracy = accuracy,
      scale = scale,
      prefix = prefix,
      suffix = suffix,
      big.mark = big.mark,
      decimal.mark = decimal.mark,
      style_positive = style_positive,
      style_negative = style_negative,
      scale_cut = scale_cut,
      trim = trim,
      ...
    )
  }
}

number <- function(x, accuracy = NULL, scale = 1, prefix = "",
                   suffix = "", big.mark = " ", decimal.mark = ".",
                   style_positive = c("none", "plus"),
                   style_negative = c("hyphen", "minus", "parens"),
                   scale_cut = NULL,
                   trim = TRUE, ...) {
  if (length(x) == 0) {
    return(character())
  }

  style_positive <- arg_match(style_positive)
  if (!nchar(style_negative[1]) <= 2) {
    style_negative <- arg_match(style_negative)
  }

  if (!is.null(scale_cut)) {
    cut <- scale_cut(x,
      breaks = scale_cut,
      scale = scale,
      accuracy = accuracy,
      suffix = suffix
    )

    scale <- cut$scale
    suffix <- cut$suffix
    accuracy <- cut$accuracy
  }

  accuracy <- accuracy %||% precision(x * scale)
  x <- round_any(x, accuracy / scale)
  nsmalls <- -floor(log10(accuracy))
  nsmalls <- pmin(pmax(nsmalls, 0), 20)

  sign <- sign(x)
  sign[is.na(sign)] <- 0
  x <- abs(x)
  x_scaled <- scale * x

  ret <- character(length(x))
  for (nsmall in unique(nsmalls)) {
    idx <- nsmall == nsmalls

    ret[idx] <- format(
      x_scaled[idx],
      big.mark = big.mark,
      decimal.mark = decimal.mark,
      trim = trim,
      nsmall = nsmall,
      scientific = FALSE,
      ...
    )
  }

  ret <- paste0(prefix, ret, suffix)
  ret[is.infinite(x)] <- as.character(x[is.infinite(x)])

  if (style_negative == "hyphen") {
    ret[sign < 0] <- paste0("-", ret[sign < 0])
  } else if (style_negative == "minus") {
    ret[sign < 0] <- paste0("\u2212", ret[sign < 0])
  } else if (style_negative == "parens") {
    ret[sign < 0] <- paste0("(", ret[sign < 0], ")")
  } else {
    ret[sign < 0] <- paste0(style_negative, ret[sign < 0])
  }

  if (style_positive == "plus") {
    ret[sign > 0] <- paste0("+", ret[sign > 0])
  }

  # restore NAs from input vector
  ret[is.na(x)] <- NA
  names(ret) <- names(x)

  ret
}

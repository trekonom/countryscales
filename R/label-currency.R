label_currency <- function(accuracy = NULL, scale = 1, prefix = "",
                           suffix = "", big.mark = " ", decimal.mark = ".",
                           p_sign = NULL,
                           n_sign = NULL,
                           p_sep_by = c("0", "1", "2"),
                           n_sep_by = c("0", "1", "2"),
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
    currency(
      x,
      accuracy = accuracy,
      scale = scale,
      prefix = prefix,
      suffix = suffix,
      big.mark = big.mark,
      decimal.mark = decimal.mark,
      p_sign = p_sign,
      n_sign = n_sign,
      p_sep_by = p_sep_by,
      n_sep_by = n_sep_by,
      scale_cut = scale_cut,
      trim = trim,
      ...
    )
  }
}

#' @importFrom rlang arg_match
#' @importFrom plyr round_any
currency <- function(x, accuracy = NULL, scale = 1,
                     currency = NULL,
                     big.mark = " ", decimal.mark = ".",
                     p_cs_precedes = TRUE,
                     n_cs_precedes = p_cs_precedes,
                     p_sign = "",
                     n_sign = "-",
                     p_sep_by = c("0", "1", "2"),
                     n_sep_by = c("0", "1", "2"),
                     p_sign_posn = "1",
                     n_sign_posn = "1",
                     scale_cut = NULL, trim = TRUE, ...) {
  if (length(x) == 0) {
    return(character())
  }
  p_sep_by <- arg_match(p_sep_by)
  n_sep_by <- arg_match(n_sep_by)
  if (!is.null(scale_cut)) {
    cut <- scale_cut(x,
      breaks = scale_cut, scale = scale,
      accuracy = accuracy, suffix = suffix
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
    ret[idx] <- format(x_scaled[idx],
      big.mark = big.mark,
      decimal.mark = decimal.mark, trim = trim, nsmall = nsmall,
      scientific = FALSE, ...
    )
  }
  ret[is.infinite(x)] <- as.character(x[is.infinite(x)])

  prefix <- suffix <- x
  prefix[] <- suffix[] <- ""

  # CS symbol
  if (p_cs_precedes) {
    prefix[!sign < 0] <- currency
  } else {
    suffix[!sign < 0] <- currency
  }

  if (n_cs_precedes) {
    prefix[sign < 0] <- currency
  } else {
    suffix[sign < 0] <- currency
  }

  if (p_cs_precedes) {
    if (p_sign_posn == 3) {
      prefix[!sign < 0] <- paste0(p_sign, prefix[!sign < 0])
    }
    if (p_sign_posn == 4) {
      prefix[!sign < 0] <- paste0(prefix[!sign < 0], p_sign)
    }
  } else {
    if (p_sign_posn == 3) {
      suffix[!sign < 0] <- paste0(p_sign, suffix[!sign < 0])
    }
    if (p_sign_posn == 4) {
      suffix[!sign < 0] <- paste0(suffix[!sign < 0], p_sign)
    }
  }

  if (n_cs_precedes) {
    if (n_sign_posn == 3) {
      prefix[sign < 0] <- paste0(n_sign, prefix[sign < 0])
    }
    if (n_sign_posn == 4) {
      prefix[sign < 0] <- paste0(prefix[sign < 0], n_sign)
    }
  } else {
    if (n_sign_posn == 3) {
      suffix[sign < 0] <- paste0(n_sign, suffix[sign < 0])
    }
    if (n_sign_posn == 4) {
      suffix[sign < 0] <- paste0(suffix[sign < 0], n_sign)
    }
  }

  if (p_cs_precedes) {
    if (p_sep_by == "1") {
      prefix[!sign < 0] <- paste0(prefix[!sign < 0], " ")
    }
  } else {
    if (p_sep_by == "1") {
      suffix[!sign < 0] <- paste0(" ", suffix[!sign < 0])
    }
  }

  if (n_cs_precedes) {
    if (n_sep_by == "1") {
      prefix[sign < 0] <- paste0(prefix[sign < 0], " ")
    }
  } else {
    if (n_sep_by == "1") {
      suffix[sign < 0] <- paste0(" ", suffix[sign < 0])
    }
  }

  if (p_sign_posn == 1) {
    prefix[!sign < 0] <- paste0(p_sign, prefix[!sign < 0])
  }
  if (p_sign_posn == 2) {
    suffix[!sign < 0] <- paste0(suffix[!sign < 0], p_sign)
  }

  if (n_sign_posn == 1) {
    prefix[sign < 0] <- paste0(n_sign, prefix[sign < 0])
  }
  if (n_sign_posn == 2) {
    suffix[sign < 0] <- paste0(suffix[sign < 0], n_sign)
  }

  ret <- paste0(prefix, ret, suffix)
  ret[sign > 0] <- paste0(p_sign, ret[sign > 0])

  ret[is.na(x)] <- NA
  names(ret) <- names(x)
  ret
}

# Helpers -----------------------------------------------------------------

precision <- function(x) {
  x <- unique(x)
  # ignore NA and Inf/-Inf
  x <- x[is.finite(x)]

  if (length(x) <= 1) {
    return(1)
  }

  smallest_diff <- min(diff(sort(x)))
  if (smallest_diff < sqrt(.Machine$double.eps)) {
    1
  } else {
    precision <- 10^(floor(log10(smallest_diff)) - 1)

    # reduce precision when final digit always 0
    if (all(round(x / precision) %% 10 == 0)) {
      precision <- precision * 10
    }

    # Never return precision bigger than 1
    pmin(precision, 1)
  }
}

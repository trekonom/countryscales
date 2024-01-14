label_currency <- function(currency = NULL,
                           accuracy = NULL,
                           scale = 1,
                           prefix = "",
                           suffix = "",
                           big.mark = " ",
                           decimal.mark = ".",
                           p_sign = NULL,
                           n_sign = NULL,
                           p_sep_by = c("0", "1", "2"),
                           n_sep_by = c("0", "1", "2"),
                           scale_cut = NULL,
                           trim = TRUE, ...) {
  function(x) {
    currency(
      x,
      accuracy = accuracy,
      scale = scale,
      currency = currency,
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
currency <- function(x,
                     currency = NULL,
                     accuracy = NULL,
                     scale = 1,
                     big.mark = " ",
                     decimal.mark = ".",
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

  ### COPY AND PASTE from scales::label_number
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
  ###

  prefix <- suffix <- x
  prefix[] <- suffix[] <- ""

  sign_neg <- sign < 0
  sign_pos <- !sign_neg

  # Sign
  p_sep <- if (p_sep_by == "2") "\u00a0" else ""
  n_sep <- if (n_sep_by == "2") "\u00a0" else ""

  if (p_cs_precedes) {
    prefix[sign_pos] <- currency
    if (p_sign_posn == 3) {
      prefix[sign_pos] <- paste0(p_sign, p_sep, prefix[sign_pos])
    }
    if (p_sign_posn == 4) {
      prefix[sign_pos] <- paste0(prefix[sign_pos], p_sep, p_sign)
    }
    if (p_sep_by == "1") {
      prefix[sign_pos] <- paste0(prefix[sign_pos], "\u00a0")
    }
  } else {
    suffix[sign_pos] <- currency
    if (p_sign_posn == 3) {
      suffix[sign_pos] <- paste0(p_sign, p_sep, suffix[sign_pos])
    }
    if (p_sign_posn == 4) {
      suffix[sign_pos] <- paste0(suffix[sign_pos], p_sep, p_sign)
    }
    if (p_sep_by == "1") {
      suffix[sign_pos] <- paste0("\u00a0", suffix[sign_pos])
    }
  }

  if (n_cs_precedes) {
    prefix[sign_neg] <- currency
    if (n_sign_posn == 3) {
      prefix[sign_neg] <- paste0(n_sign, n_sep, prefix[sign_neg])
    }
    if (n_sign_posn == 4) {
      prefix[sign_neg] <- paste0(prefix[sign_neg], n_sep, n_sign)
    }
    if (n_sep_by == "1") {
      prefix[sign_neg] <- paste0(prefix[sign_neg], "\u00a0")
    }
  } else {
    suffix[sign_neg] <- currency
    if (n_sign_posn == 3) {
      suffix[sign_neg] <- paste0(n_sign, n_sep, suffix[sign_neg])
    }
    if (n_sign_posn == 4) {
      suffix[sign_neg] <- paste0(suffix[sign_neg], n_sep, n_sign)
    }
    if (n_sep_by == "1") {
      suffix[sign_neg] <- paste0("\u00a0", suffix[sign_neg])
    }
  }

  if (p_sign_posn == 1) {
    prefix[sign_pos] <- paste0(p_sign, prefix[sign_pos])
  }
  if (p_sign_posn == 2) {
    suffix[sign_pos] <- paste0(suffix[sign_pos], p_sign)
  }

  if (n_sign_posn == 1) {
    prefix[sign_neg] <- paste0(n_sign, prefix[sign_neg])
  }
  if (n_sign_posn == 2) {
    suffix[sign_neg] <- paste0(suffix[sign_neg], n_sign)
  }

  ret <- paste0(prefix, ret, suffix)
  ret[sign > 0] <- paste0(p_sign, ret[sign > 0])

  ret[is.na(x)] <- NA
  names(ret) <- names(x)
  ret
}

# Helpers -----------------------------------------------------------------

### COPY AND PASTE from scales pkg
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

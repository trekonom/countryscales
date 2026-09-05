#  https://github.com/r-lib/scales/blob/main/R/label-number.R
#  https://github.com/r-lib/scales/blob/main/LICENSE.md

#' Label currencies ($100, €2.50, etc)
#'
#' @inheritParams scales::label_number
#' @param p_sign positive sign
#' @param n_sign negative sign
#' @param p_sep_by positive sign separator
#' @param n_sep_by negative sign separator
#'
#' @noRd
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
  # currency() has no free-text `suffix` argument of its own (its prefix/
  # suffix are reserved for the currency symbol and sign) -- the scale_cut
  # suffix (e.g. "K"/"M") gets appended to the formatted number below,
  # before the currency-symbol wrapping happens.
  scale_cut_suffix <- ""
  if (!is.null(scale_cut)) {
    cut <- apply_scale_cut(x,
      breaks = scale_cut, scale = scale,
      accuracy = accuracy, suffix = ""
    )
    scale <- cut$scale
    scale_cut_suffix <- cut$suffix
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
  ret <- paste0(ret, scale_cut_suffix)
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

  ret[is.na(x)] <- NA
  names(ret) <- names(x)
  ret
}

# Helpers -----------------------------------------------------------------

### COPY AND PASTE from scales pkg (renamed from `scale_cut` to avoid
### colliding with the `scale_cut` argument of number()/currency(), which
### holds the `breaks` vector this function is called with)
apply_scale_cut <- function(x, breaks, scale = 1, accuracy = NULL, suffix = "") {
  breaks <- sort(breaks, na.last = TRUE)

  break_suffix <- as.character(cut(abs(x * scale),
    breaks = c(unname(breaks), Inf), labels = c(names(breaks)), right = FALSE
  ))
  break_suffix[is.na(break_suffix)] <- ""

  bad_break <- ((x * scale / breaks[break_suffix]) %% 1 != 0) %|% FALSE
  if (any(bad_break)) {
    lower_break <- breaks[match(break_suffix[bad_break], names(breaks)) - 1]
    lower_break[lower_break == 0] <- 1
    improved_break <- (x[bad_break] * scale / lower_break) %% 1 == 0
    power10_break <- breaks[break_suffix[bad_break]] / lower_break
    power10_break <- log10(power10_break) %% 1 == 0
    break_suffix[bad_break][improved_break & !power10_break] <-
      names(lower_break[improved_break & !power10_break])
  }

  break_scale <- scale * unname(1 / breaks[break_suffix])
  break_scale[which(break_scale %in% c(Inf, NA))] <- scale
  break_scale[abs(x) == 0 | is.na(break_scale)] <- 1

  suffix <- paste0(break_suffix, suffix)
  accuracy <- accuracy %||% stats::ave(x * break_scale, break_scale, FUN = precision)

  list(scale = break_scale, suffix = suffix, accuracy = accuracy)
}

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

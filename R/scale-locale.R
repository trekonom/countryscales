#' Formatted Positional Scales
#'
#' The scale_x/y_xxx_xxx family of functions makes it easy to format
#'    axis text in decimal format, as percentages or as currencies.
#'
#' * the `number` ones format axis text in decimal format.
#' * the `percent` ones format axis text as percentages.
#' * the `currency` ones format axis text as currencies.
#'
#' @inheritParams ggplot2::scale_x_continuous
#' @inheritParams scales::label_number
#' @param currency currency symbol
#' @param locale locale string. Defaults to "en-US"
#'
#' @name scale-locale
#'
#' @example inst/ex/ex-scale-locale.R
NULL

#' @rdname scale-locale
#' @export
scale_x_number_locale <- function(name = waiver(),
                                  breaks = waiver(),
                                  minor_breaks = waiver(),
                                  guide = waiver(),
                                  n.breaks = NULL,
                                  labels = waiver(),
                                  limits = NULL,
                                  expand = waiver(),
                                  oob = censor,
                                  na.value = NA_real_,
                                  trans = "identity",
                                  position = "bottom",
                                  sec.axis = waiver(),
                                  accuracy = 1, scale = 1,
                                  prefix = "", suffix = "",
                                  big.mark = NULL,
                                  decimal.mark = NULL,
                                  locale = "en-US",
                                  trim = TRUE, ...) {
  if (is.waive(labels)) {
    labels <- label_number_locale(
      accuracy = accuracy,
      scale = scale,
      prefix = prefix,
      suffix = suffix,
      big.mark = big.mark,
      decimal.mark = decimal.mark,
      trim = trim,
      locale = locale,
      ...
    )
  }

  sc <- number_scale(
    aesthetics = countryscales_global$x_aes,
    scale_name = "position_c",
    palette = identity,
    name = name,
    breaks = breaks,
    n.breaks = n.breaks,
    minor_breaks = minor_breaks,
    labels = labels,
    limits = limits,
    expand = expand,
    oob = oob,
    na.value = na.value,
    trans = trans,
    guide = guide,
    position = position,
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    super = ScaleContinuousPosition
  )

  set_sec_axis(sec.axis, sc)
}

#' @rdname scale-locale
#' @export
scale_y_number_locale <- function(name = waiver(),
                                  breaks = waiver(),
                                  minor_breaks = waiver(),
                                  guide = waiver(),
                                  n.breaks = NULL,
                                  labels = waiver(),
                                  limits = NULL,
                                  expand = waiver(),
                                  oob = censor,
                                  na.value = NA_real_,
                                  trans = "identity",
                                  position = "left",
                                  sec.axis = waiver(),
                                  accuracy = 1, scale = 1,
                                  prefix = "", suffix = "",
                                  big.mark = NULL,
                                  decimal.mark = NULL,
                                  locale = "en-US",
                                  trim = TRUE, ...) {
  if (is.waive(labels)) {
    labels <- label_number_locale(
      accuracy = accuracy,
      scale = scale,
      prefix = prefix,
      suffix = suffix,
      big.mark = big.mark,
      decimal.mark = decimal.mark,
      trim = trim,
      locale = locale,
      ...
    )
  }

  sc <- number_scale(
    aesthetics = countryscales_global$y_aes,
    scale_name = "position_c",
    palette = identity,
    name = name,
    breaks = breaks,
    n.breaks = n.breaks,
    minor_breaks = minor_breaks,
    labels = labels,
    limits = limits,
    expand = expand,
    oob = oob,
    na.value = na.value,
    trans = trans,
    guide = guide,
    position = position,
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    super = ScaleContinuousPosition
  )

  set_sec_axis(sec.axis, sc)
}

#' @rdname scale-locale
#' @export
scale_x_percent_locale <- function(name = waiver(),
                                   breaks = waiver(),
                                   minor_breaks = waiver(),
                                   guide = waiver(),
                                   n.breaks = NULL,
                                   labels = waiver(),
                                   limits = NULL,
                                   expand = waiver(),
                                   oob = censor,
                                   na.value = NA_real_,
                                   trans = "identity",
                                   position = "bottom",
                                   sec.axis = waiver(),
                                   accuracy = 1, scale = 100,
                                   prefix = NULL, suffix = NULL,
                                   big.mark = NULL,
                                   decimal.mark = NULL,
                                   locale = "en-US",
                                   trim = TRUE, ...) {
  if (is.waive(labels)) {
    labels <- label_percent_locale(
      accuracy = accuracy,
      scale = scale,
      prefix = prefix,
      suffix = suffix,
      big.mark = big.mark,
      decimal.mark = decimal.mark,
      trim = trim,
      locale = locale,
      ...
    )
  }

  sc <- number_scale(
    aesthetics = countryscales_global$x_aes,
    scale_name = "position_c",
    palette = identity,
    name = name,
    breaks = breaks,
    n.breaks = n.breaks,
    minor_breaks = minor_breaks,
    labels = labels,
    limits = limits,
    expand = expand,
    oob = oob,
    na.value = na.value,
    trans = trans,
    guide = guide,
    position = position,
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    super = ScaleContinuousPosition
  )

  set_sec_axis(sec.axis, sc)
}


#' @rdname scale-locale
#' @export
scale_y_percent_locale <- function(name = waiver(),
                                   breaks = waiver(),
                                   minor_breaks = waiver(),
                                   guide = waiver(),
                                   n.breaks = NULL,
                                   labels = waiver(),
                                   limits = NULL,
                                   expand = waiver(),
                                   oob = censor,
                                   na.value = NA_real_,
                                   trans = "identity",
                                   position = "left",
                                   sec.axis = waiver(),
                                   accuracy = 1, scale = 100,
                                   prefix = NULL, suffix = NULL,
                                   big.mark = NULL,
                                   decimal.mark = NULL,
                                   locale = "en-US",
                                   trim = TRUE, ...) {
  if (is.waive(labels)) {
    labels <- label_percent_locale(
      accuracy = accuracy,
      scale = scale,
      prefix = prefix,
      suffix = suffix,
      big.mark = big.mark,
      decimal.mark = decimal.mark,
      trim = trim,
      locale = locale,
      ...
    )
  }

  sc <- number_scale(
    aesthetics = countryscales_global$y_aes,
    scale_name = "position_c",
    palette = identity,
    name = name,
    breaks = breaks,
    n.breaks = n.breaks,
    minor_breaks = minor_breaks,
    labels = labels,
    limits = limits,
    expand = expand,
    oob = oob,
    na.value = na.value,
    trans = trans,
    guide = guide,
    position = position,
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    super = ScaleContinuousPosition
  )

  set_sec_axis(sec.axis, sc)
}

#' @rdname scale-locale
#' @export
scale_x_currency_locale <- function(name = waiver(),
                                    breaks = waiver(),
                                    minor_breaks = waiver(),
                                    guide = waiver(),
                                    n.breaks = NULL,
                                    labels = waiver(),
                                    limits = NULL,
                                    expand = waiver(),
                                    oob = censor,
                                    na.value = NA_real_,
                                    trans = "identity",
                                    position = "bottom",
                                    sec.axis = waiver(),
                                    accuracy = 1,
                                    scale = 1,
                                    prefix = NULL,
                                    suffix = NULL,
                                    big.mark = NULL,
                                    decimal.mark = NULL,
                                    locale = "en-US",
                                    currency = "USD",
                                    trim = TRUE, ...) {
  if (is.waive(labels)) {
    labels <- label_currency_locale(
      accuracy = accuracy,
      scale = scale,
      prefix = prefix,
      suffix = suffix,
      big.mark = big.mark,
      decimal.mark = decimal.mark,
      trim = trim,
      locale = locale,
      currency = currency,
      ...
    )
  }

  sc <- number_scale(
    aesthetics = countryscales_global$x_aes,
    scale_name = "position_c",
    palette = identity,
    name = name,
    breaks = breaks,
    n.breaks = n.breaks,
    minor_breaks = minor_breaks,
    labels = labels,
    limits = limits,
    expand = expand,
    oob = oob,
    na.value = na.value,
    trans = trans,
    guide = guide,
    position = position,
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    super = ScaleContinuousPosition
  )

  set_sec_axis(sec.axis, sc)
}

#' @rdname scale-locale
#' @export
scale_y_currency_locale <- function(name = waiver(),
                                    breaks = waiver(),
                                    minor_breaks = waiver(),
                                    guide = waiver(),
                                    n.breaks = NULL,
                                    labels = waiver(),
                                    limits = NULL,
                                    expand = waiver(),
                                    oob = censor,
                                    na.value = NA_real_,
                                    trans = "identity",
                                    position = "left",
                                    sec.axis = waiver(),
                                    accuracy = 1,
                                    scale = 1,
                                    prefix = NULL,
                                    suffix = NULL,
                                    big.mark = NULL,
                                    decimal.mark = NULL,
                                    locale = "en-US",
                                    currency = "USD",
                                    trim = TRUE, ...) {
  if (is.waive(labels)) {
    labels <- label_currency_locale(
      accuracy = accuracy,
      scale = scale,
      prefix = prefix,
      suffix = suffix,
      big.mark = big.mark,
      decimal.mark = decimal.mark,
      trim = trim,
      locale = locale,
      currency = currency,
      ...
    )
  }

  sc <- number_scale(
    aesthetics = countryscales_global$y_aes,
    scale_name = "position_c",
    palette = identity,
    name = name,
    breaks = breaks,
    n.breaks = n.breaks,
    minor_breaks = minor_breaks,
    labels = labels,
    limits = limits,
    expand = expand,
    oob = oob,
    na.value = na.value,
    trans = trans,
    guide = guide,
    position = position,
    accuracy = accuracy, scale = scale,
    prefix = prefix, suffix = suffix,
    big.mark = big.mark, decimal.mark = decimal.mark,
    super = ScaleContinuousPosition
  )

  sc <- set_sec_axis(sec.axis, sc)

  sc
}

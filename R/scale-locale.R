#' Formatted Positional Scales
#'
#' The scale_xxx_xxx family of functions makes it easy to format axis text
#'     in decimal format, as percentages or as currencies.
#'
#' * the `_number` ones format axis text as numbers.
#' * the `_percent` ones format axis text as percentages.
#' * the `_dollar` ones format axis text as dollars.
#' * the `_euro` ones format axis text as euros.
#'
#' @inheritParams ggplot2::scale_x_continuous
#' @inheritParams scales::label_number
#' @param prefix Symbol to display before value.
#' @param suffix Symbol to display after value.
#' @param locale locale string. Defaults to "en_US"
#'
#' @name scale-format
NULL

#' @rdname scale-format
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

  locale <- check_locale(locale)
  big.mark <- check_big(big.mark, locale)
  decimal.mark <- check_decimal(decimal.mark, locale)

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

#' @rdname scale-format
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

  locale <- check_locale(locale)
  big.mark <- check_big(big.mark, locale)
  decimal.mark <- check_decimal(decimal.mark, locale)

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

#' @rdname scale-format
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
                           prefix = "", suffix = NULL,
                           big.mark = NULL,
                           decimal.mark = NULL,
                           locale = "en-US",
                           trim = TRUE, ...) {

  locale <- check_locale(locale)
  big.mark <- check_big(big.mark, locale)
  decimal.mark <- check_decimal(decimal.mark, locale)
  suffix <- check_suffix(suffix, locale, suffix = "%")

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


#' @rdname scale-format
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
                            prefix = "", suffix = NULL,
                            big.mark = NULL,
                            decimal.mark = NULL,
                            locale = "en-US",
                            trim = TRUE, ...) {

  locale <- check_locale(locale)
  big.mark <- check_big(big.mark, locale)
  decimal.mark <- check_decimal(decimal.mark, locale)
  suffix <- check_suffix(suffix, locale, suffix = "%")

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


#' @rdname scale-format
#' @export
scale_x_comma_locale <- function(name = waiver(),
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
                          big.mark = ",", decimal.mark = ".",
                          trim = TRUE, ...) {
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

#' @rdname scale-format
#' @export
scale_y_comma_locale <- function(name = waiver(),
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
                          big.mark = ",", decimal.mark = ".",
                          trim = TRUE, ...) {
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


#' @rdname scale-format
#' @export
scale_x_dollar_locale <- function(name = waiver(),
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
                           prefix = "$", suffix = "",
                           big.mark = " ", decimal.mark = ".",
                           trim = TRUE, ...) {
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

#' @rdname scale-format
#' @export
scale_y_dollar_locale <- function(name = waiver(),
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
                           prefix = "$", suffix = "",
                           big.mark = " ", decimal.mark = ".",
                           trim = TRUE, ...) {
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

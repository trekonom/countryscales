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
#'
#' @name scale-format
#'
#' @example inst/ex/ex-scale-de.R
NULL

#' @rdname scale-format
#' @export
scale_x_number_de <- function(name = waiver(),
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
                              big.mark = NULL, decimal.mark = NULL,
                              trim = TRUE, ...) {
  scale_x_number(
    name = name,
    breaks = breaks,
    minor_breaks = minor_breaks,
    guide = guide,
    n.breaks = n.breaks,
    labels = labels,
    limits = limits,
    expand = expand,
    oob = oob,
    na.value = na.value,
    trans = trans,
    position = position,
    sec.axis = sec.axis,
    accuracy = accuracy,
    scale = scale,
    prefix = prefix,
    suffix = suffix,
    big.mark = big.mark,
    decimal.mark = decimal.mark,
    locale = "de-DE",
    trim = trim,
    ...
  )
}

#' @rdname scale-format
#' @export
scale_y_number_de <- function(name = waiver(),
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
                              big.mark = NULL, decimal.mark = NULL,
                              trim = TRUE, ...) {
  scale_y_number(
    name = name,
    breaks = breaks,
    minor_breaks = minor_breaks,
    guide = guide,
    n.breaks = n.breaks,
    labels = labels,
    limits = limits,
    expand = expand,
    oob = oob,
    na.value = na.value,
    trans = trans,
    position = position,
    sec.axis = sec.axis,
    accuracy = accuracy,
    scale = scale,
    prefix = prefix,
    suffix = suffix,
    big.mark = big.mark,
    decimal.mark = decimal.mark,
    locale = "de-DE",
    trim = trim,
    ...
  )
}

#' @rdname scale-format
#' @export
scale_x_percent_de <- function(name = waiver(),
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
                               big.mark = NULL, decimal.mark = NULL,
                               trim = TRUE, ...) {

  scale_x_percent(
    name = name,
    breaks = breaks,
    minor_breaks = minor_breaks,
    guide = guide,
    n.breaks = n.breaks,
    labels = labels,
    limits = limits,
    expand = expand,
    oob = oob,
    na.value = na.value,
    trans = trans,
    position = position,
    sec.axis = sec.axis,
    accuracy = accuracy,
    scale = scale,
    prefix = prefix,
    suffix = suffix,
    big.mark = big.mark,
    decimal.mark = decimal.mark,
    locale = "de-DE",
    trim = trim,
    ...
  )
}

#' @rdname scale-format
#' @export
scale_y_percent_de <- function(name = waiver(),
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
                               big.mark = NULL, decimal.mark = NULL,
                               trim = TRUE, ...) {

  scale_y_percent(
    name = name,
    breaks = breaks,
    minor_breaks = minor_breaks,
    guide = guide,
    n.breaks = n.breaks,
    labels = labels,
    limits = limits,
    expand = expand,
    oob = oob,
    na.value = na.value,
    trans = trans,
    position = position,
    sec.axis = sec.axis,
    accuracy = accuracy,
    scale = scale,
    prefix = prefix,
    suffix = suffix,
    big.mark = big.mark,
    decimal.mark = decimal.mark,
    locale = "de-DE",
    trim = trim,
    ...
  )
}

#' @rdname scale-format
#' @export
scale_x_dollar_de <- function(name = waiver(),
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
                              big.mark = ".", decimal.mark = ",",
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
scale_y_dollar_de <- function(name = waiver(),
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
                              big.mark = ".", decimal.mark = ",",
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
scale_x_euro_de <- function(name = waiver(),
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
                            prefix = "", suffix = .euro,
                            big.mark = ".", decimal.mark = ",",
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
scale_y_euro_de <- function(name = waiver(),
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
                            prefix = "", suffix = .euro,
                            big.mark = ".", decimal.mark = ",",
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

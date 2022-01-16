#' German Style Positional Scales
#'
#' The scale_xxx_xxx_de family of functions makes it easy to style (numeric) axes
#'     using standard number formatting used in Germany.
#'
#' * the `_number` ones format axis text as numbers.
#'
#' * the `_percent` ones format axis text as percentages.
#'
#' * the `_dollar` ones format axis text as dollars.
#'
#' * the `_euro` ones format axis text as euros.
#'
#' @inheritParams ggplot2::scale_x_continuous
#' @inheritParams scales::label_number
#' @param prefix Symbol to display before value.
#' @param suffix Symbol to display after value.
#'
#' @export
scale_x_number <- function(name = waiver(),
                           breaks = waiver(),
                           minor_breaks = waiver(),
                           guide = waiver(),
                           n.breaks = NULL,
                           labels,
                           limits = NULL,
                           expand = c(0.01, 0),
                           oob = censor,
                           na.value = NA_real_,
                           trans = "identity",
                           position = "bottom",
                           sec.axis = waiver(),
                           accuracy = 1, scale = 1,
                           prefix = "", suffix = "",
                           big.mark = " ", decimal.mark = ".",
                           trim = TRUE, ...) {
  labels <- if (missing(labels)) {
    label_number(
      accuracy = accuracy,
      scale = scale,
      prefix = prefix,
      suffix = suffix,
      big.mark = big.mark,
      decimal.mark = decimal.mark,
      trim = trim,
      ...
    )
  }

  sc <- continuous_scale(
    aesthetics = gscales_global$x_aes,
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
    super = ScaleContinuousPosition
  )

  set_sec_axis(sec.axis, sc)
}

#' @rdname scale_x_number
#' @export
scale_y_number <- function(name = waiver(),
                           breaks = waiver(),
                           minor_breaks = waiver(),
                           guide = waiver(),
                           n.breaks = NULL,
                           labels,
                           limits = NULL,
                           expand = c(0.01, 0),
                           oob = censor,
                           na.value = NA_real_,
                           trans = "identity",
                           position = "left",
                           sec.axis = waiver(),
                           accuracy = 1, scale = 1,
                           prefix = "", suffix = "",
                           big.mark = " ", decimal.mark = ".",
                           trim = TRUE, ...) {
  labels <- if (missing(labels)) {
    label_number(
      accuracy = accuracy,
      scale = scale,
      prefix = prefix,
      suffix = suffix,
      big.mark = big.mark,
      decimal.mark = decimal.mark,
      trim = trim,
      ...
    )
  }

  sc <- continuous_scale(
    aesthetics = gscales_global$y_aes,
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
    super = ScaleContinuousPosition
  )

  sc <- set_sec_axis(sec.axis, sc)

  sc
}

#' @rdname scale_x_number
#' @export
scale_x_number_de <- function(name = waiver(),
                              breaks = waiver(),
                              minor_breaks = waiver(),
                              guide = waiver(),
                              n.breaks = NULL,
                              labels,
                              limits = NULL,
                              expand = c(0.01, 0),
                              oob = censor,
                              na.value = NA_real_,
                              trans = "identity",
                              position = "bottom",
                              sec.axis = waiver(),
                              accuracy = 1, scale = 1,
                              prefix = "", suffix = "",
                              big.mark = ".", decimal.mark = ",",
                              trim = TRUE, ...) {
  labels <- if (missing(labels)) {
    label_number_de(
      accuracy = accuracy,
      scale = scale,
      prefix = prefix,
      suffix = suffix,
      big.mark = big.mark,
      decimal.mark = decimal.mark,
      trim = trim,
      ...
    )
  }

  sc <- continuous_scale(
    aesthetics = gscales_global$x_aes,
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
    super = ScaleContinuousPosition
  )

  set_sec_axis(sec.axis, sc)
}

#' @rdname scale_x_number
#' @export
scale_y_number_de <- function(name = waiver(),
                              breaks = waiver(),
                              minor_breaks = waiver(),
                              guide = waiver(),
                              n.breaks = NULL,
                              labels,
                              limits = NULL,
                              expand = c(0.01, 0),
                              oob = censor,
                              na.value = NA_real_,
                              trans = "identity",
                              position = "left",
                              sec.axis = waiver(),
                              accuracy = 1, scale = 1,
                              prefix = "", suffix = "",
                              big.mark = ".", decimal.mark = ",",
                              trim = TRUE, ...) {
  labels <- if (missing(labels)) {
    label_number_de(
      accuracy = accuracy,
      scale = scale,
      prefix = prefix,
      suffix = suffix,
      big.mark = big.mark,
      decimal.mark = decimal.mark,
      trim = trim,
      ...
    )
  }

  sc <- continuous_scale(
    aesthetics = gscales_global$y_aes,
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
    super = ScaleContinuousPosition
  )

  sc <- set_sec_axis(sec.axis, sc)

  sc
}

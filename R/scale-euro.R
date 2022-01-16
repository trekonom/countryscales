#' @rdname scale_x_number
#' @export
scale_x_euro_de <- function(name = waiver(),
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
                              prefix = "", suffix = "\u20ac",
                              big.mark = ".", decimal.mark = ",",
                              trim = TRUE, ...) {
  labels <- if (missing(labels)) {
    label_euro_de(
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
scale_y_euro_de <- function(name = waiver(),
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
                              prefix = "", suffix = "\u20ac",
                              big.mark = ".", decimal.mark = ",",
                              trim = TRUE, ...) {
  labels <- if (missing(labels)) {
    label_euro_de(
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

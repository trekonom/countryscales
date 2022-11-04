#' Formatted Scale Constructor
#'
#' The scale_xxx_xxx_de family of functions makes it easy to style (numeric) axes
#'     using standard number formatting used in Germany.
#'
#' @inheritParams ggplot2::continuous_scale
#' @inheritParams scales::label_number
#' @param prefix Symbol to display before value.
#' @param suffix Symbol to display after value.
#'
#' @export
number_scale <- function(aesthetics, scale_name, palette,
                         name = waiver(),
                         breaks = waiver(),
                         minor_breaks = waiver(),
                         guide = waiver(),
                         n.breaks = NULL,
                         labels = waiver(),
                         limits = NULL,
                         expand = c(0.01, 0),
                         oob = censor,
                         na.value = NA_real_,
                         trans = "identity",
                         position = "bottom",
                         super = ScaleContinuous,
                         accuracy = 1, scale = 1,
                         prefix = "", suffix = "",
                         big.mark = " ", decimal.mark = ".",
                         trim = TRUE, ...) {

  if (is.waive(labels)) {
    labels <- number_format(
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

  continuous_scale(
    aesthetics = aesthetics,
    scale_name = scale_name,
    palette = palette,
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
    super = super
  )
}

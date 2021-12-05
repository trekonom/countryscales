#' X & Y scales with opinionated pre-sets for percent & comma label formats
#'
#' The `_number` ones format axis text as numbers and `expand=c(0,0)` (you need to set limits).
#'
#' The `_percent` ones format axis text as percentages and `expand=c(0,0)` (you need to set limits).
#'
#' The `_dollar` ones format axis text as dollars and `expand=c(0,0)` (you need to set limits).
#'
#' The `_euro` ones format axis text as euros and `expand=c(0,0)` (you need to set limits).
#'
#' @param name The name of the scale. Used as axis or legend title. If
#'   `waiver()`, the default, the name of the scale is taken from the first
#'   mapping used for that aesthetic. If `NULL`, the legend title will be
#'   omitted.
#' @param breaks One of:
#'   - `NULL` for no breaks
#'   - `waiver()` for the default breaks computed by the
#'     transformation object
#'   - A numeric vector of positions
#'   - A function that takes the limits as input and returns breaks
#'     as output
#' @param minor_breaks One of:
#'   - `NULL` for no minor breaks
#'   - `waiver()` for the default breaks (one minor break between
#'     each major break)
#'   - A numeric vector of positions
#'   - A function that given the limits returns a vector of minor breaks.
#' @param guide	guide	A function used to create a guide or its name. See [guides()] for more information.
#' @param n.breaks	An integer guiding the number of major breaks. The algorithm may choose a
#'        slightly different number to ensure nice break labels. Will only have an effect if
#'        `breaks = waiver()`. Use NULL to use the default number of breaks given by the transformation.
#' @param labels Specifying overrides the default format (i.e. you really don't
#'   want to do that). `NULL` means no labels.
#' @param limits A numeric vector of length two providing limits of the scale.
#'   Use `NA` to refer to the existing minimum or maximum.
#' @param oob Function that handles limits outside of the scale limits
#'   (out of bounds). The default replaces out of bounds values with NA.
#' @param na.value If `na.translate = TRUE`, what value aesthetic
#'   value should missing be displayed as? Does not apply to position scales
#'   where `NA` is always placed at the far right.
#' @param expand same as in ggplot2
#' @param trans Either the name of a transformation object, or the
#'   object itself. Built-in transformations include "asn", "atanh",
#'   "boxcox", "exp", "identity", "log", "log10", "log1p", "log2",
#'   "logit", "probability", "probit", "reciprocal", "reverse" and "sqrt".
#' @param position The position of the axis. "left" or "right" for vertical
#' scales, "top" or "bottom" for horizontal scales
#' @param sec.axis specify a secondary axis
#' @inheritParams label_percent_de
#'
#' @export
scale_x_percent_de <- function(name = waiver(),
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
                               accuracy = 1, scale = 100,
                               prefix = "", suffix = "%",
                               big.mark = ".", decimal.mark = ",",
                               trim = TRUE, ...) {
  labels <- if (missing(labels)) {
    label_percent_de(
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

#' @rdname scale_x_percent_de
#' @export
scale_y_percent_de <- function(name = waiver(),
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
                               accuracy = 1, scale = 100,
                               prefix = "", suffix = "%",
                               big.mark = ".", decimal.mark = ",",
                               trim = TRUE, ...) {
  labels <- if (missing(labels)) {
    label_percent_de(
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

#' @rdname scale_x_percent_de
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

#' @rdname scale_x_percent_de
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

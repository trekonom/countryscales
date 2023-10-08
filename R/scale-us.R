#' US Style Positional Scales
#'
#' The scale_xxx_xxx family of functions makes it easy to format axis text
#'     in decimal format, as percentages or as currencies.
#'
#' * the `number` ones format axis text in decimal format.
#' * the `percent` ones format axis text as percentages.
#' * the `currency` ones format axis text as currencies.
#'
#' @inheritParams scale_x_number_locale
#'
#' @name scale-us
#'
#' @example inst/ex/ex-scale-us.R
NULL

#' @rdname scale-us
#' @export
scale_x_number_us <- function(name = waiver(),
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
  scale_x_number_locale(
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
    locale = "en-US",
    trim = trim,
    ...
  )
}

#' @rdname scale-us
#' @export
scale_y_number_us <- function(name = waiver(),
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
  scale_y_number_locale(
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
    locale = "en-US",
    trim = trim,
    ...
  )
}

#' @rdname scale-us
#' @export
scale_x_percent_us <- function(name = waiver(),
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

  scale_x_percent_locale(
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
    locale = "en-US",
    trim = trim,
    ...
  )
}

#' @rdname scale-us
#' @export
scale_y_percent_us <- function(name = waiver(),
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

  scale_y_percent_locale(
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
    locale = "en-US",
    trim = trim,
    ...
  )
}

#' @rdname scale-us
#' @export
scale_x_currency_us <- function(name = waiver(),
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
                                currency = "USD",
                                trim = TRUE, ...) {

  scale_x_currency_locale(
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
    currency = currency,
    locale = "en-US",
    trim = trim,
    ...
  )
}

#' @rdname scale-us
#' @export
scale_y_currency_us <- function(name = waiver(),
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
                                currency = "USD",
                                trim = TRUE, ...) {

  scale_y_currency_locale(
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
    currency = currency,
    locale = "en-US",
    trim = trim,
    ...
  )
}

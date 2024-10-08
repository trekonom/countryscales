#' German Style Positional Scales
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
#' @name scale-de
#'
#' @examples
#' \dontrun{
#' demo_number(c(-1e6, 1e6), scale_name = "number_de")
#' demo_number(c(-1, 1), scale_name = "percent_de")
#' demo_number(c(-1e4, 1e4), scale_name = "currency_de")
#' }
NULL

#' @rdname scale-de
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
    locale = "de-DE",
    trim = trim,
    ...
  )
}

#' @rdname scale-de
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
    locale = "de-DE",
    trim = trim,
    ...
  )
}

#' @rdname scale-de
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
    locale = "de-DE",
    trim = trim,
    ...
  )
}

#' @rdname scale-de
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
    locale = "de-DE",
    trim = trim,
    ...
  )
}

#' @rdname scale-de
#' @export
scale_x_currency_de <- function(name = waiver(),
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
                                currency = "EUR",
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
    locale = "de-DE",
    trim = trim,
    ...
  )
}

#' @rdname scale-de
#' @export
scale_y_currency_de <- function(name = waiver(),
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
                                currency = "EUR",
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
    locale = "de-DE",
    trim = trim,
    ...
  )
}

#' Labeling Functions Factory
#'
#' Creates source code to add new labeling functions
#'
#' @param iso2 a character. Country code to be appended to the
#' @param unit a charatcer. One of "number", "percent, "dollar" or "euro
#' @param country a charatcer. Country label to be used in the docs.
#' @param adjectival a charatcer. Country adjectival to be used in the docs.
#' @param params a list. Specification of the labeling function. Default values.
#'
#' @export
#'
#' @examples
#' params <- list(big.mark = ".", decimal.mark = ",", suffix = "%")
#' label_factory("de", "percent", "Germany", "German", params = params)
label_factory <- function(iso2, unit, country, adjectival, params = list()) {

  fun_name <- paste("label", unit, iso2, sep = "_")

  # Title
  fun_title <- sprintf("%s Style Number Formatting", adjectival) |>
    tag_app() |>
    ap_em_rox()

  # Description
  fun_desc1 <- c(
    sprintf("The label_xxx_%s family of functions makes it easy to format numbers", iso2),
    sprintf("    using standard number formatting used in %s.", country)
  ) |>
    purrr::map_chr(tag_app) |>
    purrr::reduce(c) |>
    ap_em_rox()

  fun_desc2 <- c(
    " * the `_number` ones format numbers in decimal format.",
    " * the `_percent` ones format numbers as percentages.",
    " * the `_dollar` ones format numbers as dollars.",
    " * the `_euro` ones format numbers as euros."
  ) |>
    purrr::map_chr(tag_app) |>
    purrr::map(ap_em_rox) |>
    purrr::reduce(c)

  # Parameters
  fun_params <- c(
    "@inheritParams ggplot2::scale_x_continuous",
    "@inheritParams scales::label_number",
    "@param prefix Symbol to display before value.",
    "@param suffix Symbol to display after value."
  ) |>
    purrr::map_chr(tag_app) |>
    purrr::reduce(c) |>
    ap_em_rox()

  fun_code <- c(fun_title, fun_desc1, fun_desc2, fun_params)

  fun_code <- c(
    fun_code,
    tag_app("@export")
  ) |>
    ap_em_rox()
  # Examples
  fun_code <- c(
    fun_code,
    tag_app("@examples")
  )
  fun_ex <- if (unit == "number") {
    tag_app(sprintf("demo_continuous(c(0, 1e6), labels = %s())", fun_name))
  } else if (unit == "percent") {
    tag_app(sprintf("demo_continuous(c(0, 1), labels = %s())", fun_name))
  }
  fun_code <- c(fun_code, fun_ex)

  # Function header
  fun_code <- c(
    fun_code,
    sprintf('%s <- function(', thinkr::clean_vec(fun_name))
  )

  default_params <- list(
    accuracy = NULL,
    scale = 1,
    big.mark = ",",
    decimal.mark = ".",
    prefix = "",
    suffix = "",
    trim = TRUE
  )

  params <- modifyList(default_params, params)

  # Function Arguments
  fun_args <- purrr::imap_chr(params, function(x, y) {
    x <- if (is.null(x)) "NULL" else if (is.character(x)) paste0("\"", x ,"\"" ) else as.character(x)
    sprintf("  %s = %s,", thinkr::clean_vec(y), x)
  })
  fun_args <- c(fun_args, " ...) {")

  fun_code <- c(fun_code, fun_args)

  # Function Body

  fun_body <- sprintf("label_%s(accuracy = accuracy, scale = scale, big.mark = big.mark, decimal.mark = decimal.mark, trim = trim, ...)", unit)
  fun_body <- c(fun_body, "}")

  fun_code <- c(fun_code, fun_body)

  fun_code <- styler::style_text(fun_code)

  fun_code
}


#fun_code <- fun_code[fun_code != ""]

  # label_percent_de <- function(accuracy = 1, scale = 100, big.mark = ".", decimal.mark = ",", prefix = "",
  #                              suffix = "%", trim = TRUE, ...) {
  #   label_percent(
  #     accuracy = accuracy, scale = scale, big.mark = big.mark, decimal.mark = decimal.mark, prefix = "",
  #     suffix = suffix, trim = trim, ...
  #   )
  # }

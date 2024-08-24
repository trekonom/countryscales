#' Locales
#'
#' A convenience function which returns a dataframe of available locales and
#' countries.
#'
#' @return a dataframe
#'
#' @export
#'
#' @examples
#' show_locales()
show_locales <- function() {
  #x <- countryscales::locales[c("locale", "cntry")]
  x <- countryscales::locales[c("locale")]
  x$locale <- gsub("_", "-", x$locale)
  return(x)
}

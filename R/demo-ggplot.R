#' ggplo2 demo
#'
#' @param x
#' @param scale_name
#'
#' @return
#' @export
#'
#' @examples
#' demo_number(c(32, 212), scale_name = "number")
demo_number <- function(x, scale_name) {
  demo_ggplot(x, paste("scale", "x", scale_name, sep = "_"))
}

demo_ggplot <- function(x, scale_name, ...) {
  call <- substitute(list(...))
  call[[1]] <- as.name(scale_name)
  cat(paste0(deparse(call), "\n", collapse = ""))
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    message("Skipping; ggplot2 not installed")
    return(invisible())
  }
  scale <- getExportedValue("countryscales", scale_name)
  df <- data.frame(x = x, stringsAsFactors = FALSE)
  ggplot2::ggplot(df, ggplot2::aes(x, 1)) +
    ggplot2::geom_blank() +
    scale(NULL, ...) +
    ggplot2::scale_y_continuous(NULL,
      breaks = NULL
    ) +
    ggplot2::theme(aspect.ratio = 1 / 5)
}

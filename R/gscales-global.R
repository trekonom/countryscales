# Adapted from gggplot2
#
# Environment that holds various global settings for ggplot.
gscales_global <- new.env(parent = emptyenv())

# x aesthetics
gscales_global$x_aes <- c(
  "x", "xmin", "xmax", "xend", "xintercept",
  "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper", "x0"
)

gscales_global$y_aes <- c(
  "y", "ymin", "ymax", "yend", "yintercept",
  "ymin_final", "ymax_final", "lower", "middle", "upper", "y0"
)

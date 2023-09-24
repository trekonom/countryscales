# ex-label_euro_de.R
set.seed(42)
x <- 100 * runif(10)
# Formatting numbers as currencies
euro_de(x)
dollar_de(x, accuracy = .1)
# For other currencies or if you want to switch the position
# of the currency symbol switch the suffix and/or prefix
# Dollar symbol after number
euro_de(x, suffix = .dollar)
# British pound before number
dollar_de(x, prefix = .pound)
# For use as labeller e.g. with ggplot2
library(ggplot2)
p <- ggplot(gapminder15, aes(gdp_per_cap, life_exp)) +
  geom_point()
# Dollar prefix
p +
  scale_x_continuous(labels = label_dollar_de())
# Euro suffix
p +
  scale_x_continuous(labels = label_euro_de())

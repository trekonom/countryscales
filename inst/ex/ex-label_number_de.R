# ex-label_number_de.R
set.seed(42)
x <- 100 * runif(10)
# Formatting numbers in decimal format
number_de(x)
number_de(x, accuracy = .1)
# For use as labeller e.g. with ggplot2
library(ggplot2)
ggplot(gapminder15, aes(gdp_per_cap, life_exp)) +
  geom_point() +
  scale_x_continuous(labels = label_number_locale())

ggplot(gapminder15, aes(gdp_per_cap, life_exp)) +
  geom_point() +
  scale_x_continuous(labels = label_number_de())

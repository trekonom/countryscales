# ex-label_percent_de.R
set.seed(42)
x <- runif(10)
# Formatting numbers as percentages
percent_de(x)
percent_de(x, accuracy = .1)
# For use as labeller e.g. with ggplot2
library(ggplot2)
gapminder15$pct_pop <- gapminder15$pop / sum(gapminder15$pop)
ggplot(gapminder15, aes(gdp_per_cap, pct_pop)) +
  geom_point() +
  scale_y_continuous(labels = label_percent_de())

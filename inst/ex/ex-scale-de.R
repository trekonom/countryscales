library(ggplot2)

# formatting axis labels as numbers
base <- ggplot(gapminder15, aes(gdp_per_cap, life_exp)) +
  geom_point()

base +
  scale_x_number() +
  scale_y_number(suffix = " yrs")

base +
  scale_x_dollar_de() +
  scale_y_number_de(suffix = " Jahre")

# formatting axis labels as percentages
gapsum <- by(gapminder15, gapminder15$region, function(df) {
    with(df, data.frame(region = region[[1]], n = nrow(df)))
  })
gapsum <- do.call(rbind, gapsum)
gapsum <- transform(gapsum, n = n / sum(n))

ggplot(gapsum, aes(n, reorder(region, n))) +
  geom_col() +
  scale_x_percent_de(accuracy = .1)

ggplot(gapsum, aes(reorder(region, n), n)) +
  geom_col() +
  scale_y_percent_de(accuracy = .1)

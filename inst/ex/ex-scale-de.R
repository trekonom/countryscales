library(ggplot2)
library(dplyr)

# formatting axis labels as numbers
ggplot(gapminder15, aes(gdp_per_cap, life_exp)) +
  geom_point() +
  scale_x_number_de() +
  scale_y_number_de(suffix = " yrs")

# formatting axis labels as percentages
gapsum <- gapminder15 %>%
  count(region) %>%
  mutate(n = n / sum(n))

ggplot(gapsum, aes(n, reorder(region, n))) +
  geom_col() +
  scale_x_percent_de()

ggplot(gapsum, aes(reorder(region, n), n)) +
  geom_col() +
  scale_y_percent_de()

# Make dfs with summary statistics of the gapminder dataset
# useful for plot or chart examples like chart_dumbbell, ...

library(dplyr)

sumstat <- function(x, ...) {
  summarise(
    .data = x,
    across(
      .cols = c(...),
      .fns = list(
        mean = mean,
        sd = sd,
        med = median,
        q25 = ~ quantile(., .25),
        q75 = ~ quantile(., .75),
        min = min,
        max = max
      )
    ),
    .groups = "drop"
  )
}

d <- gapminder %>%
  group_by(year, region) %>%
  sumstat(life_exp, gdp_per_cap, pop)

dw <- gapminder %>%
  group_by(year) %>%
  sumstat(life_exp, gdp_per_cap, pop) %>%
  mutate(region = "World")

gapsum <- bind_rows(d, dw)
gapsum_0015 <- filter(gapsum, year %in% c(2000, 2015))

usethis::use_data(gapsum, overwrite = TRUE)
usethis::use_data(gapsum_0015, overwrite = TRUE)

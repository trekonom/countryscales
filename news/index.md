# Changelog

## countryscales 0.2.0

Initial CRAN release.

- [`label_number_locale()`](https://trekonom.github.io/countryscales/reference/label-locale.md),
  [`label_percent_locale()`](https://trekonom.github.io/countryscales/reference/label-locale.md),
  and
  [`label_currency_locale()`](https://trekonom.github.io/countryscales/reference/label-locale.md)
  format numbers, percentages, and currencies using the conventions of
  any of several hundred locales from the Unicode Common Locale Data
  Repository (CLDR), sourced via the `i18n` package — thousands
  separator, decimal mark, currency symbol placement, and sign placement
  all follow the target locale. Locales using the Indian numbering
  system (grouping by threes then twos) are not supported, as they
  deviate from the standard grouping this package implements.
- Dedicated `_de()`, `_ch()`, and `_us()` wrapper families (e.g.
  [`label_number_de()`](https://trekonom.github.io/countryscales/reference/label-de.md)/[`number_de()`](https://trekonom.github.io/countryscales/reference/label-de.md),
  [`label_percent_ch()`](https://trekonom.github.io/countryscales/reference/label-ch.md)/[`percent_ch()`](https://trekonom.github.io/countryscales/reference/label-ch.md),
  [`label_currency_us()`](https://trekonom.github.io/countryscales/reference/label-us.md)/[`currency_us()`](https://trekonom.github.io/countryscales/reference/label-us.md))
  provide ready-to-use formatters for Germany, Switzerland, and the
  United States.
- [`scale_x_number_locale()`](https://trekonom.github.io/countryscales/reference/scale-locale.md)/[`scale_y_number_locale()`](https://trekonom.github.io/countryscales/reference/scale-locale.md)
  and their `_percent_`/`_currency_` and `_de`/`_ch`/`_us` counterparts
  label `ggplot2` axes directly using the same locale-aware formatting.
- [`show_locales()`](https://trekonom.github.io/countryscales/reference/show_locales.md)
  lists all supported locale codes.

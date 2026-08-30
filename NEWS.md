# countryscales 0.2.0

Initial CRAN release.

* `label_number_locale()`, `label_percent_locale()`, and
  `label_currency_locale()` format numbers, percentages, and currencies
  using the conventions of any of several hundred locales from the Unicode
  Common Locale Data Repository (CLDR), sourced via the `i18n` package —
  thousands separator, decimal mark, currency symbol placement, and sign
  placement all follow the target locale. Locales using the Indian
  numbering system (grouping by threes then twos) are not supported, as
  they deviate from the standard grouping this package implements.
* Dedicated `_de()`, `_ch()`, and `_us()` wrapper families (e.g.
  `label_number_de()`/`number_de()`, `label_percent_ch()`/`percent_ch()`,
  `label_currency_us()`/`currency_us()`) provide ready-to-use formatters
  for Germany, Switzerland, and the United States.
* `scale_x_number_locale()`/`scale_y_number_locale()` and their
  `_percent_`/`_currency_` and `_de`/`_ch`/`_us` counterparts label
  `ggplot2` axes directly using the same locale-aware formatting.
* `show_locales()` lists all supported locale codes.

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
* `label_number_xx()`/`label_percent_xx()`/`label_currency_xx()` and their
  `scale_x/y_number/percent/currency_xx()` counterparts additionally cover
  24 more locales (#4): France (`fr`), the United Kingdom (`gb`), Italy
  (`it`), Spain (`es`), Austria (`at`), Ireland (`ie`), Portugal (`pt`),
  Sweden (`se`), Norway (`no`), Finland (`fi`), Denmark (`dk`), Brazil
  (`br`), Argentina (`ar`), Mexico (`mx`), Canada (`ca`), Japan (`jp`),
  China (`cn`), Indonesia (`id`), South Korea (`kr`), Australia (`au`),
  Saudi Arabia (`sa`), South Africa (`za`), Turkey (`tr`), and Russia
  (`ru`) — every G20 member except India, whose Indian numbering system
  isn't supported (see above).
* `scale_x_number_locale()`/`scale_y_number_locale()` and their
  `_percent_`/`_currency_` and `_de`/`_ch`/`_us` counterparts label
  `ggplot2` axes directly using the same locale-aware formatting.
* `show_locales()` lists all supported locale codes.

demo_number(c(-1e6, 1e6), scale_name = "number_locale", locale = "fr-FR")
# scale_x_number_locale()
demo_number(c(-1, 1), scale_name = "percent_locale", locale = "it-IT")
# scale_x_percent_locale()
demo_number(c(-1e4, 1e4),
  scale_name = "currency_locale",
  locale = "ja-JP", currency = "JPY"
)
# scale_x_currency_locale()

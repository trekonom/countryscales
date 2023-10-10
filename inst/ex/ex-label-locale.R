require(scales)

demo_continuous(c(-1e6, 1e6),
  labels = label_number_locale(locale = "fr-FR")
)
demo_continuous(c(-1, 1),
  labels = label_percent_locale(locale = "it-IT", accuracy = .01)
)
demo_continuous(c(-1, 1),
  labels = label_currency_locale(
    locale = "ja-JP", accuracy = .1,
    currency = "JPY"
  )
)

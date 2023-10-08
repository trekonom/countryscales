demo_continuous(c(-1e6, 1e6),
  labels = label_number_de()
)
demo_continuous(c(-1, 1),
  labels = label_percent_de(accuracy = .01)
)
demo_continuous(c(-1, 1),
  labels = label_currency_de(accuracy = .1, currency = "USD")
)

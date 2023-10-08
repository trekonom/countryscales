demo_continuous(c(-1e6, 1e6),
  labels = label_number_us()
)
demo_continuous(c(-1, 1),
  labels = label_percent_us(accuracy = .1)
)
demo_continuous(c(-1, 1),
  labels = label_currency_us(accuracy = .1)
)

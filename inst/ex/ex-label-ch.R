require(scales)

demo_continuous(c(-1e6, 1e6),
  labels = label_number_ch()
)
demo_continuous(c(-1, 1),
  labels = label_percent_ch(accuracy = .01)
)
demo_continuous(c(-1, 1),
  labels = label_currency_ch(accuracy = .1)
)

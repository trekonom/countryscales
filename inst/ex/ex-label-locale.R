# rendered directly (rather than via scales::demo_continuous()) because
# several locales' group/currency signs aren't in the check device's font
label_number_locale(locale = "fr-FR")(c(-1e6, 0, 1e6))
label_percent_locale(locale = "it-IT", accuracy = .01)(c(-1, 0, 1))
label_currency_locale(
  locale = "ja-JP", accuracy = .1,
  currency = "JPY"
)(c(-1e4, 0, 1e4))

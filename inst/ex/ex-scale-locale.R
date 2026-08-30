# not rendered via demo_number(): several locales' group/currency signs
# aren't in the check device's font, so only the (unrendered) scale
# objects are shown
scale_x_number_locale(locale = "fr-FR")
scale_x_percent_locale(locale = "it-IT")
scale_x_currency_locale(locale = "ja-JP", currency = "JPY")

# rendered directly (rather than via scales::demo_continuous()) because
# several locales' group/currency signs aren't in the check device's font
label_number_tr()(c(-1e6, 0, 1e6))
label_percent_tr(accuracy = .01)(c(-1, 0, 1))
label_currency_tr(accuracy = .1)(c(-1e6, 0, 1e6))


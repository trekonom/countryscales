# rendered directly (rather than via scales::demo_continuous()) because
# several locales' group/currency signs aren't in the check device's font
label_number_cn()(c(-1e6, 0, 1e6))
label_percent_cn(accuracy = .01)(c(-1, 0, 1))
label_currency_cn(accuracy = .1)(c(-1e6, 0, 1e6))


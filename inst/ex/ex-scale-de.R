demo_number(c(-1e6, 1e6), scale_name = "number_de")
scale_x_number_de()
demo_number(c(-1, 1), scale_name = "percent_de")
scale_x_percent_de()
# not rendered via demo_number(): the euro sign isn't in the check
# device's font, so only the (unrendered) scale object is shown
scale_x_currency_de()

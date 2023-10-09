## code to prepare `currencies` dataset goes here

data(currencies, package = "i18n")

currencies <- currencies |>
  mutate(
    # Fixes.
    currency_symbol = case_when(
      locale %in% c(
        "xh", "und",
        "nds", "nds-NL", "lag"
      ) & currency_code == "USD" ~ "$",
      .default = currency_symbol
    )
  )
usethis::use_data(currencies, overwrite = TRUE)

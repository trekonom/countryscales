label_helper <- function(locale, currency = "USD") {
  label_currency_locale(
    locale = locale,
    currency = currency
  )
}
value <- c(-1, 1) * 123456.789

currency <- "USD"

test_that("label_currency_locale works", {

  expect_equal(
    label_helper("de-DE")(value),
    c("-123.457\u00a0$", "123.457\u00a0$")
  )

  expect_equal(
    label_helper("de-CH")(value),
    c("$-123\u2019457", "$\u00a0123\u2019457")
  )

  expect_equal(
    label_helper("en-US")(value),
    c("-$123,457", "$123,457")
  )

  expect_equal(
    label_helper("fr-FR")(value),
    c("-123\u202f457\u00a0$US", "123\u202f457\u00a0$US")
  )
})

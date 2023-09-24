label_currency_locale(locale = "de-CH")(123456.789)

value <- c(-1, 1) * 123456.789

test_that("label_currency_locale works", {
  expect_equal(
    label_currency_locale(
      locale = "de-DE"
    )(value),
    c("-123.457 $", "123.457 $")
  )

  expect_equal(
    label_currency_locale(
      locale = "de-CH"
    )(value),
    c("$-123\u2019457", "$ 123\u2019457")
  )

  expect_equal(
    label_currency_locale(
      locale = "en-US"
    )(value),
    c("-$123,457", "$123,457")
  )

  expect_equal(
    label_currency_locale(
      locale = "fr-FR"
    )(value),
    c("-123\u202f457 $", "123\u202f457 $")
  )
})

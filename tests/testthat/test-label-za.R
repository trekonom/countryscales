# Correctness of label_number_locale()/label_percent_locale()/
# label_currency_locale() against Intl.js is already covered exhaustively
# (across all locales, including non-Latin numbering systems) by
# test-label-locale.R. These tests just confirm label_number_za()/
# label_percent_za()/label_currency_za() delegate to that engine with the
# right locale (and, for currency, the right native currency) pinned.
locale <- "af-ZA"

x <- c(123456, -123456)
y <- c(.789, -.789)

test_that("label_number_za matches label_number_locale", {
  expect_equal(
    label_number_za(accuracy = 1)(x),
    label_number_locale(accuracy = 1, locale = locale)(x)
  )
})

test_that("label_percent_za matches label_percent_locale", {
  expect_equal(
    label_percent_za()(y),
    label_percent_locale(locale = locale)(y)
  )
})

test_that("label_currency_za uses ZAR as the default currency", {
  expect_equal(
    label_currency_za()(x),
    label_currency_locale(locale = locale, currency = "ZAR")(x)
  )
})


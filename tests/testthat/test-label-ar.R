locale <- "es-AR"

dat <- readRDS(
  system.file("extdata", "testloc.rds", package = "countryscales")
)
dat <- dat[dat$locale == locale, ]

x <- 123456
y <- .789
z <- x

test_that("returns same formatted strings as Intl.js", {
  expect_equal(
    label_number_ar(accuracy = 1)(c(x, -x)),
    unlist(
      dat[c("number_pos", "number_neg")],
      use.names = FALSE
    )
  )
  expect_equal(
    label_percent_ar()(c(y, -y)),
    unlist(
      dat[c("percent_pos", "percent_neg")],
      use.names = FALSE
    )
  )
  # the fixture's currency columns are USD formatted in this locale
  # (matching label_currency_locale()'s own USD default), not the
  # locale's native currency, so compare against label_currency_locale()
  # directly here rather than label_currency_ar() (which defaults to "ARS")
  expect_equal(
    label_currency_locale(accuracy = 1, locale = locale)(c(z, -z)),
    unlist(
      dat[c("currency_pos", "currency_neg")],
      use.names = FALSE
    )
  )
})

test_that("label_currency_ar uses ARS as the default currency", {
  expect_equal(
    label_currency_ar()(c(z, -z)),
    label_currency_locale(locale = "es-AR", currency = "ARS")(c(z, -z))
  )
})


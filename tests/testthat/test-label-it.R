locale <- "it-IT"

dat <- readRDS(
  system.file("extdata", "testloc.rds", package = "countryscales")
)
dat <- dat[dat$locale == locale, ]

x <- 123456
y <- .789
z <- x

test_that("returns same formatted strings as Intl.js", {
  expect_equal(
    label_number_it(accuracy = 1)(c(x, -x)),
    unlist(
      dat[c("number_pos", "number_neg")],
      use.names = FALSE
    )
  )
  expect_equal(
    label_percent_it()(c(y, -y)),
    unlist(
      dat[c("percent_pos", "percent_neg")],
      use.names = FALSE
    )
  )
  # the fixture's currency columns are USD formatted in this locale
  # (matching label_currency_locale()'s own USD default), not the
  # locale's native currency, so compare against label_currency_locale()
  # directly here rather than label_currency_it() (which defaults to "EUR")
  expect_equal(
    label_currency_locale(accuracy = 1, locale = locale)(c(z, -z)),
    unlist(
      dat[c("currency_pos", "currency_neg")],
      use.names = FALSE
    )
  )
})

test_that("label_currency_it uses EUR as the default currency", {
  expect_equal(
    label_currency_it()(c(z, -z)),
    label_currency_locale(locale = "it-IT", currency = "EUR")(c(z, -z))
  )
})


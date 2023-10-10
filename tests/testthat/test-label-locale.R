dat <- readRDS(
  system.file("extdata", "testloc.rds", package = "countryscales")
)

test_that("label_percent_locale works with prefixed percent symbol", {
  value <- .789
  locales <- c("eu", "tr", "ku")
  lapply(locales, \(locale) {
    expect_equal(
      label_percent_locale(locale = locale)(c(value, -value)),
      unlist(
        dat[dat$locale == locale, c("percent_pos", "percent_neg")],
        use.names = FALSE
      )
    )
  })
})

test_that("label_number_locale works", {
  value <- 123456
  lapply(locales$locale, \(locale) {
    expect_equal(
      paste(
        locale,
        label_number_locale(locale = locale)(c(value, -value)),
        sep = ": "
      ),
      paste(
        locale,
        unlist(
          dat[dat$locale == locale, c("number_pos", "number_neg")],
          use.names = FALSE
        ),
        sep = ": "
      )
    )
  })
})

test_that("label_currency_locale works", {
  value <- 123456
  lapply(locales$locale, \(locale) {
    expect_equal(
      paste(
        locale,
        label_currency_locale(locale = locale)(c(value, -value)),
        sep = ": "
      ),
      paste(
        locale,
        unlist(
          dat[dat$locale == locale, c("currency_pos", "currency_neg")],
          use.names = FALSE
        ),
        sep = ": "
      )
    )
  })
})

test_that("label_percent_locale works", {
  value <- .789
  lapply(locales$locale, \(locale) {
    expect_equal(
      paste(
        locale,
        label_percent_locale(locale = locale)(c(value, -value)),
        sep = ": "
      ),
      paste(
        locale,
        unlist(
          dat[dat$locale == locale, c("percent_pos", "percent_neg")],
          use.names = FALSE
        ),
        sep = ": "
      )
    )
  })
})

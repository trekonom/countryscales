locale <- "en-US"

dat <- readRDS(
  system.file("extdata", "testloc.rds", package = "countryscales")
)
dat <- dat[dat$locale == locale, ]

x <- 123456
y <- .789
z <- x

test_that("returns same formatted strings as Intl.js", {
  expect_equal(
    label_number_us(accuracy = 1)(c(x, -x)),
    unlist(
      dat[c("number_pos", "number_neg")],
      use.names = FALSE
    )
  )
  expect_equal(
    label_percent_us()(c(y, -y)),
    unlist(
      dat[c("percent_pos", "percent_neg")],
      use.names = FALSE
    )
  )
  expect_equal(
    label_currency_locale(accuracy = 1, locale = locale)(c(z, -z)),
    unlist(
      dat[c("currency_pos", "currency_neg")],
      use.names = FALSE
    )
  )
})

test_that("uses a ’ as big.mark and a . as decimal mark", {
  expect_equal(label_number_us()(1200.243), "1,200")
  expect_equal(label_number_us(accuracy = .1)(1.243), "1.2")
  expect_equal(label_percent_us()(.243), "24%")
  expect_equal(label_percent_us(accuracy = .1)(.243), "24.3%")
})

test_that("number_us adds a suffix and a prefix", {
  value <- 100
  expect_equal(
    number_us(value, scale = 1, suffix = "%", prefix = "%"),
    c("%100%")
  )
})

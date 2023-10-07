dat <- readRDS(
  system.file("extdata", "testloc.rds", package = "countryscales")
)

x <- 123456.789
y <- .789
z <- x

test_that("label_percent_locale works with prefixed percent symbol", {
  locale <- "eu"
  expect_equal(
    label_percent_locale(locale = locale)(c(y, -y)),
    unlist(
      dat[dat$locale == locale, c("percent_pos", "percent_neg")],
      use.names = FALSE
    )
  )
  locale <- "tr"
  expect_equal(
    label_percent_locale(locale = locale)(c(y, -y)),
    unlist(
      dat[dat$locale == locale, c("percent_pos", "percent_neg")],
      use.names = FALSE
    )
  )
  locale <- "ku"
  expect_equal(
    label_percent_locale(locale = locale)(c(y, -y)),
    unlist(
      dat[dat$locale == locale, c("percent_pos", "percent_neg")],
      use.names = FALSE
    )
  )
})

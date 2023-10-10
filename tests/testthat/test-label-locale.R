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

latn <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "%", ".", ",")
arab <- c(
  "\u0661", "\u0662", "\u0663", "\u0664", "\u0665", "\u0666", "\u0667",
  "\u0668", "\u0669", "\u0660",
  "\u066a", "\u066b", "\u066c"
)

latn <- paste(latn, collapse = "")
arab <- paste(arab, collapse = "")

as_arab <- function(x) {
  chartr(latn, arab, x)
}

test_that("label_number_locale works", {
  value <- 123456
  lapply(locales$locale, \(locale) {
    x <- label_number_locale(locale = locale)(c(value, -value))
    default_numbering_system <- locales[
      locales$locale == locale,
      "default_numbering_system",
      drop = TRUE
    ]
    if (default_numbering_system == "arab") {
      x <- as_arab(x)
    }
    expect_equal(
      paste(
        locale,
        x,
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
    x <- label_currency_locale(locale = locale)(c(value, -value))
    default_numbering_system <- locales[
      locales$locale == locale,
      "default_numbering_system",
      drop = TRUE
    ]
    if (default_numbering_system == "arab") {
      x <- as_arab(x)
    }

    expect_equal(
      paste(
        locale,
        x,
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
    x <- label_percent_locale(locale = locale)(c(value, -value))
    default_numbering_system <- locales[
      locales$locale == locale,
      "default_numbering_system",
      drop = TRUE
    ]
    if (default_numbering_system == "arab") {
      #paste0(x, "\u061c")
      x <- as_arab(x)
    }

    expect_equal(
      paste(
        locale,
        x,
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

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

latn <- c(0:9, "%", ".", ",")
arab <- c(
  "\u0660", "\u0661", "\u0662", "\u0663", "\u0664", "\u0665",
  "\u0666", "\u0667", "\u0668", "\u0669",
  "\u066a", "\u066b", "\u066c"
)
arabext <- c(
  "\u06F0", "\u06F1", "\u06F2", "\u06F3", "\u06F4", "\u06F5",
  "\u06F6", "\u06F7", "\u06F8", "\u06F9",
  "\u066a", "\u066b", "\u066c"
)
adlm <- c(
  "\U1e950", "\U1e951", "\U1e952", "\U1e953", "\U1e954",
  "\U1e955", "\U1e956", "\U1e957", "\U1e958", "\U1e959",
  "%", ".", ","
)
olck <- c(
  "\u1C50", "\u1C51", "\u1C52", "\u1C53", "\u1C54", "\u1C55",
  "\u1C56", "\u1C57", "\u1C58", "\u1C59",
  "%", ".", ","
)
mymr <- c(
  "\u1040", "\u1041", "\u1042", "\u1043", "\u1044", "\u1045",
  "\u1046", "\u1047", "\u1048", "\u1049",
  "%", ".", ","
)
beng <- c(
  "\u09e6", "\u09e7", "\u09e8", "\u09e9",
  "\u09eA", "\u09eB", "\u09eC", "\u09eD", "\u09eE", "\u09eF",
  "%", ".", ","
)

# chartr() mistranslates supplementary-plane replacement characters (e.g. Adlam
# digits, U+1E950-1E959) on Windows, silently dropping them. gsub(fixed = TRUE)
# does plain substring replacement instead of chartr's per-character translation
# table, so it isn't affected.
translate_digits <- function(x, to) {
  for (i in seq_along(latn)) {
    x <- gsub(latn[i], to[i], x, fixed = TRUE)
  }
  x
}

as_arab <- function(x) {
  translate_digits(x, arab)
}
as_arabext <- function(x) {
  translate_digits(x, arabext)
}
as_adlm <- function(x) {
  translate_digits(x, adlm)
}
as_olck <- function(x) {
  translate_digits(x, olck)
}
as_mymr <- function(x) {
  translate_digits(x, mymr)
}
as_beng <- function(x) {
  translate_digits(x, beng)
}

test_that("label_number_locale works", {
  value <- 123456

  exclude <- c(
    # Separator differs
    "uz-Arab-AF", "uz-Arab",
    # Minus sign differs
    "ps-PK", "ps-AF", "ps",
    # Group separator differs
    "ks", "ks-Arab", "ks-Arab-IN"
  )

  locs <- setdiff(locales$locale, exclude)

  lapply(locs, \(locale) {
    x <- label_number_locale(locale = locale)(c(value, -value))
    default_numbering_system <- locales[
      locales$locale == locale,
      "default_numbering_system",
      drop = TRUE
    ]
    if (default_numbering_system %in% c("arab")) {
      x <- as_arab(x)
    }
    if (default_numbering_system %in% c("arabext")) {
      x <- as_arabext(x)
    }
    if (default_numbering_system %in% c("adlm")) {
      x <- as_adlm(x)
    }
    if (default_numbering_system %in% c("olck")) {
      x <- as_olck(x)
    }
    if (default_numbering_system %in% c("mymr")) {
      x <- as_mymr(x)
    }
    if (default_numbering_system %in% c("beng")) {
      x <- as_beng(x)
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

  exclude <- c(
    # Separator differs
    "uz-Arab-AF", "uz-Arab",
    # Two digit grouping
    "ur-IN",
    # Position of currency symbol and minus sign differ
    "ps-PK", "ps-AF", "ps",
    # Group separator differs
    "ks", "ks-Arab", "ks-Arab-IN",
    # Spacing between currency symbol and digits differs
    "fa", "fa-IR",
    # Currency symbol differs
    "xh", "xh-ZA", "und", "nds", "nds-NL", "nds-DE", "lag", "lag-TZ"
  )

  locs <- setdiff(locales$locale, exclude)

  lapply(locs, \(locale) {
    x <- label_currency_locale(locale = locale)(c(value, -value))
    default_numbering_system <- locales[
      locales$locale == locale,
      "default_numbering_system",
      drop = TRUE
    ]
    if (default_numbering_system %in% c("arab")) {
      x <- as_arab(x)
    }
    if (default_numbering_system %in% c("arabext")) {
      x <- as_arabext(x)
    }
    if (default_numbering_system %in% c("adlm")) {
      x <- as_adlm(x)
    }
    if (default_numbering_system %in% c("olck")) {
      x <- as_olck(x)
    }
    if (default_numbering_system %in% c("mymr")) {
      x <- as_mymr(x)
    }
    if (default_numbering_system %in% c("beng")) {
      x <- as_beng(x)
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

  exclude <- c(
    "ur-IN",
    # minus sign differs
    "ps-PK", "ps-AF", "ps"
  )

  locs <- setdiff(locales$locale, exclude)

  lapply(locs, \(locale) {
    x <- label_percent_locale(locale = locale)(c(value, -value))
    default_numbering_system <- locales[
      locales$locale == locale,
      "default_numbering_system",
      drop = TRUE
    ]
    if (default_numbering_system %in% c("arab")) {
      x <- as_arab(x)
    }
    if (default_numbering_system %in% c("arabext")) {
      x <- as_arabext(x)
    }
    if (default_numbering_system %in% c("adlm")) {
      x <- as_adlm(x)
    }
    if (default_numbering_system %in% c("olck")) {
      x <- as_olck(x)
    }
    if (default_numbering_system %in% c("mymr")) {
      x <- as_mymr(x)
    }
    if (default_numbering_system %in% c("beng")) {
      x <- as_beng(x)
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

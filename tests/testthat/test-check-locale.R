test_that("check_locale returns the matching locale row for a valid locale", {
  x <- check_locale("de-DE")

  expect_s3_class(x, "data.frame")
  expect_equal(nrow(x), 1)
  expect_equal(x$locale, "de-DE")
})

test_that("check_locale errors informatively for an invalid locale", {
  expect_error(
    check_locale("xx-YY"),
    class = "countryscales_error_invalid_locale"
  )
  expect_error(
    check_locale("xx-YY"),
    regexp = '"xx-YY"',
    fixed = TRUE
  )
  expect_error(
    check_locale("xx-YY"),
    regexp = "show_locales\\(\\)"
  )
})

test_that("check_locale suggests a close match for a likely typo", {
  err <- tryCatch(check_locale("de-De"), error = function(e) e)

  expect_s3_class(err, "countryscales_error_invalid_locale")
  expect_match(conditionMessage(err), '"de-DE"', fixed = TRUE)
})

test_that("label_number/percent/currency_locale error informatively on an invalid locale", {
  expect_error(
    label_number_locale(locale = "not-a-locale"),
    class = "countryscales_error_invalid_locale"
  )
  expect_error(
    label_percent_locale(locale = "not-a-locale"),
    class = "countryscales_error_invalid_locale"
  )
  expect_error(
    label_currency_locale(locale = "not-a-locale"),
    class = "countryscales_error_invalid_locale"
  )
})

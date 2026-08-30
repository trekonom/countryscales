get_scale_labels <- function(scale, breaks) {
  scale$train(range(breaks))
  scale$get_labels(breaks)
}

test_that("scale_x/y_number_locale label like label_number_locale", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_number_locale(locale = "de-DE"), breaks),
    label_number_locale(locale = "de-DE")(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_number_locale(locale = "fr-FR"), breaks),
    label_number_locale(locale = "fr-FR")(breaks)
  )
})

test_that("scale_x/y_percent_locale label like label_percent_locale", {
  breaks <- c(.1, .5, .9)

  expect_equal(
    get_scale_labels(scale_x_percent_locale(locale = "de-DE"), breaks),
    label_percent_locale(locale = "de-DE")(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_percent_locale(locale = "it-IT"), breaks),
    label_percent_locale(locale = "it-IT")(breaks)
  )
})

test_that("scale_x/y_currency_locale label like label_currency_locale", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(
      scale_x_currency_locale(locale = "ja-JP", currency = "JPY"),
      breaks
    ),
    label_currency_locale(locale = "ja-JP", currency = "JPY")(breaks)
  )
  expect_equal(
    get_scale_labels(
      scale_y_currency_locale(locale = "en-US", currency = "USD"),
      breaks
    ),
    label_currency_locale(locale = "en-US", currency = "USD")(breaks)
  )
})

test_that("set_sec_axis converts a formula to a secondary axis", {
  sc <- scale_x_number_locale(sec.axis = ~ . * 2)
  expect_true(inherits(sc$secondary.axis, "AxisSecondary"))
})

test_that("set_sec_axis passes through sec_axis() objects unchanged", {
  sc <- scale_x_number_locale(sec.axis = ggplot2::sec_axis(~ . * 2))
  expect_true(inherits(sc$secondary.axis, "AxisSecondary"))
})

test_that("set_sec_axis errors on invalid sec.axis input", {
  expect_error(
    scale_x_number_locale(sec.axis = "not valid"),
    "Secondary axes must be specified using 'sec_axis\\(\\)'"
  )
})

get_scale_labels <- function(scale, breaks) {
  scale$train(range(breaks))
  scale$get_labels(breaks)
}

test_that("scale_x/y_number_ie label like label_number_ie", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_number_ie(), breaks),
    label_number_ie()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_number_ie(), breaks),
    label_number_ie()(breaks)
  )
})

test_that("scale_x/y_percent_ie label like label_percent_ie", {
  breaks <- c(.1, .5, .9)

  expect_equal(
    get_scale_labels(scale_x_percent_ie(), breaks),
    label_percent_ie()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_percent_ie(), breaks),
    label_percent_ie()(breaks)
  )
})

test_that("scale_x/y_currency_ie label like label_currency_ie", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_currency_ie(), breaks),
    label_currency_ie()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_currency_ie(), breaks),
    label_currency_ie()(breaks)
  )
})


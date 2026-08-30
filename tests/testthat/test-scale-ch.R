get_scale_labels <- function(scale, breaks) {
  scale$train(range(breaks))
  scale$get_labels(breaks)
}

test_that("scale_x/y_number_ch label like label_number_ch", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_number_ch(), breaks),
    label_number_ch()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_number_ch(), breaks),
    label_number_ch()(breaks)
  )
})

test_that("scale_x/y_percent_ch label like label_percent_ch", {
  breaks <- c(.1, .5, .9)

  expect_equal(
    get_scale_labels(scale_x_percent_ch(), breaks),
    label_percent_ch()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_percent_ch(), breaks),
    label_percent_ch()(breaks)
  )
})

test_that("scale_x/y_currency_ch label like label_currency_ch", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_currency_ch(), breaks),
    label_currency_ch()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_currency_ch(), breaks),
    label_currency_ch()(breaks)
  )
})

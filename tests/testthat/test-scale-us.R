get_scale_labels <- function(scale, breaks) {
  scale$train(range(breaks))
  scale$get_labels(breaks)
}

test_that("scale_x/y_number_us label like label_number_us", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_number_us(), breaks),
    label_number_us()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_number_us(), breaks),
    label_number_us()(breaks)
  )
})

test_that("scale_x/y_percent_us label like label_percent_us", {
  breaks <- c(.1, .5, .9)

  expect_equal(
    get_scale_labels(scale_x_percent_us(), breaks),
    label_percent_us()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_percent_us(), breaks),
    label_percent_us()(breaks)
  )
})

test_that("scale_x/y_currency_us label like label_currency_us", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_currency_us(), breaks),
    label_currency_us()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_currency_us(), breaks),
    label_currency_us()(breaks)
  )
})

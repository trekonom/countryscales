get_scale_labels <- function(scale, breaks) {
  scale$train(range(breaks))
  scale$get_labels(breaks)
}

test_that("scale_x/y_number_kr label like label_number_kr", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_number_kr(), breaks),
    label_number_kr()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_number_kr(), breaks),
    label_number_kr()(breaks)
  )
})

test_that("scale_x/y_percent_kr label like label_percent_kr", {
  breaks <- c(.1, .5, .9)

  expect_equal(
    get_scale_labels(scale_x_percent_kr(), breaks),
    label_percent_kr()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_percent_kr(), breaks),
    label_percent_kr()(breaks)
  )
})

test_that("scale_x/y_currency_kr label like label_currency_kr", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_currency_kr(), breaks),
    label_currency_kr()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_currency_kr(), breaks),
    label_currency_kr()(breaks)
  )
})


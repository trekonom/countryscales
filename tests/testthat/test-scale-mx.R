get_scale_labels <- function(scale, breaks) {
  scale$train(range(breaks))
  scale$get_labels(breaks)
}

test_that("scale_x/y_number_mx label like label_number_mx", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_number_mx(), breaks),
    label_number_mx()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_number_mx(), breaks),
    label_number_mx()(breaks)
  )
})

test_that("scale_x/y_percent_mx label like label_percent_mx", {
  breaks <- c(.1, .5, .9)

  expect_equal(
    get_scale_labels(scale_x_percent_mx(), breaks),
    label_percent_mx()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_percent_mx(), breaks),
    label_percent_mx()(breaks)
  )
})

test_that("scale_x/y_currency_mx label like label_currency_mx", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_currency_mx(), breaks),
    label_currency_mx()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_currency_mx(), breaks),
    label_currency_mx()(breaks)
  )
})


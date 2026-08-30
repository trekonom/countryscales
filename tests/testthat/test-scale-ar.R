get_scale_labels <- function(scale, breaks) {
  scale$train(range(breaks))
  scale$get_labels(breaks)
}

test_that("scale_x/y_number_ar label like label_number_ar", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_number_ar(), breaks),
    label_number_ar()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_number_ar(), breaks),
    label_number_ar()(breaks)
  )
})

test_that("scale_x/y_percent_ar label like label_percent_ar", {
  breaks <- c(.1, .5, .9)

  expect_equal(
    get_scale_labels(scale_x_percent_ar(), breaks),
    label_percent_ar()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_percent_ar(), breaks),
    label_percent_ar()(breaks)
  )
})

test_that("scale_x/y_currency_ar label like label_currency_ar", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_currency_ar(), breaks),
    label_currency_ar()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_currency_ar(), breaks),
    label_currency_ar()(breaks)
  )
})


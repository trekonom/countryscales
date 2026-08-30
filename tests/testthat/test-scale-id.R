get_scale_labels <- function(scale, breaks) {
  scale$train(range(breaks))
  scale$get_labels(breaks)
}

test_that("scale_x/y_number_id label like label_number_id", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_number_id(), breaks),
    label_number_id()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_number_id(), breaks),
    label_number_id()(breaks)
  )
})

test_that("scale_x/y_percent_id label like label_percent_id", {
  breaks <- c(.1, .5, .9)

  expect_equal(
    get_scale_labels(scale_x_percent_id(), breaks),
    label_percent_id()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_percent_id(), breaks),
    label_percent_id()(breaks)
  )
})

test_that("scale_x/y_currency_id label like label_currency_id", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_currency_id(), breaks),
    label_currency_id()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_currency_id(), breaks),
    label_currency_id()(breaks)
  )
})


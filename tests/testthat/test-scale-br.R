get_scale_labels <- function(scale, breaks) {
  scale$train(range(breaks))
  scale$get_labels(breaks)
}

test_that("scale_x/y_number_br label like label_number_br", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_number_br(), breaks),
    label_number_br()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_number_br(), breaks),
    label_number_br()(breaks)
  )
})

test_that("scale_x/y_percent_br label like label_percent_br", {
  breaks <- c(.1, .5, .9)

  expect_equal(
    get_scale_labels(scale_x_percent_br(), breaks),
    label_percent_br()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_percent_br(), breaks),
    label_percent_br()(breaks)
  )
})

test_that("scale_x/y_currency_br label like label_currency_br", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_currency_br(), breaks),
    label_currency_br()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_currency_br(), breaks),
    label_currency_br()(breaks)
  )
})


get_scale_labels <- function(scale, breaks) {
  scale$train(range(breaks))
  scale$get_labels(breaks)
}

test_that("scale_x/y_number_jp label like label_number_jp", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_number_jp(), breaks),
    label_number_jp()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_number_jp(), breaks),
    label_number_jp()(breaks)
  )
})

test_that("scale_x/y_percent_jp label like label_percent_jp", {
  breaks <- c(.1, .5, .9)

  expect_equal(
    get_scale_labels(scale_x_percent_jp(), breaks),
    label_percent_jp()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_percent_jp(), breaks),
    label_percent_jp()(breaks)
  )
})

test_that("scale_x/y_currency_jp label like label_currency_jp", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_currency_jp(), breaks),
    label_currency_jp()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_currency_jp(), breaks),
    label_currency_jp()(breaks)
  )
})


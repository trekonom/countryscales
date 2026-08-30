get_scale_labels <- function(scale, breaks) {
  scale$train(range(breaks))
  scale$get_labels(breaks)
}

test_that("scale_x/y_number_no label like label_number_no", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_number_no(), breaks),
    label_number_no()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_number_no(), breaks),
    label_number_no()(breaks)
  )
})

test_that("scale_x/y_percent_no label like label_percent_no", {
  breaks <- c(.1, .5, .9)

  expect_equal(
    get_scale_labels(scale_x_percent_no(), breaks),
    label_percent_no()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_percent_no(), breaks),
    label_percent_no()(breaks)
  )
})

test_that("scale_x/y_currency_no label like label_currency_no", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_currency_no(), breaks),
    label_currency_no()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_currency_no(), breaks),
    label_currency_no()(breaks)
  )
})


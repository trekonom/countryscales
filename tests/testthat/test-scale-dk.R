get_scale_labels <- function(scale, breaks) {
  scale$train(range(breaks))
  scale$get_labels(breaks)
}

test_that("scale_x/y_number_dk label like label_number_dk", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_number_dk(), breaks),
    label_number_dk()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_number_dk(), breaks),
    label_number_dk()(breaks)
  )
})

test_that("scale_x/y_percent_dk label like label_percent_dk", {
  breaks <- c(.1, .5, .9)

  expect_equal(
    get_scale_labels(scale_x_percent_dk(), breaks),
    label_percent_dk()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_percent_dk(), breaks),
    label_percent_dk()(breaks)
  )
})

test_that("scale_x/y_currency_dk label like label_currency_dk", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_currency_dk(), breaks),
    label_currency_dk()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_currency_dk(), breaks),
    label_currency_dk()(breaks)
  )
})


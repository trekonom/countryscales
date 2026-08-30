get_scale_labels <- function(scale, breaks) {
  scale$train(range(breaks))
  scale$get_labels(breaks)
}

test_that("scale_x/y_number_ca label like label_number_ca", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_number_ca(), breaks),
    label_number_ca()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_number_ca(), breaks),
    label_number_ca()(breaks)
  )
})

test_that("scale_x/y_percent_ca label like label_percent_ca", {
  breaks <- c(.1, .5, .9)

  expect_equal(
    get_scale_labels(scale_x_percent_ca(), breaks),
    label_percent_ca()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_percent_ca(), breaks),
    label_percent_ca()(breaks)
  )
})

test_that("scale_x/y_currency_ca label like label_currency_ca", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_currency_ca(), breaks),
    label_currency_ca()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_currency_ca(), breaks),
    label_currency_ca()(breaks)
  )
})


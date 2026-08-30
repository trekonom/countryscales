get_scale_labels <- function(scale, breaks) {
  scale$train(range(breaks))
  scale$get_labels(breaks)
}

test_that("scale_x/y_number_de label like label_number_de", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_number_de(), breaks),
    label_number_de()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_number_de(), breaks),
    label_number_de()(breaks)
  )
})

test_that("scale_x/y_percent_de label like label_percent_de", {
  breaks <- c(.1, .5, .9)

  expect_equal(
    get_scale_labels(scale_x_percent_de(), breaks),
    label_percent_de()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_percent_de(), breaks),
    label_percent_de()(breaks)
  )
})

test_that("scale_x/y_currency_de label like label_currency_de", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_currency_de(), breaks),
    label_currency_de()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_currency_de(), breaks),
    label_currency_de()(breaks)
  )
})

get_scale_labels <- function(scale, breaks) {
  scale$train(range(breaks))
  scale$get_labels(breaks)
}

test_that("scale_x/y_number_za label like label_number_za", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_number_za(), breaks),
    label_number_za()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_number_za(), breaks),
    label_number_za()(breaks)
  )
})

test_that("scale_x/y_percent_za label like label_percent_za", {
  breaks <- c(.1, .5, .9)

  expect_equal(
    get_scale_labels(scale_x_percent_za(), breaks),
    label_percent_za()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_percent_za(), breaks),
    label_percent_za()(breaks)
  )
})

test_that("scale_x/y_currency_za label like label_currency_za", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_currency_za(), breaks),
    label_currency_za()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_currency_za(), breaks),
    label_currency_za()(breaks)
  )
})


get_scale_labels <- function(scale, breaks) {
  scale$train(range(breaks))
  scale$get_labels(breaks)
}

test_that("scale_x/y_number_it label like label_number_it", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_number_it(), breaks),
    label_number_it()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_number_it(), breaks),
    label_number_it()(breaks)
  )
})

test_that("scale_x/y_percent_it label like label_percent_it", {
  breaks <- c(.1, .5, .9)

  expect_equal(
    get_scale_labels(scale_x_percent_it(), breaks),
    label_percent_it()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_percent_it(), breaks),
    label_percent_it()(breaks)
  )
})

test_that("scale_x/y_currency_it label like label_currency_it", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_currency_it(), breaks),
    label_currency_it()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_currency_it(), breaks),
    label_currency_it()(breaks)
  )
})


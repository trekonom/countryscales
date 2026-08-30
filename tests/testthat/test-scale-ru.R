get_scale_labels <- function(scale, breaks) {
  scale$train(range(breaks))
  scale$get_labels(breaks)
}

test_that("scale_x/y_number_ru label like label_number_ru", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_number_ru(), breaks),
    label_number_ru()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_number_ru(), breaks),
    label_number_ru()(breaks)
  )
})

test_that("scale_x/y_percent_ru label like label_percent_ru", {
  breaks <- c(.1, .5, .9)

  expect_equal(
    get_scale_labels(scale_x_percent_ru(), breaks),
    label_percent_ru()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_percent_ru(), breaks),
    label_percent_ru()(breaks)
  )
})

test_that("scale_x/y_currency_ru label like label_currency_ru", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_currency_ru(), breaks),
    label_currency_ru()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_currency_ru(), breaks),
    label_currency_ru()(breaks)
  )
})


get_scale_labels <- function(scale, breaks) {
  scale$train(range(breaks))
  scale$get_labels(breaks)
}

test_that("scale_x/y_number_es label like label_number_es", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_number_es(), breaks),
    label_number_es()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_number_es(), breaks),
    label_number_es()(breaks)
  )
})

test_that("scale_x/y_percent_es label like label_percent_es", {
  breaks <- c(.1, .5, .9)

  expect_equal(
    get_scale_labels(scale_x_percent_es(), breaks),
    label_percent_es()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_percent_es(), breaks),
    label_percent_es()(breaks)
  )
})

test_that("scale_x/y_currency_es label like label_currency_es", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_currency_es(), breaks),
    label_currency_es()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_currency_es(), breaks),
    label_currency_es()(breaks)
  )
})


get_scale_labels <- function(scale, breaks) {
  scale$train(range(breaks))
  scale$get_labels(breaks)
}

test_that("scale_x/y_number_tr label like label_number_tr", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_number_tr(), breaks),
    label_number_tr()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_number_tr(), breaks),
    label_number_tr()(breaks)
  )
})

test_that("scale_x/y_percent_tr label like label_percent_tr", {
  breaks <- c(.1, .5, .9)

  expect_equal(
    get_scale_labels(scale_x_percent_tr(), breaks),
    label_percent_tr()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_percent_tr(), breaks),
    label_percent_tr()(breaks)
  )
})

test_that("scale_x/y_currency_tr label like label_currency_tr", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_currency_tr(), breaks),
    label_currency_tr()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_currency_tr(), breaks),
    label_currency_tr()(breaks)
  )
})


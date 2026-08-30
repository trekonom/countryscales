get_scale_labels <- function(scale, breaks) {
  scale$train(range(breaks))
  scale$get_labels(breaks)
}

test_that("scale_x/y_number_fi label like label_number_fi", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_number_fi(), breaks),
    label_number_fi()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_number_fi(), breaks),
    label_number_fi()(breaks)
  )
})

test_that("scale_x/y_percent_fi label like label_percent_fi", {
  breaks <- c(.1, .5, .9)

  expect_equal(
    get_scale_labels(scale_x_percent_fi(), breaks),
    label_percent_fi()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_percent_fi(), breaks),
    label_percent_fi()(breaks)
  )
})

test_that("scale_x/y_currency_fi label like label_currency_fi", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_currency_fi(), breaks),
    label_currency_fi()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_currency_fi(), breaks),
    label_currency_fi()(breaks)
  )
})


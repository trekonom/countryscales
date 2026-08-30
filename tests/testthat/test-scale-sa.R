get_scale_labels <- function(scale, breaks) {
  scale$train(range(breaks))
  scale$get_labels(breaks)
}

test_that("scale_x/y_number_sa label like label_number_sa", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_number_sa(), breaks),
    label_number_sa()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_number_sa(), breaks),
    label_number_sa()(breaks)
  )
})

test_that("scale_x/y_percent_sa label like label_percent_sa", {
  breaks <- c(.1, .5, .9)

  expect_equal(
    get_scale_labels(scale_x_percent_sa(), breaks),
    label_percent_sa()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_percent_sa(), breaks),
    label_percent_sa()(breaks)
  )
})

test_that("scale_x/y_currency_sa label like label_currency_sa", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_currency_sa(), breaks),
    label_currency_sa()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_currency_sa(), breaks),
    label_currency_sa()(breaks)
  )
})


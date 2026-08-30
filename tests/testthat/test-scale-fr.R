get_scale_labels <- function(scale, breaks) {
  scale$train(range(breaks))
  scale$get_labels(breaks)
}

test_that("scale_x/y_number_fr label like label_number_fr", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_number_fr(), breaks),
    label_number_fr()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_number_fr(), breaks),
    label_number_fr()(breaks)
  )
})

test_that("scale_x/y_percent_fr label like label_percent_fr", {
  breaks <- c(.1, .5, .9)

  expect_equal(
    get_scale_labels(scale_x_percent_fr(), breaks),
    label_percent_fr()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_percent_fr(), breaks),
    label_percent_fr()(breaks)
  )
})

test_that("scale_x/y_currency_fr label like label_currency_fr", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_currency_fr(), breaks),
    label_currency_fr()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_currency_fr(), breaks),
    label_currency_fr()(breaks)
  )
})


get_scale_labels <- function(scale, breaks) {
  scale$train(range(breaks))
  scale$get_labels(breaks)
}

test_that("scale_x/y_number_pt label like label_number_pt", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_number_pt(), breaks),
    label_number_pt()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_number_pt(), breaks),
    label_number_pt()(breaks)
  )
})

test_that("scale_x/y_percent_pt label like label_percent_pt", {
  breaks <- c(.1, .5, .9)

  expect_equal(
    get_scale_labels(scale_x_percent_pt(), breaks),
    label_percent_pt()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_percent_pt(), breaks),
    label_percent_pt()(breaks)
  )
})

test_that("scale_x/y_currency_pt label like label_currency_pt", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_currency_pt(), breaks),
    label_currency_pt()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_currency_pt(), breaks),
    label_currency_pt()(breaks)
  )
})


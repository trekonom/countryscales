get_scale_labels <- function(scale, breaks) {
  scale$train(range(breaks))
  scale$get_labels(breaks)
}

test_that("scale_x/y_number_se label like label_number_se", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_number_se(), breaks),
    label_number_se()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_number_se(), breaks),
    label_number_se()(breaks)
  )
})

test_that("scale_x/y_percent_se label like label_percent_se", {
  breaks <- c(.1, .5, .9)

  expect_equal(
    get_scale_labels(scale_x_percent_se(), breaks),
    label_percent_se()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_percent_se(), breaks),
    label_percent_se()(breaks)
  )
})

test_that("scale_x/y_currency_se label like label_currency_se", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_currency_se(), breaks),
    label_currency_se()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_currency_se(), breaks),
    label_currency_se()(breaks)
  )
})


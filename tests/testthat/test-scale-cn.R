get_scale_labels <- function(scale, breaks) {
  scale$train(range(breaks))
  scale$get_labels(breaks)
}

test_that("scale_x/y_number_cn label like label_number_cn", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_number_cn(), breaks),
    label_number_cn()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_number_cn(), breaks),
    label_number_cn()(breaks)
  )
})

test_that("scale_x/y_percent_cn label like label_percent_cn", {
  breaks <- c(.1, .5, .9)

  expect_equal(
    get_scale_labels(scale_x_percent_cn(), breaks),
    label_percent_cn()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_percent_cn(), breaks),
    label_percent_cn()(breaks)
  )
})

test_that("scale_x/y_currency_cn label like label_currency_cn", {
  breaks <- c(1000, 2000, 3000)

  expect_equal(
    get_scale_labels(scale_x_currency_cn(), breaks),
    label_currency_cn()(breaks)
  )
  expect_equal(
    get_scale_labels(scale_y_currency_cn(), breaks),
    label_currency_cn()(breaks)
  )
})


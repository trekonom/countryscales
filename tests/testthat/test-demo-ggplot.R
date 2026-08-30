test_that("demo_number builds a ggplot without erroring", {
  p <- demo_number(c(-1e6, 1e6), scale_name = "number_locale", locale = "fr-FR")

  expect_s3_class(p, "ggplot")
})

test_that("demo_number works for percent and currency scales", {
  expect_s3_class(
    demo_number(c(-1, 1), scale_name = "percent_de"),
    "ggplot"
  )
  expect_s3_class(
    demo_number(c(-1e4, 1e4), scale_name = "currency_locale", locale = "ja-JP", currency = "JPY"),
    "ggplot"
  )
})

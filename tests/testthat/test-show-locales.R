test_that("show_locales returns a dataframe of locale codes", {
  x <- show_locales()

  expect_s3_class(x, "data.frame")
  expect_named(x, "locale")
  expect_gt(nrow(x), 0)
  expect_false(any(grepl("_", x$locale)))
})

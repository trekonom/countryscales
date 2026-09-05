# Correctness of label_number_locale() against Intl.js is covered by
# test-label-locale.R. These tests cover branches of the internal, vendored
# number()/label_number() (R/label-number.R) that no supported locale's data
# ever selects -- see countryscales::locales$style_positive (always "none")
# and label_number_locale()/label_percent_locale() (always hardcode
# style_negative = "custom") -- but that the functions still need to support
# correctly, since they're a general reimplementation of scales::label_number().

test_that("label_number_locale handles an empty vector", {
  expect_equal(label_number_locale()(numeric(0)), character())
})

test_that("number handles an empty vector", {
  expect_equal(number(numeric(0)), character())
})

test_that("label_number_locale forwards scale_cut", {
  # regression test: scale_cut previously always errored with "could not
  # find function scale_cut" -- the scale_cut argument shadowed the
  # (unexported) scales:::scale_cut helper the code was trying to call
  expect_equal(
    label_number_locale(scale_cut = scales::cut_short_scale())(1e6),
    "1M"
  )
})

test_that("precision auto-detects accuracy for a single value", {
  expect_equal(label_number_locale()(5), "5")
})

test_that("precision auto-detects accuracy for all-duplicate values", {
  expect_equal(label_number_locale()(c(5, 5, 5)), rep("5", 3))
})

test_that("precision treats near-duplicate values as a single accuracy bucket", {
  expect_equal(precision(c(1, 1 + 1e-13)), 1)
})

test_that("label_number_locale's scale_cut corrects an off-boundary break", {
  # 999900 sits in the "K" bucket but doesn't divide evenly by it (999.9);
  # exercises apply_scale_cut()'s bad_break correction path
  expect_equal(
    label_number_locale(scale_cut = scales::cut_short_scale())(999900),
    "1,000K"
  )
})

test_that("number supports style_negative alternatives", {
  expect_equal(number(-5, style_negative = "hyphen"), "-5")
  expect_equal(number(-5, style_negative = "minus"), "−5")
  expect_equal(number(-5, style_negative = "parens"), "(5)")
})

test_that("number supports style_positive alternatives", {
  expect_equal(number(5, style_positive = "plus"), "+5")
  expect_equal(number(5, style_positive = "space"), " 5")
  expect_equal(
    number(5, style_positive = "custom", custom_positive = "~"),
    "~5"
  )
})

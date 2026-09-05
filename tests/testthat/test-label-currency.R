# Correctness of label_currency_locale() against Intl.js is covered by
# test-label-locale.R. These tests cover branches of the internal, vendored
# currency()/label_currency() (R/label-currency.R) that no supported locale's
# data ever selects -- see countryscales::locales$p_sign_posn (always 1) and
# the fact that n_sign_posn is only ever 2/3/4 when n_cs_precedes is TRUE
# (never combined with n_cs_precedes = FALSE) -- but that the functions still
# need to support correctly, since they're a general reimplementation of
# scales::label_currency() covering the full ICU/POSIX sign-position spec.

test_that("label_currency_locale handles an empty vector", {
  expect_equal(label_currency_locale()(numeric(0)), character())
})

test_that("currency handles an empty vector", {
  expect_equal(currency(numeric(0)), character())
})

test_that("label_currency_locale forwards scale_cut", {
  # regression test: scale_cut previously always errored with "could not
  # find function scale_cut" -- the scale_cut argument shadowed the
  # (unexported) scales:::scale_cut helper the code was trying to call
  expect_equal(
    label_currency_locale(scale_cut = scales::cut_short_scale())(1e6),
    "$1M"
  )
  expect_equal(
    label_currency_locale(scale_cut = scales::cut_short_scale())(-1e6),
    "-$1M"
  )
  expect_equal(
    label_currency_locale(scale_cut = scales::cut_short_scale())(500),
    "$500"
  )
})

test_that("currency supports sign position 1 (sign immediately before value)", {
  expect_equal(
    currency(5, currency = "$", p_cs_precedes = TRUE, p_sign = "+", p_sign_posn = 1),
    "+$5"
  )
  expect_equal(
    currency(-5, currency = "$", n_cs_precedes = TRUE, n_sign = "-", n_sign_posn = 1),
    "-$5"
  )
})

test_that("currency supports sign position 2 (sign immediately after value)", {
  expect_equal(
    currency(5, currency = "$", p_cs_precedes = FALSE, p_sign = "+", p_sign_posn = 2),
    "5$+"
  )
  expect_equal(
    currency(-5, currency = "$", n_cs_precedes = FALSE, n_sign = "-", n_sign_posn = 2),
    "5$-"
  )
})

test_that("currency supports sign position 3 (sign before currency symbol)", {
  expect_equal(
    currency(5, currency = "$", p_cs_precedes = TRUE, p_sign = "+", p_sign_posn = 3),
    "+$5"
  )
  expect_equal(
    currency(5, currency = "$", p_cs_precedes = FALSE, p_sign = "+", p_sign_posn = 3),
    "5+$"
  )
  expect_equal(
    currency(-5, currency = "$", n_cs_precedes = FALSE, n_sign = "-", n_sign_posn = 3),
    "5-$"
  )
})

test_that("currency supports sign position 4 (sign after currency symbol)", {
  expect_equal(
    currency(5, currency = "$", p_cs_precedes = TRUE, p_sign = "+", p_sign_posn = 4),
    "$+5"
  )
  expect_equal(
    currency(5, currency = "$", p_cs_precedes = FALSE, p_sign = "+", p_sign_posn = 4),
    "5$+"
  )
  expect_equal(
    currency(-5, currency = "$", n_cs_precedes = FALSE, n_sign = "-", n_sign_posn = 4),
    "5$-"
  )
})

test_that("currency does not duplicate a positive sign when p_sign is set", {
  # regression test: currency() used to unconditionally re-prepend p_sign to
  # every positive value after already placing it via p_sign_posn, which
  # would have doubled the sign for any locale with a non-empty p_sign (none
  # currently do, so this was invisible in practice)
  expect_equal(
    currency(5, currency = "$", p_cs_precedes = TRUE, p_sign = "+", p_sign_posn = 1),
    "+$5"
  )
})

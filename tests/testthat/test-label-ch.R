test_that("uses a ’ as big.mark and a . as decimal mark", {
  expect_equal(label_number_ch()(1200.243), "1\u2019200")
  expect_equal(label_number_ch(accuracy = .1)(1.243), "1.2")
})

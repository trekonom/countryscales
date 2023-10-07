test_that("uses a . as big.mark and a , as decimal mark", {
  expect_equal(label_number_de()(1200.243), "1.200")
  expect_equal(label_number_de(accuracy = .1)(1.243), "1,2")
  expect_equal(label_percent_de()(.243), "24 %")
  expect_equal(label_percent_de(accuracy = .1)(.243), "24,3 %")
})

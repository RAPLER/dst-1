context("Conversion of data")
library(dst)
test_that("encode", {
  # convert decimal number to base
  result <- encode(c(2,2,2,2), 11) 
  expect_equal(result, c(1,0,1,1))
  # convert minutes to days-hrs-min.
  result <- encode(c(365,24,60), 2997)
  expect_equal(result, c(2, 1, 57))
})
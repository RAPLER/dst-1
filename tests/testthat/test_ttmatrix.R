context("Construct a description matrix from a list of subsets names.")
library(dst)
test_that("ttmatrix", {
# T1. input not a list
  x1 <- c("a","b")
  expect_error(ttmatrix(x1), "Input must be a list.")
  })

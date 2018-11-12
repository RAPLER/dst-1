# Tests "matrixToMarray" function
context("Matrix transformation")
library(dst)
test_that("matrixToMarray", {
  # T1 x must be of class bcaspec. 
  x <- list(a=1:3, b="foo")
  expect_error(matrixToMarray(x), "Input argument not of class bcaspec." )
} )
  
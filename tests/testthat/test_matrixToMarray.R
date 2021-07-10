# Tests "matrixToMarray" function
context("Matrix transformation")
library(dst)
test_that("matrixToMarray", {
  # T1 Product of the size of variables  obtained within the valuenames parameter must be equal to the number of columns of tt matrix
  x <- matrix(1:12, ncol=4)
  vars <- list(a=c("A1", "A2", "A3"), b=c("B1", "b2") )
  expect_error(matrixToMarray(tt=x, valuenames = vars), "Product of size of variables not equal to number of columns of tt matrix." )
} )
  
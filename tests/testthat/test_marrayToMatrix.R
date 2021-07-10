# Tests "marrayToMatrix" function
context("Array transformation")
library(dst)
test_that("marrayToMatrix", {
  # T1 Nb of vars of x must be an array of at least 3 variables. 
  x <- 1:3
  expect_error(marrayToMatrix(x), "Input is not an array or a matrix." )

} )

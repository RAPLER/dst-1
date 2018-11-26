# Tests "addTobca" function
context("add a specification")
library(dst)
test_that("addTobca", {
  # T1 x must be a bcaspec 
  x1 <- list(a=1:3, b="foo")
  f1 <- matrix(c(0,1,0,0,0,1, 0,1,1), nrow=3, byrow = TRUE)
  expect_error(addTobca(x = x1, f = f1) , "Input x not of class bcaspec.")
  #
  # T2 f must be a (0,1) or logical matrix
  x2 <- bca(f=matrix(c(1,0,0,1,1,1),nrow=2, byrow = TRUE), m=c(0.6, 0.4),  cnames = c("a", "b", "c"), varnb=1)
  f2 <- c(1,0,1)
  expect_error(addTobca(x = x2, f = f2) , "f parameter must be a \\(0,1\\) or logical matrix.")
  #
  # T3 Nb of columns of f must equal nb of columns of tt matrix of x
  f3 <- matrix(1:12, ncol=4)
  expect_error(addTobca(x = x2, f = f3) , "Error in input arguments: number of columns of f not equal to ncol\\(x\\$tt\\)")
})
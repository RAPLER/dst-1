# Tests "addToDSM" function
context("add a specification")
library(dst)
test_that("addToDSM", {
  # T1 x must be a DSMspec 
  x1 <- list(a=1:3, b="foo")
  f1 <- matrix(c(0,1,0,0,0,1, 0,1,1), nrow=3, byrow = TRUE)
  expect_error(addToDSM(x = x1, tt = f1) , "Input x not of class DSMspec.")
  #
  # T2 tt must be a (0,1) or logical matrix
  x2 <- DSM(tt = matrix(c(1,0,0,1,1,1),nrow=2, byrow = TRUE), m=c(0.6, 0.4),  cnames = c("a", "b", "c"), idvar=1)
  f2 <- c(1,0,1)
  expect_error(addToDSM(x = x2, tt = f2) , "tt parameter must be a \\(0,1\\) or logical matrix.")
  #
  # T3 Nb of columns of tt must equal nb of columns of tt matrix of x
  f3 <- matrix(1:12, ncol=4)
  expect_error(addToDSM(x = x2, tt = f3) , "Error in input arguments: number of columns of tt not equal to ncol\\(x\\$tt\\)")
})
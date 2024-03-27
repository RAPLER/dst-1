# Tests "extFrame" function
context("Extend the frame of a variable")
library(dst)
test_that("extFrame", {
  # T1 x must be a bcaspec 
  x1 <- list(a=1:3, b="foo")
  f1 <- matrix(c(0,1,0,0,0,1, 0,1,1), nrow=3, byrow = TRUE)
  expect_error(extFrame(x = x1, lab = "f1") , "Input x not of class bcaspec.")
  #
  # T2: lab parameter must be a character string
  x2 <- bca(tt = matrix(c(0,1,0,0, 0,0,1,1,1,1,0,0,1,0,1,0,0,1,1,0,1,1,1,1),ncol=4, byrow = TRUE), m = c(0.2, 0.5, 0.06, 0.04, 0.03, 0.17), cnames = c("a", "b", "c", "d"))
  expect_error(extFrame(x=x2, lab = 1 ), "parameter lab must be a character string")
  #
  # T3: Parammeter lab must not contain existing values of the frame
  x2 <- bca(tt = matrix(c(0,1,0,0, 0,0,1,1,1,1,0,0,1,0,1,0,0,1,1,0,1,1,1,1),ncol=4, byrow = TRUE), m = c(0.2, 0.5, 0.06, 0.04, 0.03, 0.17), cnames = c("a", "b", "c", "d"))
  expect_error(extFrame(x=x2, lab = c("c", "e", "f") ), "One on more new labels already in the frame. Labels cannot be reppeated.")
  #
 
  #
  
  })
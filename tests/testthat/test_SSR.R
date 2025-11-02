# Tests "SSR" function
context("define a SSR")
library(dst)
test_that("SSR", {
  # T1: size != length(cnames)
  size = 3
  cnames <- c("yes","no")
  expect_error(SSR( "x1", 1, size, cnames) ,"Error in input arguments: number of labels supplied not equal to size of the state space.")
  #
  #
})
context("Remove duplicate lines from a matrix")
library(dst)
test_that("doubles", {
  # T1: input must be a matrix
  x <- c(1,1,3)
  expect_error(doubles(c(1,1,3)), "Input is not a matrix.")
  # T2: check a matrix of only 1 row. Result must be matrix
  x <- matrix(c(1,1,3), ncol = 3)
  result <- doubles(x)
  expect_is(result, class = "matrix")
  # T3: check (0,1) matrix
  x <-matrix(c(rep(c(1,0,1),times=3),0,0,1,1,1,1, 1,1,1),ncol=3,byrow=TRUE)
  result <- doubles(x)
  expect_equal(result, matrix(c(1,0,1,0,0,1,1,1,1), ncol = 3, byrow = TRUE))
  # T4: check character matrix
  x=matrix(c("d","e","f", rep(c("a","b","cc"),times=3),"g","h","i"),nrow=5,byrow=TRUE)
  result <- doubles(x)
  expect_equal(result, matrix(c("d","e","f","a","b","cc","g","h","i" ), ncol = 3, byrow = TRUE))
})
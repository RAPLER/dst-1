context("Dot product calculation")
library(dst)
test_that("dotprod", {
  # Standard matrix product
  x <- y <- matrix(c(1:6), nrow = 2, byrow = TRUE)
  result <- dotprod(x, t(y), g = "+", f = "*")
  expect_equal(result, matrix(c(14,32,32,77), ncol = 2))
  #  check vector equality
  x2 <- matrix(c(1,0,0,1,1,1), nrow = 2, byrow = TRUE)
  y2 <- matrix(c(1,0,0,0,1,0,1,1,0,0,1,1,1,1,1), nrow = 5, byrow = TRUE)
  result <- dotprod(x2, t(y2), g = "&", f = "==")
  expect_equal(result, matrix(c(TRUE, rep(FALSE,4), rep(FALSE,4), TRUE ), ncol = 5,  byrow = TRUE))
  # Check a comparison operator
  x <- matrix(4:9, ncol=3, byrow = TRUE)
  y <- matrix(1:9, ncol=3, byrow=TRUE)
  result <- dotprod(x,t(y),g="&",f=">")
  expect_equal(result, matrix(c(TRUE, rep(FALSE,2), rep(TRUE,2), FALSE ), ncol = 3, byrow = TRUE))
  # boolean comparisons
  x <- matrix(c(1,0,0,1,1,0), ncol=3, byrow = TRUE)
  y <- matrix(c(1,0,0,1,1,0, 0,1,1), ncol=3, byrow = TRUE)
  result <- dotprod(x,t(y),g="|",f="&")
  expect_equal(result, matrix(c(rep(TRUE,2), FALSE , rep(TRUE,3) ), ncol = 3, byrow = TRUE))
})
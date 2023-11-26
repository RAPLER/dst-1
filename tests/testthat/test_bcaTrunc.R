# Tests "bcaTrunc" function
context("truncate a bca")
library(dst)
test_that("bcaTrunc", {
  # 
  # T1 x must be of class bcaspec. 
  # 
  x1 <- list(a=1:3, b="foo")
  expect_error(bcaTrunc(x = x1, seuil = 0.1) ,"Input not of class bcaspec.")
  #
  #
  # T2: seuil must be numeric
  x2 <- bca(tt = matrix(c(0,1,0,0, 0,0,1,1,1,1,0,0,1,0,1,0,0,1,1,0,1,1,1,1),ncol=4, byrow = TRUE), m = c(0.2, 0.5, 0.06, 0.04, 0.03, 0.17), cnames = c("a", "b", "c", "d"))
  expect_error(bcaTrunc(x=x2,  seuil=,1 ), "Treshold must be a numeric value between 0 and 1.")
  #
  # T3: seuil must be > 0
  x3 <- bca(tt = matrix(c(0,1,0,0, 0,0,1,1,1,1,0,0,1,0,1,0,0,1,1,0,1,1,1,1),ncol=4, byrow = TRUE), m = c(0.2, 0.5, 0.06, 0.04, 0.03, 0.17), cnames = c("a", "b", "c", "d"))
  expect_error(bcaTrunc(x=x3,  seuil= -.1 ), "Treshold must be a numeric value between 0 and 1.")
  #
  # T4: seuil must be < 1
  x3 <- bca(tt = matrix(c(0,1,0,0, 0,0,1,1,1,1,0,0,1,0,1,0,0,1,1,0,1,1,1,1),ncol=4, byrow = TRUE), m = c(0.2, 0.5, 0.06, 0.04, 0.03, 0.17), cnames = c("a", "b", "c", "d"))
  expect_error(bcaTrunc(x=x3,  seuil= 2 ), "Treshold must be a numeric value between 0 and 1.")
  #
})

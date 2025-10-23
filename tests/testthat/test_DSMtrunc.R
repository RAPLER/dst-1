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
  # T5: truncate masses <= 0.06, not using ssnames
  x1 <- bca(tt = matrix(c(0,1,0,0,0,0,1,1,1,1,0,0,1,0,1,0,0,1,1,0,1,1,1,1),ncol=4, byrow = TRUE), m = c(0.2, 0.5, 0.06, 0.04, 0.03, 0.17),cnames = c("a", "b", "c", "d"))
  tr_x1 <- bcaTrunc(x1, seuil = 0.06)
  expect_equal(tr_x1$spec[3,2],0.04+0.06+0.03)
  # T6: truncate masses <= 0.06, using ssnames
  tr_x2 <- bcaTrunc(x1, seuil = 0.06, use_ssnames=TRUE)
  expect_equal(as.numeric(tr_x2$spec[3,2]),0.04+0.06+0.03)
  # T7: truncate masses <= 0.06, using ssnames, with first element length >= 2
  x2 <- bca(tt = matrix(c(0,0,1,1,1,1,0,0,1,0,1,0,0,1,1,0,1,1,1,1),ncol=4, byrow = TRUE), m = c(0.7, 0.06, 0.04, 0.03, 0.17),cnames = c("a", "b", "c", "d"))
  tr_x2 <- bcaTrunc(x2, seuil = 0.06, use_ssnames=TRUE)
  expect_equal(as.numeric(tr_x2$spec[2,2]),0.04+0.06+0.03)
  # T8: truncate masses <= 0.06, using ssnames, with first element "Emptyset", doesn't affect masses on "Emptyset"
  x3 <- bca(tt = matrix(c(0,0,0,0,0,0,1,1,1,1,0,0,1,0,1,0,0,1,1,0,1,1,1,1),ncol=4, byrow = TRUE), m = c(0.2, 0.5, 0.06, 0.04, 0.03, 0.17),cnames = c("a", "b", "c", "d"))
  tr_x3 <- bcaTrunc(x3, seuil = 0.06, use_ssnames=TRUE)
  expect_equal(as.numeric(tr_x3$spec[1,2]),0.2)
})

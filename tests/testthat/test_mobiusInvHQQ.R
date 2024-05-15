# Tests "mobiusInv" function
context("Compute tt matrix from qq function")
library(dst)
test_that("mobiusInvHQQ", {
  # Check computation with qq is the same as tt, ssnams
  # by performing Mobius inversion on the combined bca to get the tt matrix
  x <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, 
                       byrow = TRUE), m = c(0.2,0.5, 0.3), 
           cnames = c("a", "b", "c"), varnames = "x", idvar = 1)
  y <- bca(tt = matrix(c(1,0,0,1,1,1),nrow = 2, 
                       byrow = TRUE), m = c(0.6, 0.4),  
           cnames = c("a", "b", "c"),  varnames = "y", idvar = 1)
  z <- dsrwon(x, y)
  w <- dsrwon(x, y, use_qq = TRUE)
  expect_equal(as.numeric(z$spec[1,2]), mobiusInvHQQ(w$qq, c(0,0,0)))
  expect_equal(as.numeric(z$spec[2,2]), mobiusInvHQQ(w$qq, c(1,0,0)))
  expect_equal(0, mobiusInvHQQ(w$qq, c(0,1,0)))
  expect_equal(0, mobiusInvHQQ(w$qq, c(0,1,0)))
  expect_equal(as.numeric(z$spec[3,2]), mobiusInvHQQ(w$qq, c(0,1,1)))
  expect_equal(as.numeric(z$spec[4,2]), mobiusInvHQQ(w$qq, c(1,1,0)))
  expect_equal(0, mobiusInvHQQ(w$qq, c(1,0,1)))
  # the last element is the same as commonality on the whole frame
  expect_equal(as.numeric(z$spec[5,2]), mobiusInvHQQ(w$qq, c(1,1,1)))
})

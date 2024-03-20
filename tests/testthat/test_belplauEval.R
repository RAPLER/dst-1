# Tests "belplauEval" function
context("evaluate a belplau object")
library(dst)
test_that("belplauEval", {
  x <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, byrow = TRUE), m = c(0.2,0.5, 0.3), cnames = c("a", "b", "c"), varnames = "x", idvar = 1)
  belplau(x)
  y <- bca(tt = matrix(c(1,0,0,1,1,1),nrow = 2, byrow = TRUE), m = c(0.6, 0.4), cnames = c("a", "b", "c"),  varnames = "y", idvar = 1)
  xy <- nzdsr(dsrwon(x,y))
  z<-belplau(xy,h=ttmatrixPartition(3,3))
  expect_equal(belplauEval(z,c(0,1,0)),0.5)
})

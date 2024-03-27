# Tests "belplauHLogsumexp" function
context("Calculate belief, disbelief, unkown, plausibility, plausibility ratio")
library(dst)
test_that("belplauHLogsumexp",{
  ## T1 test hypotheses 
  x1 <- bca(matrix(c(1,1,0,1,1,1), nrow = 2, byrow = TRUE), c(0.8, 0.2), c(1,2,3))
  y1 <- belplauHLogsumexp(x1$spec[,2],x1$tt,matrix(c(1,1,0,1,1,1), nrow=2, byrow = TRUE))
  y2 <- belplauH(x1$spec[,2],x1$tt,matrix(c(1,1,0,1,1,1), nrow=2, byrow = TRUE))
  expect_equal(y1,y2)
})

# Tests "DSAh" function
context("Calculate belief, disbelief, unkown, plausibility, plausibility ratio")
library(dst)
test_that("DSAh",{
  ## T1 test hypotheses 
  x1 <- DSM(tt=matrix(c(1,1,0,1,1,1), nrow = 2, byrow = TRUE), m = c(0.8, 0.2), cnames =  c(1,2,3))
  y <- DSAh(x1$spec[,2],x1$tt,matrix(c(1,1,0,1,1,1), nrow=2, byrow = TRUE))
  expect_equal(unname(x1$spec[1,1]), unname(y[2,1]))
})

# Tests "dsrwonLogsumexp" function
context("Compute unnormalized Dempster's Rule with logsumexp")
library(dst)
test_that("dsrwonLogsumexp", {
  x <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, 
                       byrow = TRUE), m = c(0.2,0.5, 0.3), 
           cnames = c("a", "b", "c"), varnames = "x", idvar = 1)
  y <- bca(tt = matrix(c(1,0,0,1,1,1),nrow = 2, 
                       byrow = TRUE), m = c(0.6, 0.4),  
           cnames = c("a", "b", "c"),  varnames = "y", idvar = 1)
  expect_equal(dsrwon(x,y),dsrwonLogsumpexp(x,y))
})

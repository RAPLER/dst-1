# Tests "nzdsrLogsumexp" function
context("Normalization of a belief function with logsumexp")
library(dst)
test_that("nzdsrLogsumexp", {
  x1 <- bca(tt= matrix(c(1,0,1,1),nrow = 2, byrow = TRUE), 
  m = c(0.9,0.1), cnames = c("yes", "no"),
  varnames = "x", idvar = 1)
  x2 <- bca(tt = matrix(c(0,1,1,1),nrow = 2, byrow = TRUE), 
  m = c(0.5,0.5), cnames = c("yes", "no"), 
  varnames = "x", idvar = 1)
  print("combination of x1 and x2")
  x1x2 <- dsrwon(x1,x2, varname = "x")
  expect_equal(nzdsr(x1x2), nzdsrLogsumexp(x1x2) )
})

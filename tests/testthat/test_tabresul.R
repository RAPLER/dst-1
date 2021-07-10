# Tests "tabresul" function
context("compute a table of results")
library(dst)
test_that("tabresul", {
  # T1 x and y must be of class bcaspec. 
  x1 <- list(f=matrix(c(1,0,0,1,1,1),nrow=2, byrow = TRUE), m=c(0.6, 0.4),  cnames = c("a", "b", "c"),  infovarnames = "x1", varnb = 1)
  expect_error(tabresul(x1) , "Input argument not of class bcaspec.")
  ##
  # T2 check that the input is a normalized bca
  x2 <- bca(tt = matrix(c(0,0,0,0,1,1,1,1,0,1,1,1),nrow=4, byrow = TRUE), m=c(0.2,0.1,0.4, 0.3), cnames =c("a", "b", "c"), infovarnames = "x2", varnb = 1)
  expect_error(tabresul(x2) ,"Invalid data: Empty set among the focal elements. Normalization necessary. See nzdsr function.")
  ##
  # T3 test that m_empty is null, if present
  x3 <- bca(tt = matrix(c(0,0,0,0,1,1,1,1,0,0,1,0,1,1,1),nrow=5, byrow = TRUE), m=c(0,0.1,0.4,0.1, 0.4), cnames =c("a", "b", "c"), infovarnames = "x", varnb = 1)
  result <- tabresul(x3)
  expect_equal(nrow(result$mbp), nrow(x3$tt))  #  nb of rows of result must match nb of rows of input table x3$tt
})
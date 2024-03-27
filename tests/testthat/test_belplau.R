# Tests "belplau" function
context("Compute belief and plausibility measures")
library(dst)
test_that("belplau", {
  # T1 x and y must be of class bcaspec. 
  x1 <- list(f=matrix(c(1,0,0,1,1,1),nrow=2, byrow = TRUE), m=c(0.6, 0.4),  cnames = c("a", "b", "c"),  varnames = "y1", idvar = 1)
  expect_error(belplau(x1) , "Input argument not of class bcaspec.")
  ##
  # T2 check that the input is a normalized bca
  x2 <- bca(tt = matrix(c(0,0,0,0,1,1,1,1,0,1,1,1),nrow=4, byrow = TRUE), m=c(0.2,0.1,0.4, 0.3), cnames =c("a", "b", "c"), varnames = "x", idvar = 1)
  expect_error(belplau(x2) , "Invalid data: Empty set among the focal elements. Normalization necessary. Apply function nzdsr to your bca to normalize your result.")
  ##
  # T3 test that m_empty is null, if present
  x3 <- bca(tt = matrix(c(0,0,0,0,1,1,1,1,0,0,1,0,1,1,1),nrow=5, byrow = TRUE), m=c(0,0.1,0.4,0.1, 0.4), cnames =c("a", "b", "c"), varnames = "x", idvar = 1)
  result <- belplau(x3)
  expect_equal(nrow(result), nrow(x3$tt))  #  nb of rows of result must match nb of rows of input table x3$tt
  ##
  # T4 check the case of a matrix with one row only
  frame <- bca(matrix(c(1,1,1), nrow=1), m=1, cnames = c("a","b","c"))
  result <- belplau(frame)
  target <- c(1,1,Inf)
  expect_equal(dim(result), c(1,5))
  ## T5 test hypotheses 
  x5 <- bca(matrix(c(1,1,0,1,1,1), nrow = 2, byrow = TRUE), c(0.8, 0.2), c(1,2,3))
  result <- belplau(x5, h=matrix(c(1,1,0,1,1,1), nrow=2, byrow = TRUE))
  expect_equal(unname(x5$spec[1,1]), unname(result[2,1]))
})

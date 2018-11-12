# Tests "belplau" function
context("Compute belief and plausibility measures")
Library(dst)
test_that("belplau", {
  # T1 x and y must be of class bcaspec. 
  x1 <- list(f=matrix(c(1,0,0,1,1,1),nrow=2, byrow = TRUE), m=c(0.6, 0.4),  cnames = c("a", "b", "c"),  infovarnames = "y1", varnb = 1)
  expect_error(belplau(x1) , "Input argument not of class bcaspec.")
  ##
  # T2 check that the input is a normalized bca
  x2 <- bca(f=matrix(c(0,0,0,0,1,1,1,1,0,1,1,1),nrow=4, byrow = TRUE), m=c(0.2,0.1,0.4, 0.3), cnames =c("a", "b", "c"), infovarnames = "x", varnb = 1)
  expect_error(belplau(x2) ,"Invalid data: Empty set among the focal elements. Normalization necessary. See nzdsr function.")
  ##
  # T3 test that m_empty is null, if present
  x3 <- bca(f=matrix(c(0,0,0,0,1,1,1,1,0,0,1,0,1,1,1),nrow=5, byrow = TRUE), m=c(0,0.1,0.4,0.1, 0.4), cnames =c("a", "b", "c"), infovarnames = "x", varnb = 1)
  result <- belplau(x3)
  expect_equal(nrow(result), nrow(x3$tt))  #  nb of rows of result must match nb of rows of input table x3$tt
  ##
  # T4 check the case of a matrix with one row only
  frame <- bca(matrix(c(1,1,1), nrow=1), m=1, cnames = c("a","b","c"))
  result <- belplau(frame)
  target <- c(1,1,Inf)
  expect_equal(dim(result), c(1,3))
})
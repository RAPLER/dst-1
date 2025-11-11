# Tests "DSA" function
context("Compute belief and plausibility measures")
library(dst)
test_that("DSA", {
  # T1 x and y must be of class DSMspec 
  x1 <- list(f=matrix(c(1,0,0,1,1,1),nrow=2, byrow = TRUE), m=c(0.6, 0.4),  cnames = c("a", "b", "c"),  varnames = "y1", idvar = 1)
  expect_error(DSA(x1) , "Input argument not of class DSMspec.")
  ##
  # T2 check that the input is a normalized DSM
  x2 <- DSM(tt = matrix(c(0,0,0,0,1,1,1,1,0,1,1,1),nrow=4, byrow = TRUE), m=c(0.2,0.1,0.4, 0.3), cnames =c("a", "b", "c"), varnames = "x", idvar = 1)
  expect_error(DSA(x2) , "Invalid data: Empty set among the focal sets. Normalization necessary. Apply function normalize to your DSM.")
  ##
  # T3 test that m_empty is null, if present
  x3 <- DSM(tt = matrix(c(0,0,0,0,1,1,1,1,0,0,1,0,1,1,1),nrow=5, byrow = TRUE), m=c(0,0.1,0.4,0.1, 0.4), cnames =c("a", "b", "c"), varnames = "x", idvar = 1)
  result <- DSA(x3)
  expect_equal(nrow(result), nrow(x3$tt))  #  nb of rows of result must match nb of rows of input table x3$tt
  ##
  # T4 check the case of a matrix with one row only
  frame <- DSM(matrix(c(1,1,1), nrow=1), m=1, cnames = c("a","b","c"))
  result <- DSA(frame)
  target <- c(1,1,Inf)
  expect_equal(dim(result), c(1,5))
  ## T5 test hypotheses 
  x5 <- DSM(tt = matrix(c(1,1,0,1,1,1), nrow = 2, byrow = TRUE), m = c(0.8, 0.2), cnames = c(1,2,3))
  result <- DSA(x5, h=matrix(c(1,1,0,1,1,1), nrow=2, byrow = TRUE))
  expect_equal(unname(x5$spec[1,1]), unname(result[2,1]))
  
  ## T6 test fzt
  x <- DSM(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, 
                       byrow = TRUE), m = c(0.2,0.5, 0.3), 
           cnames = c("a", "b", "c"), varnames = "x", idvar = 1)
  y <- DSM(tt = matrix(c(1,0,0,1,1,1),nrow = 2, 
                       byrow = TRUE), m = c(0.6, 0.4),  
           cnames = c("a", "b", "c"),  varnames = "y", idvar = 1)
  z <- DSC(x,y)
  # expect_equal(DSA(normalize(z))["a",], DSA(normalize(z), method="fzt")["a",])
  # expect_equal(DSA(normalize(z))["b + c",], DSA(normalize(z), method="fzt")["b + c",])
  # expect_equal(DSA(normalize(z))["a + b",], DSA(normalize(z), method="fzt")["a + b",])
  # expect_equal(DSA(normalize(z))["frame",], DSA(normalize(z), method="fzt")["frame",])
  
  ## T7 test ezt
  #expect_equal(DSA(normalize(z))["a",], DSA(normalize(z), method="ezt")["a",])
  #expect_equal(DSA(normalize(z))["b + c",], DSA(normalize(z), method="ezt")["b + c",])
  #expect_equal(DSA(normalize(z))["a + b",], DSA(normalize(z), method="ezt")["a + b",])
  #expect_equal(DSA(normalize(z))["frame",], DSA(normalize(z), method="ezt")["frame",])
  
  ## T8 test ezt with figure 3
  tt6 <- matrix(c(0,0,0,0,0,0,
                  1,0,0,0,0,0,
                  0,0,0,1,0,0,
                  1,0,0,1,0,0,
                  0,0,1,1,0,1,
                  1,0,1,1,0,1,
                  1,1,1,1,1,1), nrow = 7, byrow = TRUE)
  cnames6 <- c("a","b","c","d","e","f")
  m6 <- c(0.01,0.02,0.03,0.04,0.05,0.06,0.79)
  x6 <- DSM(tt6,m6,cnames=cnames6)
  x6n <- normalize(x6)
  # expect_equal(DSA(x6n, method="fzt")["a",], DSA(x6n, method="ezt")["a",])
  # expect_equal(DSA(x6n, method="fzt")["d",], DSA(x6n, method="ezt")["d",])
  # expect_equal(DSA(x6n, method="fzt")["a + d",], DSA(x6n, method="ezt")["a + d",])
  # expect_equal(DSA(x6n, method="fzt")["c + d + f",], DSA(x6n, method="ezt")["c + d + f",])
  # expect_equal(DSA(x6n, method="fzt")["a + c + d + f",], DSA(x6n, method="ezt")["a + c + d + f",])
  # expect_equal(DSA(x6n, method="fzt")["frame",], DSA(x6n, method="ezt")["frame",])
  
  ## T9 Test ezt-j with figure 9 
  tt9 <- matrix(c(0,0,0,0,0,0,
                  1,0,0,0,1,1,
                  0,0,1,1,1,1,
                  1,0,1,1,1,1,
                  1,1,1,0,1,1,
                  1,1,0,1,1,1,
                  0,1,1,1,1,1,
                  1,1,1,1,1,1), nrow = 8, byrow = TRUE)
  cnames9 <- c("a","b","c","d","e","f")
  m9 <- c(0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.72)
  x9 <- DSM(tt9,m9,cnames=cnames9)
  x9n <- normalize(x9)
  # expect_equal(DSA(x9n, method="fzt")["a + e + f",], DSA(x9n, method="ezt-m")["a + e + f",])
  # expect_equal(DSA(x9n, method="fzt")["c + d + e + f",], DSA(x9n, method="ezt-m")["c + d + e + f",])
  # expect_equal(DSA(x9n, method="fzt")["a + c + d + e + f",], DSA(x9n, method="ezt-m")["a + c + d + e + f",])
  # expect_equal(DSA(x9n, method="fzt")["a + b + c + e + f",], DSA(x9n, method="ezt-m")["a + b + c + e + f",])
  # expect_equal(DSA(x9n, method="fzt")["a + b + d + e + f",], DSA(x9n, method="ezt-m")["a + b + d + e + f",])
  # expect_equal(DSA(x9n, method="fzt")["b + c + d + e + f",], DSA(x9n, method="ezt-m")["b + c + d + e + f",])
  # expect_equal(DSA(x9n, method="fzt")["frame",], DSA(x9n, method="ezt-m")["frame",])
  # 
  # TODO: cross testing
  
})

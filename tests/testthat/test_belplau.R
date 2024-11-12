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
  
  ## T6 test fzt
  x <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, 
                       byrow = TRUE), m = c(0.2,0.5, 0.3), 
           cnames = c("a", "b", "c"), varnames = "x", idvar = 1)
  y <- bca(tt = matrix(c(1,0,0,1,1,1),nrow = 2, 
                       byrow = TRUE), m = c(0.6, 0.4),  
           cnames = c("a", "b", "c"),  varnames = "y", idvar = 1)
  z <- dsrwon(x,y)
  expect_equal(belplau(nzdsr(z))["a",], belplau(nzdsr(z), method="fzt")["a",])
  expect_equal(belplau(nzdsr(z))["b + c",], belplau(nzdsr(z), method="fzt")["b + c",])
  expect_equal(belplau(nzdsr(z))["a + b",], belplau(nzdsr(z), method="fzt")["a + b",])
  expect_equal(belplau(nzdsr(z))["frame",], belplau(nzdsr(z), method="fzt")["frame",])
  
  ## T7 test ezt
  #expect_equal(belplau(nzdsr(z))["a",], belplau(nzdsr(z), method="ezt")["a",])
  #expect_equal(belplau(nzdsr(z))["b + c",], belplau(nzdsr(z), method="ezt")["b + c",])
  #expect_equal(belplau(nzdsr(z))["a + b",], belplau(nzdsr(z), method="ezt")["a + b",])
  #expect_equal(belplau(nzdsr(z))["frame",], belplau(nzdsr(z), method="ezt")["frame",])
  
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
  x6 <- bca(tt6,m6,cnames=cnames6)
  x6n <- nzdsr(x6)
  expect_equal(belplau(x6n, method="fzt")["a",], belplau(x6n, method="ezt")["a",])
  expect_equal(belplau(x6n, method="fzt")["d",], belplau(x6n, method="ezt")["d",])
  expect_equal(belplau(x6n, method="fzt")["a + d",], belplau(x6n, method="ezt")["a + d",])
  expect_equal(belplau(x6n, method="fzt")["c + d + f",], belplau(x6n, method="ezt")["c + d + f",])
  expect_equal(belplau(x6n, method="fzt")["a + c + d + f",], belplau(x6n, method="ezt")["a + c + d + f",])
  expect_equal(belplau(x6n, method="fzt")["frame",], belplau(x6n, method="ezt")["frame",])
  
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
  x9 <- bca(tt9,m9,cnames=cnames9)
  x9n <- nzdsr(x9)
  
  expect_equal(belplau(x9n, method="fzt")["a + e + f",], belplau(x9n, method="ezt-m")["a + e + f",])
  expect_equal(belplau(x9n, method="fzt")["c + d + e + f",], belplau(x9n, method="ezt-m")["c + d + e + f",])
  expect_equal(belplau(x9n, method="fzt")["a + c + d + e + f",], belplau(x9n, method="ezt-m")["a + c + d + e + f",])
  expect_equal(belplau(x9n, method="fzt")["a + b + c + e + f",], belplau(x9n, method="ezt-m")["a + b + c + e + f",])
  expect_equal(belplau(x9n, method="fzt")["a + b + d + e + f",], belplau(x9n, method="ezt-m")["a + b + d + e + f",])
  expect_equal(belplau(x9n, method="fzt")["b + c + d + e + f",], belplau(x9n, method="ezt-m")["b + c + d + e + f",])
  expect_equal(belplau(x9n, method="fzt")["frame",], belplau(x9n, method="ezt-m")["frame",])
  
})

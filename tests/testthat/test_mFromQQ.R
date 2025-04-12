context("Construct a mass vector from qq function.")
library(dst)
test_that("mFromQQ", {
  # Test 1.0.1 Define bca, method = NULL
  x1 <- bca(tt = matrix(c(1,1,0,1,1,1), nrow = 2, 
                        byrow = TRUE), m = c(0.4, 0.6), 
            cnames = c("a", "b", "c"), varnames = "x", idvar = 1, method="fzt")
  y1 <- bca(tt = matrix(c(0,1,1,1,1,1), nrow = 2, 
                        byrow = TRUE), m = c(0.3, 0.7),  
            cnames = c("a", "b", "c"),  varnames = "y", idvar = 2, method="fzt")
  z1 <- dsrwon(x1,y1)
  w1 <- dsrwon(x1,y1,use_qq = TRUE)
  
  # Test 1.0.2 NULL vs "fmt"
  m1 <- mFromQQ(w1$qq,3,cnames=c("a", "b", "c"))
  m2 <- mFromQQ(w1$qq,3,method="fmt",cnames=c("a", "b", "c"))
  expect_equal(m1,m2)
  
  # Test 1.2.1 define bca, method = "ezt"
  x <- bca(tt = matrix(c(1,1,0,1,1,1), nrow = 2, 
                       byrow = TRUE), m = c(0.4, 0.6), method="fzt",
           cnames = c("a", "b", "c"), varnames = "x", idvar = 1)
  y <- bca(tt = matrix(c(0,1,1,1,1,1), nrow = 2, 
                       byrow = TRUE), m = c(0.3, 0.7), method="fzt",  
           cnames = c("a", "b", "c"),  varnames = "y", idvar = 2)
  w <- dsrwon(x,y,use_qq = TRUE)
  
  ttx <- matrix(c(1,1,0,1,1,1), nrow = 2, 
                byrow = TRUE)
  tty <- matrix(c(0,1,1,1,1,1), nrow = 2, 
                byrow = TRUE)
  
  x <- bca(tt = ttx, m = c(0.4, 0.6), method="ezt",
           cnames = c("a", "b", "c"), varnames = "x", idvar = 1)
  y <- bca(tt =  tty, m = c(0.3, 0.7), method="ezt",  
           cnames = c("a", "b", "c"),  varnames = "y", idvar = 2)
  z <- dsrwon(x,y,use_qq = TRUE,method="ezt")
  
  # Test 1.2.2 mFromQQ vs fmt
  m1 <- mFromQQ(w$qq,3,cnames=c("a", "b", "c"))
  m2 <- mFromQQ(w$qq,3,method="fmt",cnames=c("a", "b", "c"))
  expect_equal(m1,m2)

  # Test 1.3.1 fmt vs emt
  m1 <- mFromQQ(w$qq,3,method="fmt",cnames=c("a", "b", "c"))
  m2 <- mFromQQ(w$qq,3,method="emt",cnames=c("a", "b", "c"))
  expect_equal(m1,m2)
  
  # Test 1.4.1 fmt vs emt-m
  m1 <- mFromQQ(w$qq,3,method="fmt",cnames=c("a", "b", "c"))
  m2 <- mFromQQ(w$qq,3,method="emt-m",cnames=c("a", "b", "c"))
  expect_equal(m1,m2)
  
  # Test 1.4.1 with figure 6
  tt6 <- matrix(c(0,0,0,0,0,0,
                  1,0,0,0,0,0,
                  0,0,0,1,0,0,
                  1,0,0,1,0,0,
                  0,0,1,1,0,1,
                  1,0,1,1,0,1,
                  1,1,1,1,1,1), nrow = 7, byrow = TRUE)
  cnames6 <- c("a","b","c","d","e","f")
  colnames(tt6) <- cnames6
  rownames(tt6) <- nameRows(tt6)
  m6 <- c(0.01,0.02,0.03,0.04,0.05,0.06,0.79)
  x6 <- bca(tt6,m6,cnames=cnames6, method = "fzt")
  x6n <- nzdsr(x6)
  
  m1 <- mFromQQ(x6$qq,6,method="emt",cnames=cnames6)
  m2 <- mFromQQ(x6$qq,6,method="fmt",cnames=cnames6)
  expect_equal(m1,m2)
  
  # Test 1.4.2 with figure 8
  tt8 <- matrix(c(0,0,0,0,0,0,
                  1,0,0,0,0,0,
                  0,1,0,0,0,0,
                  1,1,0,0,0,0,
                  0,0,1,0,0,0,
                  0,0,0,1,0,0,
                  0,1,1,1,0,0,
                  1,1,1,1,1,1), nrow = 8, byrow = TRUE)
  cnames8 <- c("a","b","c","d","e","f")
  colnames(tt8) <- cnames8
  rownames(tt8) <- nameRows(tt8)
  m8 <- c(0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.72)
  x8 <- bca(tt8,m8,cnames=cnames8)
  x8n <- nzdsr(x8)
  
  m1 <- mFromQQ(x8$qq,6,method="emt",cnames=cnames8)
  m2 <- mFromQQ(x8$qq,6,method="fmt",cnames=cnames8)
  expect_equal(m1,m2)
  
  # Test 1.5.1: testing algorithm from figure 8 with the data from figure 6
  m1 <- mFromQQ(x6$qq,6,method="emt",cnames=cnames6)
  m2 <- mFromQQ(x6$qq,6,method="emt-m",cnames=cnames6)
  expect_equal(m1,m2)
  
  # Test 1.5.2: testing algorithm from figure 6 with the data from figure 8
  m1 <- mFromQQ(x8$qq,6,method="emt",cnames=cnames8)
  m2 <- mFromQQ(x8$qq,6,method="emt-m",cnames=cnames8)
  expect_equal(m1,m2)
})

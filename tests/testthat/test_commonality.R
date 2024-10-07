# Tests "commonality" function
context("Compute qq function from tt matrix")
library(dst)
test_that("commonality", {
  # Check combination with qq is the same as combination with tt 
  # by checking commonality function of the bca is same of the combined commonality function
  x <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, 
                       byrow = TRUE), m = c(0.2,0.5, 0.3), 
           cnames = c("a", "b", "c"), varnames = "x", idvar = 1)
  y <- bca(tt = matrix(c(1,0,0,1,1,1),nrow = 2, 
                       byrow = TRUE), m = c(0.6, 0.4),  
           cnames = c("a", "b", "c"),  varnames = "y", idvar = 1)
  z <- dsrwon(x,y)
  w <- dsrwon(x,y,use_qq = TRUE)
  q <- commonality(z$tt,z$spec[,2])
  expect_equal(q(c(1,0,0)),w$qq(c(1,0,0)))
  expect_equal(q(c(0,1,0)),w$qq(c(0,1,0)))
  expect_equal(q(c(0,0,1)),w$qq(c(0,0,1)))
  expect_equal(q(c(1,1,0)),w$qq(c(1,1,0)))
  expect_equal(q(c(1,0,1)),w$qq(c(1,0,1)))
  expect_equal(q(c(0,1,1)),w$qq(c(0,1,1)))
  expect_equal(q(c(1,1,1)),w$qq(c(1,1,1)))
  
  # with Fast Zeta Transform
  q <- commonality(z$tt,z$spec[,2],method="fzt")
  expect_equal(q(c(1,0,0)),w$qq(c(1,0,0)))
  expect_equal(q(c(0,1,0)),w$qq(c(0,1,0)))
  expect_equal(q(c(0,0,1)),w$qq(c(0,0,1)))
  expect_equal(q(c(1,1,0)),w$qq(c(1,1,0)))
  expect_equal(q(c(1,0,1)),w$qq(c(1,0,1)))
  expect_equal(q(c(0,1,1)),w$qq(c(0,1,1)))
  expect_equal(q(c(1,1,1)),w$qq(c(1,1,1)))
  
  # with Efficient Zeta Transform
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
  
  q1 <- commonality(x6n$tt,x6n$spec[,2], method="fzt")
  q2 <- commonality(x6n$tt,x6n$spec[,2], method="ezt")
  
  expect_equal(q1(c(0,0,0,0,0,0)),q2(c(0,0,0,0,0,0)))
  expect_equal(q1(c(1,0,0,0,0,0)),q2(c(1,0,0,0,0,0)))
  expect_equal(q1(c(0,0,0,1,0,0)),q2(c(0,0,0,1,0,0)))
  expect_equal(q1(c(1,0,0,1,0,0)),q2(c(1,0,0,1,0,0)))
  expect_equal(q1(c(0,0,1,1,0,1)),q2(c(0,0,1,1,0,1)))
  expect_equal(q1(c(1,0,1,1,0,1)),q2(c(1,0,1,1,0,1)))
  expect_equal(q1(c(1,1,1,1,1,1)),q2(c(1,1,1,1,1,1)))
  
  # with EZT
  x61 <- bca(tt6, m6, cnames=cnames6, method="fzt")
  x62 <- bca(tt6, m6, cnames=cnames6, method="ezt")
  
  expect_equal(x61$qq(c(0,0,0,0,0,0)),x62$qq(c(0,0,0,0,0,0)))
  expect_equal(x61$qq(c(1,0,0,0,0,0)),x62$qq(c(1,0,0,0,0,0)))
  expect_equal(x61$qq(c(0,0,0,1,0,0)),x62$qq(c(0,0,0,1,0,0)))
  expect_equal(x61$qq(c(1,0,0,1,0,0)),x62$qq(c(1,0,0,1,0,0)))
  expect_equal(x61$qq(c(0,0,1,1,0,1)),x62$qq(c(0,0,1,1,0,1)))
  expect_equal(x61$qq(c(1,0,1,1,0,1)),x62$qq(c(1,0,1,1,0,1)))
  expect_equal(x61$qq(c(1,1,1,1,1,1)),x62$qq(c(1,1,1,1,1,1)))
  
  
  # with EZT
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
  
  x <- bca(tt = ttx, m = c(0.4, 0.6), W2c = tty, method="ezt",
           cnames = c("a", "b", "c"), varnames = "x", idvar = 1)
  y <- bca(tt =  tty, m = c(0.3, 0.7), W2c = ttx, method="ezt",  
           cnames = c("a", "b", "c"),  varnames = "y", idvar = 2)
  z <- dsrwon(x,y,use_qq = TRUE)
  
  # expect_equal(w$qq(c(0,0,0)),z$qq(c(0,0,0)))
  # expect_equal(w$qq(c(1,0,0)),z$qq(c(1,0,0)))
  expect_equal(w$qq(c(0,1,0)),z$qq(c(0,1,0)))
  # expect_equal(w$qq(c(0,0,1)),z$qq(c(0,0,1)))
  expect_equal(w$qq(c(1,1,0)),z$qq(c(1,1,0)))
  # expect_equal(w$qq(c(1,0,1)),z$qq(c(1,0,1)))
  expect_equal(w$qq(c(0,1,1)),z$qq(c(0,1,1)))
  expect_equal(w$qq(c(1,1,1)),z$qq(c(1,1,1)))
  
})

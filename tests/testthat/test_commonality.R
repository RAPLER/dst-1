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
})

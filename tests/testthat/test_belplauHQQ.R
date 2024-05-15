# Test belplauHQQ
context("Compute belplau from qq function")
library(dst)
test_that("belplauHQQ", {
  # Check computation with qq is the same as tt 
  # by performing belplau calculation on the qq function
  x <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, 
                       byrow = TRUE), m = c(0.2,0.5, 0.3), 
           cnames = c("a", "b", "c"), varnames = "x", idvar = 1)
  y <- bca(tt = matrix(c(1,0,0,1,1,1),nrow = 2, 
                       byrow = TRUE), m = c(0.6, 0.4),  
           cnames = c("a", "b", "c"),  varnames = "y", idvar = 1)
  z <- dsrwon(x, y)
  w <- dsrwon(x, y, use_qq = TRUE)
  expect_equal(belplauH(z$spec[,2], z$tt, matrix(c(0,0,0), nrow = 1)), belplauHQQ(w$qq, matrix(c(0,0,0), nrow = 1)))
  expect_equal(belplauH(z$spec[,2], z$tt, matrix(c(1,0,0), nrow = 1)), belplauHQQ(w$qq, matrix(c(1,0,0), nrow = 1)))
  expect_equal(belplauH(z$spec[,2], z$tt, matrix(c(0,1,0), nrow = 1)), belplauHQQ(w$qq, matrix(c(0,1,0), nrow = 1)))
  expect_equal(belplauH(z$spec[,2], z$tt, matrix(c(0,0,1), nrow = 1)), belplauHQQ(w$qq, matrix(c(0,0,1), nrow = 1)))
  expect_equal(belplauH(z$spec[,2], z$tt, matrix(c(0,1,1), nrow = 1)), belplauHQQ(w$qq, matrix(c(0,1,1), nrow = 1)))
  expect_equal(belplauH(z$spec[,2], z$tt, matrix(c(1,1,0), nrow = 1)), belplauHQQ(w$qq, matrix(c(1,1,0), nrow = 1)))
  # expect_equal(belplauH(z$spec[,2], z$tt, matrix(c(1,1,1), nrow = 1)), belplauHQQ(w$qq, matrix(c(1,1,1), nrow = 1)))
})

context("Construct a description matrix from qq function.")
library(dst)
test_that("ttmatrixFromQQ", {
  x <- bca(tt = matrix(c(0,1,1,1,1,1),nrow = 2, 
                       byrow = TRUE), m = c(0.3, 0.7), 
           cnames = c("a", "b", "c"), varnames = "x", idvar = 1, method = "ezt-j")
  y <- bca(tt = matrix(c(1,1,0,1,1,1),nrow = 2, 
                       byrow = TRUE), m = c(0.6, 0.4),  
           cnames = c("a", "b", "c"),  varnames = "y", idvar = 1, method = "ezt-j")
  z <- dsrwon(x,y)
  w <- dsrwon(x,y,use_qq = TRUE, method = "emt")
  expect_equal(z$tt,ttmatrixFromQQ(w$qq,as.integer(w$infovar[1,2]),w$valuenames[[1]]))
})
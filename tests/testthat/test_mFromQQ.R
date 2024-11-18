context("Construct a mass vector from qq function.")
library(dst)
test_that("mFromQQ", {
  x <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, 
                       byrow = TRUE), m = c(0.2, 0.5, 0.3), 
           cnames = c("a", "b", "c"), varnames = "x", idvar = 1)
  y <- bca(tt = matrix(c(1,0,0,1,1,1),nrow = 2, 
                       byrow = TRUE), m = c(0.6, 0.4),  
           cnames = c("a", "b", "c"),  varnames = "y", idvar = 1)
  z <- dsrwon(x,y)
  w <- dsrwon(x,y,use_qq = TRUE)
  expect_equal(z$spec[,2],unname(mFromQQ(w$qq,ttmatrixFromQQ(w$qq,as.integer(w$infovar[1,2]),unlist(w$valuenames)))))
})

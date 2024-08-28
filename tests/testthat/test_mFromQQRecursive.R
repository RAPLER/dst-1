context("Construct a mass vector from qq function.")
library(dst)
test_that("mFromQQ", {
  x <- bca(tt = matrix(c(1,1,0,1,1,1),nrow = 2, 
                       byrow = TRUE), m = c(0.4, 0.6), 
           cnames = c("a", "b", "c"), varnames = "x", idvar = 1)
  y <- bca(tt = matrix(c(0,1,1,1,1,1),nrow = 2, 
                       byrow = TRUE), m = c(0.3, 0.7),  
           cnames = c("a", "b", "c"),  varnames = "y", idvar = 2)
  z <- dsrwon(x,y)
  w <- dsrwon(x,y,use_qq = TRUE)
  # test mFromQQ and mFromQQRecursive gives the same result
  # expect_equal(mFromQQ(w$qq,ttmatrixFromQQ(w$qq,as.integer(w$infovar[1,2]),unlist(w$valuenames))),
  # mFromQQRecursive(w$qq,ttmatrixFromTT(list(x$tt,y$tt))))
  # test combination
  w$tt<-ttmatrixFromQQ(w$qq,as.integer(w$infovar[1,2]),unlist(w$valuenames))
  expect_equal(z$spec[,2],mFromQQRecursive(w$qq,w$tt))
})

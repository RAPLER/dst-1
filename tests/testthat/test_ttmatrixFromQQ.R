context("Construct a description matrix from qq function.")
library(dst)
test_that("ttmatrixFromQQ", {
  x <- bca(tt = matrix(c(0,1,1,1,1,1),nrow = 2, 
                       byrow = TRUE), m = c(0.3, 0.7), 
           cnames = c("a", "b", "c"), varnames = "x", idvar = 1, method = "ezt-m")
  y <- bca(tt = matrix(c(1,1,0,1,1,1),nrow = 2, 
                       byrow = TRUE), m = c(0.6, 0.4),  
           cnames = c("a", "b", "c"),  varnames = "y", idvar = 1, method = "ezt-m")
  z <- dsrwon(x,y)
  w <- dsrwon(x,y,use_qq = TRUE,tree_type="multiple",method = "emt-m")
  # test combination with tt and combiantion with qq have the same focal elements
  expect_equal(z$tt,ttmatrixFromQQ(w$qq,as.integer(w$infovar[1,2]),w$valuenames[[1]])[rownames(z$tt),])
  # test sparse option
  expect_equal(z$tt,as.matrix(ttmatrixFromQQ(w$qq,as.integer(w$infovar[1,2]),w$valuenames[[1]],"yes"))[rownames(z$tt),])
})
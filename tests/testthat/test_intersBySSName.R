# Test "intersBySSName" function
context("intersect ssnames from two bcas")
library(dst)
test_that("intersBySSName", {
  y1 <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, 
                        byrow = TRUE), m = c(0.2,0.5, 0.3), 
            cnames = c("a", "b", "c"),  
            varnames = "x", idvar = 1) 
  y2 <- bca(tt = matrix(c(1,0,0,1,1,1),nrow = 2, 
                        byrow = TRUE), m = c(0.6, 0.4),  
            cnames = c("a", "b", "c"),  
            varnames = "x", idvar = 1)
  expect_equal(intersBySSName(y1$ssnames[[1]], y2$ssnames[[2]]), "b c")
})

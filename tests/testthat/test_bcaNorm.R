# Tests "bcaNorm" function
context("Compute norm between two bcas")
library(dst)
test_that("bcaNorm", {
  # evaluate norm axioms: 
  y1 <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, 
                        byrow = TRUE), m = c(0.2,0.5, 0.3), 
            cnames = c("a", "b", "c"),  
            varnames = "x", idvar = 1)
  expect_equal(bcaNorm(y1,y1),0)
  y2 <- bca(tt = matrix(c(1,0,0,1,1,1),nrow = 2, 
                        byrow = TRUE), m = c(0.6, 0.4),  
            cnames = c("a", "b", "c"),  
            varnames = "x", idvar = 1)
  expect_equal(bcaNorm(y1,y2)>=0,TRUE)
  expect_equal(bcaNorm(y1,y2),bcaNorm(y2,y1))
  # evaluate norm after combination
  y1y2<-dsrwon(y1,y2,use_ssnames = TRUE)
  my1<-c(0,0,0.2,0.5,0.3)
  my2<-c(0,0.6,0,0,0.4)
  my1y2<-c(0.12,0.48,0.08,0.2,0.12)
  expect_equal(bcaNorm(y1y2,y1),sum(abs(my1y2-my1)))
  expect_equal(bcaNorm(y1y2,y2),sum(abs(my1y2-my2)))
  expect_equal(bcaNorm(y1y2,y1,p=2),sqrt(sum(abs(my1y2-my1)**2)))
  expect_equal(bcaNorm(y1y2,y2,p=2),sqrt(sum(abs(my1y2-my2)**2)))
  # evaluate norm after combination over sparse matrices
  y1s <- y1
  y2s <- y2
  y1s$tt <- methods::as(y1$tt, "RsparseMatrix")
  y2s$tt <- methods::as(y2$tt, "RsparseMatrix")
  y1y2s <- dsrwon(y1s, y2s, use_ssnames = TRUE)
  expect_equal(bcaNorm(y1y2s,y1y2),0)
  expect_equal(bcaNorm(y1y2s,y1),sum(abs(my1y2-my1)))
  expect_equal(bcaNorm(y1y2s,y2),sum(abs(my1y2-my2)))
})

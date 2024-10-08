context("Construct a mass vector from qq function.")
library(dst)
test_that("mFromQQRecursive", {
  # Test 1.0.1 Define bca, method = NULL
  x1 <- bca(tt = matrix(c(1,1,0,1,1,1), nrow = 2, 
                        byrow = TRUE), m = c(0.4, 0.6), 
            cnames = c("a", "b", "c"), varnames = "x", idvar = 1)
  y1 <- bca(tt = matrix(c(0,1,1,1,1,1), nrow = 2, 
                        byrow = TRUE), m = c(0.3, 0.7),  
            cnames = c("a", "b", "c"),  varnames = "y", idvar = 2)
  z1 <- dsrwon(x1,y1)
  w1 <- dsrwon(x1,y1,use_qq = TRUE)
  
  # Test 1.0.2 mFromQQ and mFromQQRecursive, method="fmt" gives the same result
  expect_equal(mFromQQ(w1$qq,ttmatrixFromQQ(w1$qq,as.integer(w1$infovar[1,2]),unlist(w1$valuenames)))[1],
               mFromQQRecursive(w1$qq,3,method="fmt")[3])
  expect_equal(mFromQQ(w1$qq,ttmatrixFromQQ(w1$qq,as.integer(w1$infovar[1,2]),unlist(w1$valuenames)))[2],
               mFromQQRecursive(w1$qq,3,method="fmt")[4])
  expect_equal(mFromQQ(w1$qq,ttmatrixFromQQ(w1$qq,as.integer(w1$infovar[1,2]),unlist(w1$valuenames)))[3],
               mFromQQRecursive(w1$qq,3,method="fmt")[7])
  expect_equal(mFromQQ(w1$qq,ttmatrixFromQQ(w1$qq,as.integer(w1$infovar[1,2]),unlist(w1$valuenames)))[4],
               mFromQQRecursive(w1$qq,3,method="fmt")[8])
  # test combination
  w1$tt<-ttmatrixFromQQ(w1$qq,as.integer(w1$infovar[1,2]),unlist(w1$valuenames))
  expect_equal(z1$spec[,2][1], mFromQQRecursive(w1$qq,3,method="fmt")[3])
  expect_equal(z1$spec[,2][2], mFromQQRecursive(w1$qq,3,method="fmt")[7])
  expect_equal(z1$spec[,2][3], mFromQQRecursive(w1$qq,3,method="fmt")[4])
  expect_equal(z1$spec[,2][4], mFromQQRecursive(w1$qq,3,method="fmt")[8])
  
  # Test 1.1.1 Define bca, method = "fzt"
  x1 <- bca(tt = matrix(c(1,1,0,1,1,1), nrow = 2, 
                       byrow = TRUE), m = c(0.4, 0.6), method="fzt", 
           cnames = c("a", "b", "c"), varnames = "x", idvar = 1)
  y1 <- bca(tt = matrix(c(0,1,1,1,1,1), nrow = 2, 
                       byrow = TRUE), m = c(0.3, 0.7), method="fzt",  
           cnames = c("a", "b", "c"),  varnames = "y", idvar = 2)
  z1 <- dsrwon(x1,y1)
  w1 <- dsrwon(x1,y1,use_qq = TRUE)
  
  # Test 1.1.2 mFromQQ and mFromQQRecursive, method="fmt" gives the same result
  expect_equal(mFromQQ(w1$qq,ttmatrixFromQQ(w1$qq,as.integer(w1$infovar[1,2]),unlist(w1$valuenames)))[1],
               mFromQQRecursive(w1$qq,3,method="fmt")[3])
  expect_equal(mFromQQ(w1$qq,ttmatrixFromQQ(w1$qq,as.integer(w1$infovar[1,2]),unlist(w1$valuenames)))[2],
               mFromQQRecursive(w1$qq,3,method="fmt")[4])
  expect_equal(mFromQQ(w1$qq,ttmatrixFromQQ(w1$qq,as.integer(w1$infovar[1,2]),unlist(w1$valuenames)))[3],
               mFromQQRecursive(w1$qq,3,method="fmt")[7])
  expect_equal(mFromQQ(w1$qq,ttmatrixFromQQ(w1$qq,as.integer(w1$infovar[1,2]),unlist(w1$valuenames)))[4],
               mFromQQRecursive(w1$qq,3,method="fmt")[8])
  # test combination
  w1$tt<-ttmatrixFromQQ(w1$qq,as.integer(w1$infovar[1,2]),unlist(w1$valuenames))
  expect_equal(z1$spec[,2][1], mFromQQRecursive(w1$qq,3,method="fmt")[3])
  expect_equal(z1$spec[,2][2], mFromQQRecursive(w1$qq,3,method="fmt")[7])
  expect_equal(z1$spec[,2][3], mFromQQRecursive(w1$qq,3,method="fmt")[4])
  expect_equal(z1$spec[,2][4], mFromQQRecursive(w1$qq,3,method="fmt")[8])
  
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
  
  x <- bca(tt = ttx, m = c(0.4, 0.6), W2c = tty, method="ezt",
           cnames = c("a", "b", "c"), varnames = "x", idvar = 1)
  y <- bca(tt =  tty, m = c(0.3, 0.7), W2c = ttx, method="ezt",  
           cnames = c("a", "b", "c"),  varnames = "y", idvar = 2)
  z <- dsrwon(x,y,use_qq = TRUE)
  
  # Test 1.2.2 mFromQQRecursive, method="fmt" and mFromQQRecursive, method="fmt" gives the same result
  expect_equal(mFromQQ(w$qq,ttmatrixFromQQ(w$qq,as.integer(w$infovar[1,2]),unlist(w$valuenames)))[1],
               mFromQQRecursive(w$qq,3,method="fmt")[3])
  expect_equal(mFromQQ(w$qq,ttmatrixFromQQ(w$qq,as.integer(w$infovar[1,2]),unlist(w$valuenames)))[2],
               mFromQQRecursive(w$qq,3,method="fmt")[4])
  expect_equal(mFromQQ(w$qq,ttmatrixFromQQ(w$qq,as.integer(w$infovar[1,2]),unlist(w$valuenames)))[3],
               mFromQQRecursive(w$qq,3,method="fmt")[7])
  expect_equal(mFromQQ(w$qq,ttmatrixFromQQ(w$qq,as.integer(w$infovar[1,2]),unlist(w$valuenames)))[4],
               mFromQQRecursive(w$qq,3,method="fmt")[8])
  # test combination
  w$tt<-ttmatrixFromQQ(w$qq,as.integer(w$infovar[1,2]),unlist(w$valuenames))
  expect_equal(z1$spec[,2][1], mFromQQRecursive(w$qq,3,method="fmt")[3])
  expect_equal(z1$spec[,2][2], mFromQQRecursive(w$qq,3,method="fmt")[7])
  expect_equal(z1$spec[,2][3], mFromQQRecursive(w$qq,3,method="fmt")[4])
  expect_equal(z1$spec[,2][4], mFromQQRecursive(w$qq,3,method="fmt")[8])
  
  # Test 1.3.2 mFromQQRecursive, method="fmt" and mFromQQRecursive, method="emt" gives the same result
  z$tt <- ttmatrixFromQQ(z$qq,as.integer(z$infovar[1,2]),unlist(z$valuenames))
  expect_equal(mFromQQRecursive(w$qq,3,method="fmt")[3],
               unname(mFromQQRecursive(w$qq,3,method="emt",tt=z$tt)[1]))
  expect_equal(mFromQQRecursive(w$qq,3,method="fmt")[4],
               unname(mFromQQRecursive(w$qq,3,method="emt",tt=z$tt)[2]))
  expect_equal(mFromQQRecursive(w$qq,3,method="fmt")[7],
               unname(mFromQQRecursive(w$qq,3,method="emt",tt=z$tt)[3]))
  expect_equal(mFromQQRecursive(w$qq,3,method="fmt")[8],
               unname(mFromQQRecursive(w$qq,3,method="emt",tt=z$tt)[4]))
})

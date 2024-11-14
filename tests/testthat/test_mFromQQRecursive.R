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
  
  # Test 1.0.3 combination
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
  
  # Test 1.1.2 mFromQQ and mFromQQRecursive, method="fmt" give the same result
  expect_equal(mFromQQ(w1$qq,ttmatrixFromQQ(w1$qq,as.integer(w1$infovar[1,2]),unlist(w1$valuenames)))[1],
               mFromQQRecursive(w1$qq,3,method="fmt")[3])
  expect_equal(mFromQQ(w1$qq,ttmatrixFromQQ(w1$qq,as.integer(w1$infovar[1,2]),unlist(w1$valuenames)))[2],
               mFromQQRecursive(w1$qq,3,method="fmt")[4])
  expect_equal(mFromQQ(w1$qq,ttmatrixFromQQ(w1$qq,as.integer(w1$infovar[1,2]),unlist(w1$valuenames)))[3],
               mFromQQRecursive(w1$qq,3,method="fmt")[7])
  expect_equal(mFromQQ(w1$qq,ttmatrixFromQQ(w1$qq,as.integer(w1$infovar[1,2]),unlist(w1$valuenames)))[4],
               mFromQQRecursive(w1$qq,3,method="fmt")[8])
  
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
  
  # Test 1.2.2 mFromQQ vs fmt
  expect_equal(mFromQQ(w$qq,ttmatrixFromQQ(w$qq,as.integer(w$infovar[1,2]),unlist(w$valuenames)))[1],
               mFromQQRecursive(w$qq,3,method="fmt")[3])
  expect_equal(mFromQQ(w$qq,ttmatrixFromQQ(w$qq,as.integer(w$infovar[1,2]),unlist(w$valuenames)))[2],
               mFromQQRecursive(w$qq,3,method="fmt")[4])
  expect_equal(mFromQQ(w$qq,ttmatrixFromQQ(w$qq,as.integer(w$infovar[1,2]),unlist(w$valuenames)))[3],
               mFromQQRecursive(w$qq,3,method="fmt")[7])
  expect_equal(mFromQQ(w$qq,ttmatrixFromQQ(w$qq,as.integer(w$infovar[1,2]),unlist(w$valuenames)))[4],
               mFromQQRecursive(w$qq,3,method="fmt")[8])
  
  # Test 1.3.1 fmt vs emt
  z$tt <- ttmatrixFromQQ(z$qq,as.integer(z$infovar[1,2]),unlist(z$valuenames))
  expect_equal(mFromQQRecursive(w$qq,3,method="fmt")[3],
               unname(mFromQQRecursive(w$qq,3,method="emt",tt=z$tt)[1]))
  expect_equal(mFromQQRecursive(w$qq,3,method="fmt")[4],
               unname(mFromQQRecursive(w$qq,3,method="emt",tt=z$tt)[2]))
  expect_equal(mFromQQRecursive(w$qq,3,method="fmt")[7],
               unname(mFromQQRecursive(w$qq,3,method="emt",tt=z$tt)[3]))
  expect_equal(mFromQQRecursive(w$qq,3,method="fmt")[8],
               unname(mFromQQRecursive(w$qq,3,method="emt",tt=z$tt)[4]))
  
  # Test 1.4.1 fmt vs emt-m
  expect_equal(mFromQQRecursive(w$qq,3,method="fmt")[3],
               unname(mFromQQRecursive(w$qq,3,method="emt-m",tt=z$tt)[1]))
  expect_equal(mFromQQRecursive(w$qq,3,method="fmt")[4],
               unname(mFromQQRecursive(w$qq,3,method="emt-m",tt=z$tt)[2]))
  expect_equal(mFromQQRecursive(w$qq,3,method="fmt")[7],
               unname(mFromQQRecursive(w$qq,3,method="emt-m",tt=z$tt)[3]))
  expect_equal(mFromQQRecursive(w$qq,3,method="fmt")[8],
               unname(mFromQQRecursive(w$qq,3,method="emt-m",tt=z$tt)[4]))
  
  # Test 1.4.1 with figure 6
  tt6 <- matrix(c(0,0,0,0,0,0,
                  1,0,0,0,0,0,
                  0,0,0,1,0,0,
                  1,0,0,1,0,0,
                  0,0,1,1,0,1,
                  1,0,1,1,0,1,
                  1,1,1,1,1,1), nrow = 7, byrow = TRUE)
  cnames6 <- c("a","b","c","d","e","f")
  colnames(tt6) <- cnames6
  rownames(tt6) <- nameRows(tt6)
  m6 <- c(0.01,0.02,0.03,0.04,0.05,0.06,0.79)
  x6 <- bca(tt6,m6,cnames=cnames6)
  x6n <- nzdsr(x6)
  expect_equal(unname(mFromQQRecursive(x6$qq,6,method="emt",tt6)[1]),mFromQQRecursive(x6$qq,6,method="fmt",tt6)[1])
  expect_equal(unname(mFromQQRecursive(x6$qq,6,method="emt",tt6)[2]),mFromQQRecursive(x6$qq,6,method="fmt",tt6)[33])
  expect_equal(unname(mFromQQRecursive(x6$qq,6,method="emt",tt6)[3]),mFromQQRecursive(x6$qq,6,method="fmt",tt6)[5])
  expect_equal(unname(mFromQQRecursive(x6$qq,6,method="emt",tt6)[4]),mFromQQRecursive(x6$qq,6,method="fmt",tt6)[37])
  expect_equal(unname(mFromQQRecursive(x6$qq,6,method="emt",tt6)[5]),mFromQQRecursive(x6$qq,6,method="fmt",tt6)[14])
  expect_equal(unname(mFromQQRecursive(x6$qq,6,method="emt",tt6)[6]),mFromQQRecursive(x6$qq,6,method="fmt",tt6)[46])
  expect_equal(unname(mFromQQRecursive(x6$qq,6,method="emt",tt6)[7]),mFromQQRecursive(x6$qq,6,method="fmt",tt6)[64])
  
  # Test 1.4.2 with figure 8
  tt8 <- matrix(c(0,0,0,0,0,0,
                  1,0,0,0,0,0,
                  0,1,0,0,0,0,
                  1,1,0,0,0,0,
                  0,0,1,0,0,0,
                  0,0,0,1,0,0,
                  0,1,1,1,0,0,
                  1,1,1,1,1,1), nrow = 8, byrow = TRUE)
  cnames8 <- c("a","b","c","d","e","f")
  colnames(tt8) <- cnames8
  rownames(tt8) <- nameRows(tt8)
  m8 <- c(0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.72)
  x8 <- bca(tt8,m8,cnames=cnames8)
  x8n <- nzdsr(x8)
  expect_equal(unname(mFromQQRecursive(x8$qq,6,method="emt-m",tt8)[1]),mFromQQRecursive(x8$qq,6,method="fmt",tt8)[1])
  expect_equal(unname(mFromQQRecursive(x8$qq,6,method="emt-m",tt8)[2]),mFromQQRecursive(x8$qq,6,method="fmt",tt8)[33])
  expect_equal(unname(mFromQQRecursive(x8$qq,6,method="emt-m",tt8)[3]),mFromQQRecursive(x8$qq,6,method="fmt",tt8)[17])
  expect_equal(unname(mFromQQRecursive(x8$qq,6,method="emt-m",tt8)[4]),mFromQQRecursive(x8$qq,6,method="fmt",tt8)[9])
  expect_equal(unname(mFromQQRecursive(x8$qq,6,method="emt-m",tt8)[5]),mFromQQRecursive(x8$qq,6,method="fmt",tt8)[5])
  expect_equal(unname(mFromQQRecursive(x8$qq,6,method="emt-m",tt8)[6]),mFromQQRecursive(x8$qq,6,method="fmt",tt8)[49])
  expect_equal(unname(mFromQQRecursive(x8$qq,6,method="emt-m",tt8)[7]),mFromQQRecursive(x8$qq,6,method="fmt",tt8)[29])
  expect_equal(unname(mFromQQRecursive(x8$qq,6,method="emt-m",tt8)[8]),mFromQQRecursive(x8$qq,6,method="fmt",tt8)[64])
  
  # Test 1.5.1: testing algorithm from figure 8 with the data from figure 6
  # expected: Don't need to modify the domain
  expect_equal(mFromQQRecursive(x6$qq,6,method="emt",tt6),mFromQQRecursive(x6$qq,6,method="emt-m",tt6))
  
  # Test 1.5.2: testing algorithm from figure 6 with the data from figure 8 (failed)
  # expected: compute the join closure
  #mFromQQRecursive(x8$qq,6,method="emt",tt8)
  #mFromQQRecursive(x8$qq,6,method="emt-m",tt8)
})

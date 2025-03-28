# Tests "commonality" function
context("Compute qq function from tt matrix")
library(dst)
test_that("commonality", {
  # Check combination with qq is the same as combination with tt 
  # by checking commonality function of the bca is same of the combined commonality function
  x <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow = 3, 
                       byrow = TRUE), m = c(0.2,0.5, 0.3), 
           cnames = c("a", "b", "c"), varnames = "x", idvar = 1, method="fzt")
  y <- bca(tt = matrix(c(1,0,0,1,1,1),nrow = 2, 
                       byrow = TRUE), m = c(0.6, 0.4), 
           cnames = c("a", "b", "c"),  varnames = "y", idvar = 1, method="fzt")
  z <- dsrwon(x,y)
  w <- dsrwon(x,y,use_qq = TRUE)
  q <- commonality(z$tt,z$spec[,2])
  expect_equal(q,w$qq)
  
  # test agreement between fzt, ezt
  ttxl <- lapply(1:nrow(z$tt), function(i) z$tt[i, ])
  ttyl <- closure(ttxl,TRUE)
  ttzl <- do.call(rbind, ttyl)
  colnames(ttzl) <- c("a", "b", "c")
  rownames(ttzl) <- nameRows(ttzl)
  q <- commonality(ttzl,c(z$spec[,2],0),method="ezt")
  expect_equal(q,w$qq[names(q)])
  
  # test fzt vs ezt
  tt6 <- matrix(c(0,0,0,0,0,0,
                  1,0,0,0,0,0,
                  0,0,0,1,0,0,
                  1,0,0,1,0,0,
                  0,0,1,1,0,1,
                  1,0,1,1,0,1,
                  1,1,1,1,1,1), nrow = 7, byrow = TRUE)
  cnames6 <- c("a","b","c","d","e","f")
  m6 <- c(0.01,0.02,0.03,0.04,0.05,0.06,0.79)
  x6 <- bca(tt6,m6,cnames=cnames6)
  x6n <- nzdsr(x6)
  
  q1 <- commonality(x6n$tt,x6n$spec[,2], method="fzt")
  q2 <- commonality(x6n$tt,x6n$spec[,2], method="ezt")
  
  expect_equal(q1[names(q2)],q2)
  
  # Test fzt vs ezt
  x61 <- bca(tt6, m6, cnames=cnames6, method="fzt")
  x62 <- bca(tt6, m6, cnames=cnames6, method="ezt")
  
  expect_equal(x61$qq[names(x62$qq)],x62$qq)
  
  # Test ezt on combination
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
  
  x <- bca(tt = ttx, m = c(0.4, 0.6), method="ezt",
           cnames = c("a", "b", "c"), varnames = "x", idvar = 1)
  y <- bca(tt =  tty, m = c(0.3, 0.7), method="ezt",  
           cnames = c("a", "b", "c"),  varnames = "y", idvar = 2)
  
  z <- dsrwon(x,y,use_qq = TRUE, method="emt")
  
  expect_equal(w$qq[names(z$qq)],z$qq)
  
  # Test ezt-j with figure 5
  x61 <- bca(tt6, m6, cnames=cnames6, method="fzt")
  x62 <- bca(tt6, m6, cnames=cnames6, method="ezt-m")
  
  expect_equal(x61$qq[names(x62$qq)],x62$qq)
})


# Tests "dsrwon" function
context("Compute unnormalized Dempster's Rule")
library(dst)
test_that("dsrwon", {
  # 
  # T1 x and y must be of class bcaspec. 
  # 
  x1 <- list(a=1:3, b="foo")
  y1 <- bca(tt = matrix(c(1,0,0,1,1,1),nrow=2, byrow = TRUE), m=c(0.6, 0.4),  cnames = c("a", "b", "c"),  varnames = "y1", idvar = 1)
  expect_error(dsrwon(x = x1, y = y1) , "One or more inputs not of class bcaspec.")
  #
  # T2 ncol of x and y must be equal. 
  # 
  x2 <- bca(tt = matrix(c(0,1,1,1,1,1,0,1,1,1,1,1),nrow=3, byrow = TRUE), m=c(0.2,0.5, 0.3), cnames =c("a", "b", "c","d"),  varnames = "x2", idvar = 1)
  expect_error(dsrwon(x = x2, y = y1) , "Nb of elements of frame x and frame y not equal.")
  #
  # T3 Value names of the frames must identical and in the same order.
  # 
  z1 <- bca(tt = matrix(c(1,0,0,1,1,1), ncol=2, byrow=TRUE), m= c(0.99, 0.01, 0), cnames =c("M", "T"), varnames = "Diagnosis1", idvar = 1)
  z2 <- bca(tt = matrix(c(1,0, 0,1, 1,1), ncol=2, byrow=TRUE), m= c(0.01, 0.99, 0), cnames =c("C", "T"), varnames = "Diagnosis2", idvar = 2)
  expect_error(dsrwon(x = z1, y = z2) , "Value names of the two frames differ. Check value names of variables as well as their position.")
  #
  # T4 warning message when evidence is completely contradictory
  # Test removed
  #
  # T5 Check that the sum of masses is <= 1
  #
  zy1 <- bca(tt = matrix(c(1,0,0,1,1,1),nrow=2, byrow = TRUE), m=c(0.6, 0.4),  cnames = c("a", "b", "c"),  varnames = "y1", idvar = 1)
  zx2 <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow=3, byrow = TRUE), m=c(0.2,0.5, 0.3), cnames =c("a", "b", "c"),  varnames = "x2", idvar = 1)
  zx2$spec[,2] <- c(0.3, 0.5, 0.3)
  expect_error(dsrwon(x = zy1, y = zx2) , "Invalid data, sum of masses of one vector, or both, greater than one.")
  #
  # T6 Check that nb of elements of frame in last of ssnames equal parameter infovar[,2]
  #
  zy1 <- bca(tt = matrix(c(1,0,0,1,1,1),nrow=2, byrow = TRUE), m=c(0.6, 0.4),  cnames = c("a", "b", "c"),  varnames = "y1", idvar = 1)
  zx2 <- bca(tt = matrix(c(0,1,1,1,1,0,1,1,1),nrow=3, byrow = TRUE), m=c(0.2,0.5, 0.3), cnames =c("a", "b", "c"),  varnames = "x2", idvar = 1)
  zx2$infovar[,2] <- 2
  expect_error(dsrwon(x = zy1, y = zx2, use_ssnames = TRUE) , "Number of elements of frame differs from infovar parameter.")
  #
  # T7
  # Check if the  column names of the two tt matrices are in the same order
  X0 <- bca(tt = matrix(c(0,1,0,1, rep(1,4)), ncol = 4, byrow = TRUE), m = c(0.6,0.4), cnames = c("a", "b", "c", "d"), idvar = 1, varnames = "X0")
  X1 <- bca(tt = matrix(c(1,1,0,0, rep(1,4)), ncol = 4, byrow = TRUE), m = c(0.2,0.8), cnames = c("c", "a", "b", "d"), idvar = 1, varnames = "X1")
  #
  expect_error(dsrwon(x = X0, y = X1) , "Value names of the two frames differ. Check value names of variables as well as their position.")
  #
  # T8: 
  # Test dsrwon with a generated binary matrix
  
  # Subset data
  n <- 10
  m <- 30
  
  # Sample S
  S <- 3
  set.seed(1)
  e <- sample.int(m,S)
  
  # Sample b
  b0 <- rnorm(S, sd=0.6)
  b <- rep(0,m)
  b[e] <- b0
  
  # Sample X
  m0 <- matrix(0, n, m)
  X <- apply(m0, c(1,2), function(x) sample(c(0,1),1))
  Xb <- X %*% b
  
  # Set outcome
  y <- runif(length(Xb)) < 1/(1 + exp(-Xb))
  
  a <- 0.01
  rsid <- 1:m
  bma <- bca(rbind(if (y[1]>0) X[1,1:m] >= 1 else
    (1-X[1,1:m]) >= 1,rep(1,m)), c(a,1-a),
    cnames=rsid)
  
  for(i in 2:n) {
    print(i)
    start.time <- Sys.time()
    bma_new <- bca(rbind(if (y[i]>0) X[i,1:m] >= 1 else
      (1-X[i,1:m]) >= 1,rep(1,m)), c(a,1-a),
      cnames=rsid)
    bma <- dsrwon(bma,bma_new,use_ssnames = TRUE)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(time.taken)
  }
  
  a <- 0.01
  rsid <- 1:m
  bma1 <- bca(rbind(if (y[1]>0) X[1,1:m] >= 1 else
    (1-X[1,1:m]) >= 1,rep(1,m)), c(a,1-a),
    cnames=rsid, method="ezt-m")
  
  for(i in 2:n) {
    print(i)
    start.time <- Sys.time()
    bma_new <- bca(rbind(if (y[i]>0) X[i,1:m] >= 1 else
      (1-X[i,1:m]) >= 1,rep(1,m)), c(a,1-a),
      cnames=rsid, method="ezt-m")
    bma1 <- dsrwon(bma1,bma_new,use_qq = TRUE,method="emt-m")
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    print(time.taken)
  }
  
  #library(profvis)
  #prof <- profvis(dsrwon(bma1,bma_new,use_qq = TRUE,method="emt-m"))
  #htmlwidgets::saveWidget(prof, "../../prof.html")
  #browseURL("../../prof.html")

  mm <- mFromQQ(bma1$qq,unname(bma1$infovar[,2]),bma1$valuenames[[1]],"emt-m")
  tt <- ttmatrixFromQQ(bma1$qq,unname(bma1$infovar[,2]),bma1$valuenames[[1]])
  
  bma1$tt <- tt
  bma1$spec <- as.matrix(cbind(seq(1,length(m)),mm))
  colnames(bma1$spec) <- c("spec","mass")
  
  sort_order <- order(apply(bma1$tt,1,function(x) decode(rep(2,ncol(bma1$tt)),x)))
  bma1$tt <- bma1$tt[sort_order,]
  bma1$spec <- bma1$spec[sort_order,]
  
  sort_order <- order(apply(bma$tt,1,function(x) decode(rep(2,ncol(bma$tt)),x)))
  bma$tt <- bma$tt[sort_order,]
  bma$spec <- bma$spec[sort_order,]
  
  expect_equal(bma$spec[,2],unname(bma1$spec[,2]))
  
})

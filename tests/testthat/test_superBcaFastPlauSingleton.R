# Tests "superBcaFast" function
context("make a superBca")
library(dst)
test_that("superBcaFastPlauSingleton", {
  a <- 1e-3
  # test T1
  x <- matrix(c(0,1,1,1,1,0), nrow = 2, byrow = TRUE)
  
  y <- rep(1,1)
  
  s1<-superBcaFast(x,y,a,tree_type = "multiple")
  
  # test T2
  x <- matrix(c(1,0,0,1,1,0), nrow = 2, byrow = TRUE)
  
  y <- c(0,1)
  
  s2<-superBcaFast(x,y,a,tree_type = "single")
  
  expect_equal(s1$m, s2$m)
  
  # test T3
  x <- matrix(c(0,1,0,1,0,0), nrow = 2, byrow = TRUE)
  
  y <- rep(1,1)
  
  s3<-superBcaFast(x,y,a,tree_type = "single")
  
  # T4
  # Test dsrwon with a generated binary matrix
  
  # Subset data
  n <- 20
  m <- 100
  
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
  y <- as.integer(runif(length(Xb)) < 1/(1 + exp(-Xb)))
  
  # Parameter
  a <- 1e-3
  
  # Regular combination
  rsid <- 1:m
  
  start.time <- Sys.time()
  bma <- bca(rbind(if (y[1]>0) X[1,1:m] >= 1 else
    (1-X[1,1:m]) >= 1,rep(1,m)), c(a,1-a),
    cnames=rsid)
  
  for(i in 2:n) {
    print(i)
    #print(nzdsr(bma)$con)
    bma_new <- bca(rbind(if (y[i]>0) X[i,1:m] >= 1 else
      (1-X[i,1:m]) >= 1,rep(1,m)), c(a,1-a),
      cnames=rsid)
    bma <- dsrwon(bma,bma_new,use_ssnames = TRUE)
  }
  H <- ttmatrixPartition(ncol(bma$tt), ncol(bma$tt))
  bma <- nzdsr(bma)
  bp <- belplau(bma, h=H)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
  # T4: test superBcaFast single
  start.time <- Sys.time()
  bma1 <- superBcaFast(X,y,a,tree_type="single",dsa=TRUE)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
  colnames(bma1$tt) <- rsid
  names(bma1$m) <- nameRows(bma1$tt) 
  
  expect_equal(bma$con,bma1$con)
  expect_equal(unname(bp),unname(bma1$belplau))
  
  # T5: test superBcaFastPlauSingleton single
  start.time <- Sys.time()
  bma2 <- superBcaFastPlauSingleton(X,y,a,tree_type="single",dsa=TRUE)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
  
  colnames(bma1$tt) <- rsid
  
  expect_equal(unname(bp[,"plau"]),unname(bma2$plau)+bma1$con)
  expect_equal(unname(bma1$belplau[,"plau"]),unname(bma2$plau)+bma1$con)
  
  
})
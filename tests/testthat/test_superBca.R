context("create superBca")
library(dst)
test_that("superBca", {
  a <- 1e-3
  # test T1
  x <- matrix(c(0,1,1,1,1,0), nrow = 2, byrow = TRUE)
  x <- methods::as(x, "RsparseMatrix")
  
  y <- rep(1,1)
  
  s1<-superBca(x,y,a,tree_type = "single")
  
  # test T2
  x <- matrix(c(1,0,0,1,1,0), nrow = 2, byrow = TRUE)
  x <- methods::as(x, "RsparseMatrix")
  
  y <- c(0,1)

  s2<-superBca(x,y,a)
  
  expect_equal(s1$m, s2$m,tree_type = "multiple")
  
  # test T3
  x <- matrix(c(0,1,0,1,0,0), nrow = 2, byrow = TRUE)
  x <- methods::as(x, "RsparseMatrix")
  
  y <- rep(1,1)
  
  s1<-superBca(x,y,a,tree_type = "single")
  
  # T4
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
  
  # Parameter
  a <- 1e-3
  
  # Regular combination
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
  
  # T5: test superBca NULL
  bma0 <- superBca(X,y,a,tree_type=NULL)
  
  colnames(bma0$tt) <- rsid
  names(bma0$m) <- nameRows(bma0$tt)
  
  expect_equal(bma$spec[,2],unname(bma0$m[rownames(bma$tt)]))
  
  # T6: test superBca single
  bma1 <- superBca(X,y,a,tree_type="single")
  
  colnames(bma1$tt) <- rsid
  names(bma1$m) <- nameRows(bma1$tt) # TODO: the number of rows don't match
  
  expect_equal(bma$spec[,2],unname(bma1$m[rownames(bma$tt)]))
  
  # T7: test superBca multiple
  bma2 <- superBca(X,y,a,tree_type="multiple")
  
  colnames(bma2$tt) <- rsid
  names(bma2$m) <- nameRows(bma2$tt) # TODO: the number of rows don't match
  
  expect_equal(bma$spec[,2],unname(bma2$m[rownames(bma$tt)]))
  
})

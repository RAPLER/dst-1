context("create commSparse")
library(dst)
test_that("commSparse", {
  # Subset data
  n <- 33
  m <- 2000
  
  # Sample S
  S <- 1
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
  
  start.time <- Sys.time()
  X <- methods::as(X, "RsparseMatrix")
  x_c <- closureSparse(X,FALSE,TRUE)
  iota <- commSparse(X,x_c,a,TRUE)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  print(time.taken)
})
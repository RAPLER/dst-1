# Tests "superset" function
context("find the smallest superset")
library(dst)
test_that("superset", {
  # Test Fig 12
  x <- matrix(c(1,0,0,
                0,0,1,
                0,1,1,
                1,1,1), nrow = 4, byrow = TRUE, dimnames = list(NULL,c("a","b","c")))
  m <- c(0.1,0.2,0.3,0.4)
  q <- commonality(x,m,"ezt-m")
  tree <- buildTree(x,q)
  w <- as.bit(c(0,1,0))
  expect_equal(superset(tree,w),0.7)
  ww <- as.bit(c(1,1,0))
  expect_equal(superset(tree,ww),0.4)
  
  # an example from dsrwon test
  tt1 <- matrix(c(
    0,0,1,1,0,0,1,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,1,1,1,0,0,
    0,0,1,1,1,0,1,1,0,1,0,1,0,1,1,0,0,1,0,1,0,1,0,1,0,1,1,1,0,0,
    1,0,1,1,0,0,1,1,1,0,0,1,0,0,0,1,1,0,0,0,0,0,1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1
  ), nrow = 4, byrow = TRUE)
  
  rownames(tt1) <- c(
    "3 + 4 + 7 + 8 + 12 + 24 + 26 + 27 + 28",
    "3 + 4 + 5 + 7 + 8 + 10 + 12 + 14 + 15 + 18 + 20 + 22 + 24 + 26 + 27 + 28",
    "1 + 3 + 4 + 7 + 8 + 9 + 12 + 16 + 17 + 23 + 24 + 25 + 26 + 27 + 28 + 29 + 30",
    "frame"
  )
  
  q1 <- c(1.0000, 0.9900, 0.9900, 0.9801)
  names(q1) <- rownames(tt1)
  
  tree1 <- buildTree(tt1,q1)
  
  idx_z <- c(3, 4, 7, 8, 10, 12, 20, 22, 24, 27, 28)
  z <- rep(0,30)
  z[idx_z] <- 1 
  
  ww1 <- superset(tree1,z)
  
  expect_equal(ww1,0.99)
  
  # TODO: add more tests
})
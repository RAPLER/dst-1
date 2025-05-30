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
  tree <- buildTree(x,m)
  w1 <- as.bit(c(0,1,0))
  expect_equal(superset(tree,w1)$q,m[3])
  w2 <- as.bit(c(1,1,0))
  expect_equal(superset(tree,w2)$q,m[4])
  # same
  w3 <- as.bit(c(0,1,1))
  expect_equal(superset(tree,w3)$q,m[3])
  w4 <- as.bit(c(0,0,0))
  expect_equal(superset(tree,w4)$q,m[1])
  
  # Test Fig 12 + b
  x <- matrix(c(1,0,0,
                0,0,1,
                0,1,1,
                1,1,1,
                0,1,0), nrow = 5, byrow = TRUE, dimnames = list(NULL,c("a","b","c")))
  m <- c(0.1,0.2,0.3,0.3,0.1)
  tree <- buildTree(x,m)
  w1 <- as.bit(c(0,1,0))
  expect_equal(superset(tree,w1)$q,m[5])
  w2 <- as.bit(c(1,1,0))
  expect_equal(superset(tree,w2)$q,m[4])
  # same
  w3 <- as.bit(c(0,1,1))
  expect_equal(superset(tree,w3)$q,m[3])
  w4 <- as.bit(c(0,0,0))
  expect_equal(superset(tree,w4)$q,m[1])
  
  # Test Fig 12 + emptyset
  x <- matrix(c(1,0,0,
                0,0,1,
                0,1,1,
                1,1,1,
                0,0,0), nrow = 5, byrow = TRUE, dimnames = list(NULL,c("a","b","c")))
  m <- c(0.1,0.2,0.3,0.3,0.1)
  tree <- buildTree(x,m)
  w1 <- as.bit(c(0,1,0))
  expect_equal(superset(tree,w1)$q,m[3])
  w2 <- as.bit(c(1,1,0))
  expect_equal(superset(tree,w2)$q,m[4])
  # same
  w3 <- as.bit(c(0,1,1))
  expect_equal(superset(tree,w3)$q,m[3])
  w4 <- as.bit(c(0,0,0))
  expect_equal(superset(tree,w4)$q,m[5])
  
  # Test simple support
  x <- matrix(c(0,1,1,
                1,1,0,
                0,1,0,
                1,1,1), nrow = 4, byrow = TRUE, dimnames = list(NULL,c("a","b","c")))
  q <- c(0.1,0.2,0.3,0.4)
  
  tree <- buildTree(x,q)
  w1 <- as.bit(c(0,1,0))
  expect_equal(superset(tree,w1)$q,q[3])
  w2 <- as.bit(c(1,1,0))
  expect_equal(superset(tree,w2)$q,q[2])
  # same
  w3 <- as.bit(c(0,1,1))
  expect_equal(superset(tree,w3)$q,q[1])
  w4 <- as.bit(c(0,0,0))
  expect_equal(superset(tree,w4)$q,q[3])
  w5 <- as.bit(c(1,1,1))
  expect_equal(superset(tree,w5)$q,q[4])
  
  # TODO: add more tests
})

